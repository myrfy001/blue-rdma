import FIFOF :: *;
import ClientServer :: * ;

import UserLogicSettings :: *;

import DataTypes :: *;
import SemiFifo :: *;
import BusConversion :: *;
import AxiStreamTypes :: *;


typedef Bit#(64) XdmaDescBypAddr;
typedef Bit#(28) XdmaDescBypLength;
typedef struct {
    Bool eop;
    Bit#(2) _rsv;
    Bool completed;
    Bool stop;
}XdmaDescBypCtl deriving(Bits);


typedef struct {
    Bit#(1) _rsv;
    Bool running;
    Bool irqPending;
    Bool packetDone;
    Bool descDone;
    Bool descStop;
    Bool descCplt;
    Bool busy;
}XdmaChannelStatus deriving(Bits);

(* always_ready, always_enabled *)
interface XdmaDescriptorBypass;
    (* prefix = "" *)     method Action ready((* port = "ready" *) Bool rdy);
    (* result = "load" *) method Bool   load;
    (* result = "src_addr" *) method XdmaDescBypAddr  srcAddr;
    (* result = "dst_addr" *) method XdmaDescBypAddr  dstAddr;
    (* result = "len" *) method XdmaDescBypLength  len;
    (* result = "ctl" *) method XdmaDescBypCtl  ctl;
    (* prefix = "" *) method Action descDone((* port = "desc_done" *) Bool done) ;
endinterface

interface XdmaChannel#(numeric type dataSz, numeric type userSz);
    interface RawAxiStreamSlave#(dataSz, userSz) rawH2cAxiStream;
    interface RawAxiStreamMaster#(dataSz, userSz) rawC2hAxiStream;
    interface XdmaDescriptorBypass h2cDescByp;
    interface XdmaDescriptorBypass c2hDescByp;
endinterface

interface XdmaWrapper#(numeric type dataSz, numeric type userSz);
    interface DmaReadSrv dmaReadSrv;
    interface DmaWriteSrv dmaWriteSrv;
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
endinterface

module mkXdmaWrapper(XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH));

    FIFOF#(AxiStream#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH)) xdmaH2cStFifo <- mkFIFOF();
    let rawH2cSt <- mkPipeInToRawAxiStreamSlave(convertFifoToPipeIn(xdmaH2cStFifo));

    FIFOF#(AxiStream#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH)) xdmaC2hStFifo <- mkFIFOF();
    let rawC2hSt <- mkPipeOutToRawAxiStreamMaster(convertFifoToPipeOut(xdmaC2hStFifo));

    let dmaReadReqQ <- mkFIFOF;
    let dmaReadRespQ <- mkFIFOF;
    FIFOF#(DmaWriteReq) dmaWriteReqQ <- mkFIFOF;
    let dmaWriteRespQ <- mkFIFOF;

    FIFOF#(DmaReadReq) readReqProcessingQ <- mkFIFOF;
    FIFOF#(DmaWriteReq) writeReqProcessingQ <- mkFIFOF;

    Wire#(Bool) h2cDescBypRdyWire <- mkBypassWire;
    Reg#(Bool) h2cNextBeatIsFirst <- mkReg(True);

    Wire#(Bool) c2hDescBypRdyWire <- mkBypassWire;
    Reg#(Bool) c2hNextBeatIsFirst <- mkReg(True);
    Wire#(Bool) c2hDescBypDoneWire <- mkBypassWire;
    
    Bool h2cDescHandshakeWillSuccess = h2cDescBypRdyWire && dmaReadReqQ.notEmpty;

    rule forwardH2cDesc;
        if (h2cDescHandshakeWillSuccess) begin
            dmaReadReqQ.deq;
            readReqProcessingQ.enq(dmaReadReqQ.first);
        end
    endrule

    rule forawrdH2cData;
        let newData = xdmaH2cStFifo.first;
        let currentProcessingReq = readReqProcessingQ.first;
        xdmaH2cStFifo.deq;
        dmaReadRespQ.enq(DmaReadResp{
            initiator: currentProcessingReq.initiator,
            sqpn: currentProcessingReq.sqpn,
            wrID: currentProcessingReq.wrID,
            isRespErr: False,
            dataStream: DataStream{
                data: unpack(pack(newData.tData)),
                byteEn: newData.tKeep,
                isFirst: h2cNextBeatIsFirst,
                isLast: newData.tLast
            }
        });
        if (newData.tLast) begin
            h2cNextBeatIsFirst <= True;
            readReqProcessingQ.deq;
        end else begin
            h2cNextBeatIsFirst <= False;
        end
    endrule

    Bool c2hDescHandshakeWillSuccess = 
         c2hDescBypRdyWire && 
         dmaWriteReqQ.notEmpty &&
         dmaWriteReqQ.first.dataStream.isFirst && 
         writeReqProcessingQ.notFull && xdmaC2hStFifo.notFull && dmaWriteRespQ.notFull;  // make sure only handshake once.

    rule forwardC2hDescAndData;
        // Invariant: The descriptor count is always less than or equal to data segement count.
        // so only when data queue full it will block desc queue, but not vice versa
        // since the request from user logic combine metadata(descriptor) and data in the same channel, but
        // the xdma has two seperated channel for descriptor and data, we should split it.
        // in fact, the handshake for descriptor channel is done in the following `c2hDescByp` interface, it is done
        // automatically when we move (descriptor+data) into the data channel, controlled by 
        // `c2hDescHandshakeWillSuccess` signal. 
        // In other words, we must make sure that when c2hDescHandshakeWillSuccess is true, this rule must be also fired.

        // make sure we won't lost data on descriptor channel.(in fact, this should always be true when the implicity guard is true)
        if (c2hDescBypRdyWire == True) begin
            dmaWriteReqQ.deq;

            xdmaC2hStFifo.enq(
                AxiStream {
                    tData: unpack(pack(dmaWriteReqQ.first.dataStream.data)),
                    tKeep: dmaWriteReqQ.first.dataStream.byteEn,
                    tUser: ?,
                    tLast: dmaWriteReqQ.first.dataStream.isLast
                }
            );

            if (dmaWriteReqQ.first.dataStream.isFirst) begin
                writeReqProcessingQ.enq(dmaWriteReqQ.first);
            end
        end else begin
            $error("This rule should not be fired when c2hDescBypRdyWire is False\n");
        end
    endrule


    interface dmaReadSrv = toGPServer(dmaReadReqQ, dmaReadRespQ);
    interface dmaWriteSrv = toGPServer(dmaWriteReqQ, dmaWriteRespQ);

    interface XdmaChannel xdmaChannel;

        interface rawH2cAxiStream = rawH2cSt;
        interface rawC2hAxiStream = rawC2hSt;

        interface XdmaDescriptorBypass h2cDescByp;

            method Action ready(Bool rdy);
                h2cDescBypRdyWire <= rdy;
            endmethod

            method Bool load;
                return h2cDescHandshakeWillSuccess;
            endmethod

            method XdmaDescBypAddr  srcAddr;
                return h2cDescHandshakeWillSuccess ? dmaReadReqQ.first.startAddr : ?;
            endmethod

            method XdmaDescBypAddr  dstAddr;
                return 0;
            endmethod

            method XdmaDescBypLength len;
                return h2cDescHandshakeWillSuccess ? extend(dmaReadReqQ.first.len) : ?;
            endmethod

            method XdmaDescBypCtl ctl;
                return XdmaDescBypCtl {
                    eop: True,
                    _rsv: 0,
                    completed: False,
                    stop: False
                };
            endmethod

            method Action descDone(Bool done);
            endmethod
        endinterface

        interface XdmaDescriptorBypass c2hDescByp;

            method Action ready(Bool rdy);
                c2hDescBypRdyWire <= rdy;
            endmethod

            method Bool load;
                return c2hDescHandshakeWillSuccess;
            endmethod

            method XdmaDescBypAddr  srcAddr;
                return 0;
            endmethod

            method XdmaDescBypAddr  dstAddr;
                return c2hDescHandshakeWillSuccess ? dmaWriteReqQ.first.metaData.startAddr : ?;
            endmethod

            method XdmaDescBypLength  len;
                return c2hDescHandshakeWillSuccess ? extend(dmaWriteReqQ.first.metaData.len) : ?;
            endmethod

            method XdmaDescBypCtl  ctl;
                return XdmaDescBypCtl {
                    eop: True,
                    _rsv: 0,
                    completed: False,
                    stop: False
                };
            endmethod

            method Action descDone(Bool done);
                c2hDescBypDoneWire <= done;
                if (!writeReqProcessingQ.notEmpty) begin
                    // $error("This rule should not be fired when writeReqProcessingQ is empty\n");
                end else if (!dmaWriteRespQ.notFull) begin
                    // $error("This rule should not be fired when dmaWriteRespQ is full\n");
                end else begin
                    writeReqProcessingQ.deq;
                    dmaWriteRespQ.enq(DmaWriteResp{
                        initiator: writeReqProcessingQ.first.metaData.initiator,
                        sqpn: writeReqProcessingQ.first.metaData.sqpn,
                        psn: writeReqProcessingQ.first.metaData.psn,
                        isRespErr: False
                    }); 
                end

                
            endmethod
        endinterface

    endinterface
endmodule

interface DmaRouter;
    // interface to connect to XDMA wrapper
    interface DmaReadClt xdmaReadClt;
    interface DmaWriteClt xdmaWriteClt;

    // interface to connect DMA users
    interface DmaReadSrv dmaDataPathReadSrv;
    interface DmaWriteSrv dmaDataPathWriteSrv;
    interface DmaReadSrv dmaCtrlPathReadSrv;
    interface DmaWriteSrv dmaCtrlPathWriteSrv;
endinterface

module mkDmaRouter(DmaRouter);
    FIFOF#(DmaReadReq) xdmaReadReqQ <- mkFIFOF;
    FIFOF#(DmaReadResp) xdmaReadRespQ <- mkFIFOF;
    FIFOF#(DmaWriteReq) xdmaWriteReqQ <- mkFIFOF;
    FIFOF#(DmaWriteResp) xdmaWriteRespQ <- mkFIFOF;

    FIFOF#(DmaReadReq) ctrlPathReadReqQ <- mkFIFOF;
    FIFOF#(DmaReadResp) ctrlPathReadRespQ <- mkFIFOF;
    FIFOF#(DmaWriteReq) ctrlPathWriteReqQ <- mkFIFOF;
    FIFOF#(DmaWriteResp) ctrlPathWriteRespQ <- mkFIFOF;

    FIFOF#(DmaReadReq) dataPathReadReqQ <- mkFIFOF;
    FIFOF#(DmaReadResp) dataPathReadRespQ <- mkFIFOF;
    FIFOF#(DmaWriteReq) dataPathWriteReqQ <- mkFIFOF;
    FIFOF#(DmaWriteResp) dataPathWriteRespQ <- mkFIFOF;

    Reg#(Bool) roundRobinFlag <- mkRegU;

    rule forwardToDmaController;
        roundRobinFlag <= !roundRobinFlag;

        if (roundRobinFlag) begin
            // Read Channel
            if (ctrlPathReadReqQ.notEmpty) begin
                xdmaReadReqQ.enq(ctrlPathReadReqQ.first);
                ctrlPathReadReqQ.deq;
            end else if (dataPathReadReqQ.notEmpty) begin
                xdmaReadReqQ.enq(dataPathReadReqQ.first);
                dataPathReadReqQ.deq;
            end
            // Write Channel
            if (ctrlPathWriteReqQ.notEmpty) begin
                xdmaWriteReqQ.enq(ctrlPathWriteReqQ.first);
                ctrlPathWriteReqQ.deq;
            end else if (dataPathWriteReqQ.notEmpty) begin
                xdmaWriteReqQ.enq(dataPathWriteReqQ.first);
                dataPathWriteReqQ.deq;
            end
        end else begin
            // Read Channel
            if (dataPathReadReqQ.notEmpty) begin
                xdmaReadReqQ.enq(dataPathReadReqQ.first);
                dataPathReadReqQ.deq;
            end else if (ctrlPathReadReqQ.notEmpty) begin
                xdmaReadReqQ.enq(ctrlPathReadReqQ.first);
                ctrlPathReadReqQ.deq;
            end
            // Write Channel
            if (dataPathWriteReqQ.notEmpty) begin
                xdmaWriteReqQ.enq(dataPathWriteReqQ.first);
                dataPathWriteReqQ.deq;
            end else if (ctrlPathWriteReqQ.notEmpty) begin
                xdmaWriteReqQ.enq(ctrlPathWriteReqQ.first);
                ctrlPathWriteReqQ.deq;
            end
        end
    endrule

    rule forwardToDmaClients;
        // Read Channel
        if (xdmaReadRespQ.notEmpty) begin
            xdmaReadRespQ.deq;
            if (xdmaReadRespQ.first.initiator == DMA_SRC_CONTROL_PATH_LOGIC) begin
                ctrlPathReadRespQ.enq(xdmaReadRespQ.first);
            end else begin
                dataPathReadRespQ.enq(xdmaReadRespQ.first);
            end
        end

        // write Channel
        if (xdmaWriteRespQ.notEmpty) begin
            xdmaWriteRespQ.deq;
            if (xdmaWriteRespQ.first.initiator == DMA_SRC_CONTROL_PATH_LOGIC) begin
                ctrlPathWriteRespQ.enq(xdmaWriteRespQ.first);
            end else begin
                dataPathWriteRespQ.enq(xdmaWriteRespQ.first);
            end
        end
    endrule


    interface DmaReadClt xdmaReadClt = toGPClient(xdmaReadReqQ, xdmaReadRespQ);
    interface DmaWriteClt xdmaWriteClt = toGPClient(xdmaWriteReqQ, xdmaWriteRespQ);

    
    interface DmaReadSrv dmaDataPathReadSrv = toGPServer(dataPathReadReqQ, dataPathReadRespQ);
    interface DmaWriteSrv dmaDataPathWriteSrv = toGPServer(dataPathWriteReqQ, dataPathWriteRespQ);
    interface DmaReadSrv dmaCtrlPathReadSrv = toGPServer(ctrlPathReadReqQ, ctrlPathReadRespQ);
    interface DmaWriteSrv dmaCtrlPathWriteSrv = toGPServer(ctrlPathWriteReqQ, ctrlPathWriteRespQ);


endmodule
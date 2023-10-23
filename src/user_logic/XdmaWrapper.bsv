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
endinterface

interface XdmaChannel#(numeric type dataSz, numeric type userSz);
    // interface RawAxiStreamMaster#(dataSz, userSz) rawAxiStreamMaster;
    interface RawAxiStreamSlave#(dataSz, userSz) rawAxiStreamSlave;
    interface XdmaDescriptorBypass h2cDescByp;
    interface XdmaDescriptorBypass c2hDescByp;
    // (* prefix = "", always_ready, always_enabled *) method Action status((* port = "status" *) XdmaChannelStatus stat);
endinterface

interface XdmaWrapper#(numeric type dataSz, numeric type userSz);
    interface DmaReadSrv dmaReadSrv;
    interface DmaWriteSrv dmaWriteSrv;
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
endinterface

(* synthesize *)
module mkXdmaWrapper(XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH));

    FIFOF#(AxiStream#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH)) xdmaH2cStFifo <- mkFIFOF();
    let rawSlave <- mkPipeInToRawAxiStreamSlave(convertFifoToPipeIn(xdmaH2cStFifo));

    let dmaReadReqQ <- mkFIFOF;
    let dmaReadRespQ <- mkFIFOF;
    let dmaWriteReqQ <- mkFIFOF;
    let dmaWriteRespQ <- mkFIFOF;

    FIFOF#(DmaReadReq) readReqProcessingQ <- mkFIFOF;

    Wire#(Bool) h2cDescBypRdyWire <- mkBypassWire;

    Reg#(Bool) h2cNextBeatIsFirst <- mkReg(True);


    rule handleDescBypassPortHandshake;
        if (h2cDescBypRdyWire && dmaReadReqQ.notEmpty) begin
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


    interface dmaReadSrv = toGPServer(dmaReadReqQ, dmaReadRespQ);
    interface dmaWriteSrv = toGPServer(dmaWriteReqQ, dmaWriteRespQ);

    interface XdmaChannel xdmaChannel;

        interface rawAxiStreamSlave = rawSlave;

        interface XdmaDescriptorBypass h2cDescByp;

            Bool descReadHandshakeWillSuccess = h2cDescBypRdyWire && dmaReadReqQ.notEmpty;

            method Action ready(Bool rdy);
                h2cDescBypRdyWire <= rdy;
            endmethod

            method Bool load;
                if (descReadHandshakeWillSuccess) begin
                    return True;
                end else begin
                    return False;
                end
            endmethod

            method XdmaDescBypAddr  srcAddr;
                return descReadHandshakeWillSuccess ? dmaReadReqQ.first.startAddr : ?;
            endmethod

            method XdmaDescBypAddr  dstAddr;
                return 0;
            endmethod

            method XdmaDescBypLength len;
                return descReadHandshakeWillSuccess ? extend(dmaReadReqQ.first.len) : ?;
            endmethod

            method XdmaDescBypCtl ctl;
                return XdmaDescBypCtl {
                    eop: False,
                    _rsv: 0,
                    completed: False,
                    stop: False
                };
            endmethod
        endinterface

        interface XdmaDescriptorBypass c2hDescByp;
            method Action ready(Bool rdy);
            endmethod
            method Bool load;
                return False;
            endmethod

            method XdmaDescBypAddr  srcAddr;
                return 0;
            endmethod

            method XdmaDescBypAddr  dstAddr;
                return ?;
            endmethod

            method XdmaDescBypLength  len;
                return ?;
            endmethod

            method XdmaDescBypCtl  ctl;
                return ?;
            endmethod
        endinterface

        // method Action status(XdmaChannelStatus stat);
                
        // endmethod
    endinterface
endmodule
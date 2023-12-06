import FIFOF :: *;
import ClientServer :: * ;
import GetPut :: *;
import Clocks :: * ;
import Vector :: *;
import BRAM :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;

import DataTypes :: *;
import SemiFifo :: *;
import BusConversion :: *;
import AxiStreamTypes :: *;
import Axi4LiteTypes :: *;
import Headers :: *;
import RegisterBlock :: *;
import Gearbox :: *;
import AlignedFIFOs :: * ;

import PrimUtils :: *;
import Connectable :: * ;
import StmtFSM::*;



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
    interface UserLogicDmaReadWideSrv dmaReadSrv;
    interface UserLogicDmaWriteWideSrv dmaWriteSrv;
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
endinterface

module mkXdmaWrapper(XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH));

    FIFOF#(AxiStream#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH)) xdmaH2cStFifo <- mkFIFOF();
    let rawH2cSt <- mkPipeInToRawAxiStreamSlave(convertFifoToPipeIn(xdmaH2cStFifo));

    FIFOF#(AxiStream#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH)) xdmaC2hStFifo <- mkFIFOF();
    let rawC2hSt <- mkPipeOutToRawAxiStreamMaster(convertFifoToPipeOut(xdmaC2hStFifo));

    let dmaReadReqQ     <- mkFIFOF;
    let dmaReadRespQ    <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hWideReq) dmaWriteReqQ <- mkFIFOF;
    let dmaWriteRespQ   <- mkFIFOF;

    FIFOF#(UserLogicDmaH2cReq) readReqProcessingQ   <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hWideReq) writeReqProcessingQ <- mkFIFOF;

    Wire#(Bool) h2cDescBypRdyWire <- mkBypassWire;
    Reg#(Bool) h2cNextBeatIsFirst <- mkReg(True);

    Wire#(Bool) c2hDescBypRdyWire   <- mkBypassWire;
    Reg#(Bool) c2hNextBeatIsFirst   <- mkReg(True);
    Wire#(Bool) c2hDescBypDoneWire  <- mkBypassWire;
    
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
        dmaReadRespQ.enq(UserLogicDmaH2cWideResp{
            dataStream: DataStreamWide{
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
                return h2cDescHandshakeWillSuccess ? dmaReadReqQ.first.addr : ?;
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
                return c2hDescHandshakeWillSuccess ? dmaWriteReqQ.first.addr : ?;
            endmethod

            method XdmaDescBypLength  len;
                return c2hDescHandshakeWillSuccess ? extend(dmaWriteReqQ.first.len) : ?;
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
                    dmaWriteRespQ.enq(UserLogicDmaC2hResp{}); 
                end
            endmethod
        endinterface

    endinterface
endmodule



interface StreamReqProxy#(type t_in_req, type t_in_resp, type t_out_req, type t_out_resp);
    interface Server#(t_in_req, t_in_resp) inSrv;
    interface Client#(t_out_req, t_out_resp) outClt;
endinterface

module mkStreamReqProxy(
        function Tuple2#(t_out_req, Maybe#(t_custom)) reqTransFn(t_in_req req),
        function Tuple2#(t_in_resp, Bool) respTransFn(t_out_resp resp, t_custom customData),
        StreamReqProxy#(t_in_req, t_in_resp, t_out_req, t_out_resp) ifc
    ) provisos (
        Bits#(t_in_req, sz_in_req),
        Bits#(t_in_resp, sz_in_resp),
        Bits#(t_out_req, sz_out_req),
        Bits#(t_out_resp, sz_out_resp),
        Bits#(t_custom, sz_custom)
    );

    FIFOF#(t_in_req) inReqQ <- mkFIFOF;
    FIFOF#(t_in_resp) inRespQ <- mkFIFOF;
    FIFOF#(t_out_req) outReqQ <- mkFIFOF;
    FIFOF#(t_out_resp) outRespQ <- mkFIFOF;
    FIFOF#(t_custom) customDataQ <- mkFIFOF;

    rule forwardReq;
        inReqQ.deq;
        let inReq = inReqQ.first;
        let {outReq, customData} = reqTransFn(inReq);
        if (customData matches tagged Valid .cdata) begin
            customDataQ.enq(cdata);
        end
        outReqQ.enq(outReq);
    endrule

    rule forwardResp;
        outRespQ.deq;
        let outResp = outRespQ.first;
        let customData = customDataQ.first;
        let {inResp, dropCustomData} = respTransFn(outResp, customData);
        if (dropCustomData) begin
            customDataQ.deq;
        end
        inRespQ.enq(inResp);
    endrule

    interface inSrv = toGPServer(inReqQ, inRespQ);
    interface outClt = toGPClient(outReqQ, outRespQ);

endmodule


interface XdmaAxiLiteBridgeWrapper#(type t_csr_addr, type t_csr_data);
    interface RawAxi4LiteSlave#(SizeOf#(t_csr_addr), TDiv#(SizeOf#(t_csr_data),BYTE_WIDTH)) cntrlAxil;
    // interface Client#(CsrReadRequest#(t_csr_addr), CsrReadResponse#(t_csr_data)) csrReadClt;
    // interface Client#(CsrWriteRequest#(t_csr_addr, t_csr_data), CsrWriteResponse) csrWriteClt; 
endinterface 

module mkXdmaAxiLiteBridgeWrapper(Clock slowClock, Reset slowReset, RegisterBlock#(t_csr_addr, t_csr_data) regBlock, XdmaAxiLiteBridgeWrapper#(t_csr_addr, t_csr_data) ifc) 
    provisos (
        Bits#(t_csr_addr, sz_csr_addr),
        Bits#(t_csr_data, sz_csr_data),
        Mul#(sz_csr_strb, BYTE_WIDTH, sz_csr_data),
        Div#(sz_csr_data, BYTE_WIDTH, sz_csr_strb),
        Div#(TMul#(sz_csr_strb, 8), 8, sz_csr_strb)
    );
    ClockDividerIfc divClk <- mkClockDivider(2);

    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;


    SyncFIFOIfc#(Axi4LiteWrAddr#(sz_csr_addr)) cntrlWrAddrFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteWrData#(sz_csr_strb)) cntrlWrDataFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteWrResp) cntrlWrRespFifo <- mkSyncFIFO(2, fastClock,fastReset, slowClock);
    SyncFIFOIfc#(Axi4LiteRdAddr#(sz_csr_addr)) cntrlRdAddrFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteRdData#(sz_csr_strb)) cntrlRdDataFifo <- mkSyncFIFO(2, fastClock,fastReset, slowClock);

    let cntrlAxilSlave <- mkPipeToRawAxi4LiteSlave(
        convertSyncFifoToPipeIn(cntrlWrAddrFifo),
        convertSyncFifoToPipeIn(cntrlWrDataFifo),
        convertSyncFifoToPipeOut(cntrlWrRespFifo),

        convertSyncFifoToPipeIn(cntrlRdAddrFifo),
        convertSyncFifoToPipeOut(cntrlRdDataFifo),
        clocked_by slowClock,
        reset_by slowReset
    );

    rule handleRead;
        cntrlRdAddrFifo.deq;
        let resp <- regBlock.readReg(
            CsrReadRequest{
                addr: unpack(cntrlRdAddrFifo.first.arAddr)
            });
        cntrlRdDataFifo.enq(Axi4LiteRdData{rData: unpack(pack(resp.data)), rResp: 0});
    endrule

    rule handleWrite;
        cntrlWrAddrFifo.deq;
        cntrlWrDataFifo.deq;
        let _ <- regBlock.writeReg(
            CsrWriteRequest{
                addr: unpack(cntrlWrAddrFifo.first.awAddr),
                data: unpack(cntrlWrDataFifo.first.wData)
            });
        cntrlWrRespFifo.enq(0);
    endrule

    // interface Client csrReadClt;
    //     interface Get request;
    //         method ActionValue#(CsrReadRequest#(t_csr_addr)) get();
    //             cntrlRdAddrFifo.deq;
    //             return CsrReadRequest{
    //                 addr: unpack(cntrlRdAddrFifo.first.arAddr)
    //             };
    //         endmethod
    //     endinterface

    //     interface Put response;
    //         method Action put(data);
    //             cntrlRdDataFifo.enq(Axi4LiteRdData{rData: unpack(pack(data)), rResp: 0});
    //         endmethod
    //     endinterface
    // endinterface

    // interface Client csrWriteClt; 
    //     interface Get request;
    //         method ActionValue#(CsrWriteRequest#(t_csr_addr, t_csr_data)) get();
    //             cntrlWrAddrFifo.deq;
    //             cntrlWrDataFifo.deq;
    //             return CsrWriteRequest{
    //                 addr: unpack(cntrlWrAddrFifo.first.awAddr),
    //                 data: unpack(cntrlWrDataFifo.first.wData)
    //             };
    //         endmethod
    //     endinterface

    //     interface Put response;
    //         method Action put(data);
    //             cntrlWrRespFifo.enq(0);
    //         endmethod
    //     endinterface
    // endinterface

    interface cntrlAxil = cntrlAxilSlave;
    
endmodule




interface BluerdmaDmaProxy;
    interface Server#(DmaReadReq, DmaReadResp) blueSideReadSrv;
    interface Server#(DmaWriteReq, DmaWriteResp) blueSideWriteSrv;
    interface UserLogicDmaReadClt userlogicSideReadClt;
    interface UserLogicDmaWriteClt userlogicSideWriteClt;
endinterface

typedef struct {
    DmaReqSrcType initiator;
    QPN sqpn;
    WorkReqID wrID;
} UserLogicBluerdmaDmaProxyCustomDataH2c deriving(Bits);

typedef struct {
    DmaReqSrcType initiator;
    QPN sqpn;
    PSN psn;
} UserLogicBluerdmaDmaProxyCustomDataC2h deriving(Bits);

module mkBluerdmaDmaProxy(BluerdmaDmaProxy);
    function Tuple2#(UserLogicDmaH2cReq, Maybe#(UserLogicBluerdmaDmaProxyCustomDataH2c)) reqTransFnH2c(DmaReadReq req);
        return tuple2(
            UserLogicDmaH2cReq{
                addr: req.startAddr,
                len: zeroExtend(pack(req.len))
            },
            tagged Valid UserLogicBluerdmaDmaProxyCustomDataH2c {
                initiator: req.initiator,
                sqpn: req.sqpn,
                wrID: req.wrID
            }
        );
    endfunction

    function Tuple2#(DmaReadResp, Bool) respTransFnH2c(UserLogicDmaH2cResp resp, UserLogicBluerdmaDmaProxyCustomDataH2c customData);
        return tuple2(
            DmaReadResp{
                initiator: customData.initiator,
                sqpn: customData.sqpn,
                wrID: customData.wrID,
                isRespErr: False,
                dataStream: resp.dataStream
            },
            resp.dataStream.isLast
        );
    endfunction 


    function Tuple2#(UserLogicDmaC2hReq, Maybe#(UserLogicBluerdmaDmaProxyCustomDataC2h)) reqTransFnC2h(DmaWriteReq req);
        
        return tuple2(
            UserLogicDmaC2hReq{
                addr: req.metaData.startAddr,
                len: zeroExtend(pack(req.metaData.len)),
                dataStream: req.dataStream
            },
            req.dataStream.isFirst ? 
                (tagged Valid UserLogicBluerdmaDmaProxyCustomDataC2h {
                initiator: req.metaData.initiator,
                sqpn: req.metaData.sqpn,
                psn: req.metaData.psn
                }) : tagged Invalid
        );
    endfunction

    function Tuple2#(DmaWriteResp, Bool) respTransFnC2h(UserLogicDmaC2hResp resp, UserLogicBluerdmaDmaProxyCustomDataC2h customData);
        return tuple2(
            DmaWriteResp{
                initiator: customData.initiator,
                sqpn: customData.sqpn,
                psn: customData.psn,
                isRespErr: False
            },
            True
        );
    endfunction 

    StreamReqProxy#(
        DmaReadReq, DmaReadResp, UserLogicDmaH2cReq, UserLogicDmaH2cResp
    ) h2cProxy <- mkStreamReqProxy(reqTransFnH2c, respTransFnH2c);

    StreamReqProxy#(
        DmaWriteReq, DmaWriteResp, UserLogicDmaC2hReq, UserLogicDmaC2hResp
    ) c2hProxy <- mkStreamReqProxy(reqTransFnC2h, respTransFnC2h);


    interface blueSideReadSrv = h2cProxy.inSrv;
    interface blueSideWriteSrv = c2hProxy.inSrv;
    interface userlogicSideReadClt = h2cProxy.outClt;
    interface userlogicSideWriteClt = c2hProxy.outClt;

endmodule


interface XdmaGearbox;
    interface UserLogicDmaReadWideClt h2cStreamClt;
    interface UserLogicDmaWriteWideClt c2hStreamClt;
    interface UserLogicDmaReadSrv h2cStreamSrv;
    interface UserLogicDmaWriteSrv c2hStreamSrv;
endinterface


module mkXdmaGearbox(Clock slowClock, Reset slowReset, XdmaGearbox ifc);
    
    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;
    ClockDividerIfc divClk <- mkClockDivider(2);
    
    let h2cStreamReqQStore <- mkRegStore(fastClock, slowClock);
    let c2hStreamRespQStore <- mkRegStore(slowClock, fastClock);

    AlignedFIFO#(UserLogicDmaH2cReq) h2cStreamReqQ <- mkAlignedFIFO(
        fastClock, fastReset,
        slowClock, slowReset,
        h2cStreamReqQStore,
        divClk.clockReady,
        True
    );

    Gearbox#(XDMA_GEARBOX_WIDE_VECTOR_LEN, XDMA_GEARBOX_NARROW_VECTOR_LEN, Maybe#(UserLogicDmaH2cResp)) h2cRespGearbox <- mkNto1Gearbox(
        slowClock, slowReset,
        fastClock, fastReset
    );


    Gearbox#(XDMA_GEARBOX_NARROW_VECTOR_LEN, XDMA_GEARBOX_WIDE_VECTOR_LEN, Maybe#(UserLogicDmaC2hReq)) c2hReqGearbox <- mk1toNGearbox(
        fastClock, fastReset,    
        slowClock, slowReset
    );

    AlignedFIFO#(UserLogicDmaC2hResp) c2hStreamRespQ <- mkAlignedFIFO(
        slowClock, slowReset,
        fastClock, fastReset,
        c2hStreamRespQStore,
        True,
        divClk.clockReady
    );

    FIFOF#(UserLogicDmaH2cResp) h2cRespQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hReq) c2hReqQ <- mkFIFOF;
    
    Reg#(Bool) isCurrentC2hReqAnEvenBeat <- mkReg(False);

    rule forwardH2cResp;
        // use this rule to filter out Invalid resp.
        h2cRespGearbox.deq;
        $display("forwardH2cResp deq", fshow(h2cRespGearbox.first));
        if (h2cRespGearbox.first[0] matches tagged Valid .resp) begin
            h2cRespQ.enq(resp);
        end
    endrule

    rule forwardC2hReq;
        // use this rule to insert a invalid tail if the tail 256 bits is not used.
        Vector#(XDMA_GEARBOX_NARROW_VECTOR_LEN, Maybe#(UserLogicDmaC2hReq)) out;
        if (isCurrentC2hReqAnEvenBeat) begin
            if ( (!c2hReqQ.notEmpty) || (c2hReqQ.notEmpty && c2hReqQ.first.dataStream.isFirst)) begin
                out[0] = tagged Invalid;
                c2hReqGearbox.enq(out);
                isCurrentC2hReqAnEvenBeat <= !isCurrentC2hReqAnEvenBeat;
            end else begin
                out[0] = tagged Valid c2hReqQ.first;
                c2hReqGearbox.enq(out);
                c2hReqQ.deq;
                isCurrentC2hReqAnEvenBeat <= !isCurrentC2hReqAnEvenBeat;
            end
        end else begin
            out[0] = tagged Valid c2hReqQ.first;
            c2hReqGearbox.enq(out);
            c2hReqQ.deq;
            isCurrentC2hReqAnEvenBeat <= !isCurrentC2hReqAnEvenBeat;
        end
    endrule

    interface UserLogicDmaReadWideClt h2cStreamClt;
        interface Get request;
            method ActionValue#(UserLogicDmaH2cReq) get;
                h2cStreamReqQ.deq;
                return h2cStreamReqQ.first;
            endmethod
        endinterface

        interface Put response;
            method Action put(UserLogicDmaH2cWideResp in);
                ByteEn headPartEn = truncate(in.dataStream.byteEn);
                DATA headPartData = truncate(in.dataStream.data);
                ByteEn tailPartEn = truncateLSB(in.dataStream.byteEn);
                DATA tailPartData = truncateLSB(in.dataStream.data);

                UserLogicDmaH2cResp out0;
                UserLogicDmaH2cResp out1;


                out0.dataStream.byteEn = headPartEn;
                out1.dataStream.byteEn = tailPartEn;
                out0.dataStream.data = headPartData;
                out1.dataStream.data = tailPartData;

                Bool isTailPartValid = !isZeroR(tailPartEn);
                out0.dataStream.isFirst = in.dataStream.isFirst;
                out1.dataStream.isFirst = False;
                if (!isTailPartValid) begin
                    out0.dataStream.isLast = in.dataStream.isLast;
                    out1.dataStream.isLast = False;
                end else begin
                    out0.dataStream.isLast = False;
                    out1.dataStream.isLast = in.dataStream.isLast;
                end

                Vector#(2, Maybe#(UserLogicDmaH2cResp)) outVec;

                outVec[0] = tagged Valid out0;
                outVec[1] = isTailPartValid ? tagged Valid out1 : tagged Invalid;

                h2cRespGearbox.enq(outVec);
            endmethod
        endinterface
    endinterface

    interface UserLogicDmaWriteWideClt c2hStreamClt;
        interface Get request;
            method ActionValue#(UserLogicDmaC2hWideReq) get;
                c2hReqGearbox.deq;
                let headPartMaybe = c2hReqGearbox.first[0];
                let tailPartMaybe = c2hReqGearbox.first[1];
                immAssert(
                    isValid(headPartMaybe),
                    "XdmaGearbox c2h head part valid check err @ mkXdmaGearbox",
                    $format(
                        "expect head part to always be valid"
                    )
                );
                
                UserLogicDmaC2hWideReq out = ?;
    
                let headPart = fromMaybe(?, headPartMaybe);
                out.addr = headPart.addr;
                out.len = headPart.len;
                out.dataStream.isFirst = headPart.dataStream.isFirst;
                if (tailPartMaybe matches tagged Valid .tailPart) begin
                    out.dataStream.data = {tailPart.dataStream.data, headPart.dataStream.data};
                    out.dataStream.isLast = tailPart.dataStream.isLast;
                    out.dataStream.byteEn = {tailPart.dataStream.byteEn, headPart.dataStream.byteEn};
                end else begin
                    out.dataStream.data = {0, headPart.dataStream.data};
                    out.dataStream.isLast = headPart.dataStream.isLast;
                    out.dataStream.byteEn = {0, headPart.dataStream.byteEn};
                end

                return out;
            endmethod
        endinterface

        interface Put response;
            method Action put(UserLogicDmaC2hResp e);
                c2hStreamRespQ.enq(e);
            endmethod
        endinterface
    endinterface

    interface UserLogicDmaReadWideSrv h2cStreamSrv;
        interface Get response;
            method ActionValue#(UserLogicDmaH2cResp) get;
                h2cRespQ.deq;
                return h2cRespQ.first;
            endmethod
        endinterface

        interface Put request;
            method Action put(UserLogicDmaH2cReq e);
                h2cStreamReqQ.enq(e);
            endmethod
        endinterface

    endinterface

    interface UserLogicDmaWriteWideSrv c2hStreamSrv;
        interface Get response;
            method ActionValue#(UserLogicDmaC2hResp) get;
                c2hStreamRespQ.deq;
                return c2hStreamRespQ.first;
            endmethod
        endinterface

        interface Put request;
            method Action put(UserLogicDmaC2hReq e);
                c2hReqQ.enq(e);
            endmethod
        endinterface
    endinterface
endmodule


(* synthesize *)
module mkTbGearbox(Empty);
    ClockDividerIfc divClk <- mkClockDivider(2);
    Reset slowReset <- mkInitialReset(1, clocked_by divClk.slowClock);
    XdmaGearbox dut <- mkXdmaGearbox(divClk.slowClock, slowReset);

    // FIFOF#(UserLogicDmaH2cReq) xdmaH2cReqQ <- mkFIFOF(clocked_by divClk.slowClock, reset_by slowReset);
    // FIFOF#(UserLogicDmaH2cWideResp) xdmaH2cRespQ <- mkFIFOF(clocked_by divClk.slowClock, reset_by slowReset);
    // FIFOF#(UserLogicDmaC2hWideReq) xdmaC2hReqQ <- mkFIFOF(clocked_by divClk.slowClock, reset_by slowReset);
    // FIFOF#(UserLogicDmaC2hResp) xdmaC2hRespQ <- mkFIFOF(clocked_by divClk.slowClock, reset_by slowReset);

    FakeXdma fakeXdma <- mkFakeXdma(clocked_by divClk.slowClock, reset_by slowReset);

    FIFOF#(UserLogicDmaH2cReq) userH2cReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cResp) userH2cRespQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hReq) userC2hReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hResp) userC2hRespQ <- mkFIFOF;


    mkConnection(fakeXdma.xdmaH2cSrv, dut.h2cStreamClt);
    mkConnection(fakeXdma.xdmaC2hSrv, dut.c2hStreamClt);

    mkConnection(toGet(userH2cReqQ), dut.h2cStreamSrv.request);
    mkConnection(toPut(userH2cRespQ), dut.h2cStreamSrv.response);
    mkConnection(toGet(userC2hReqQ), dut.c2hStreamSrv.request);
    mkConnection(toPut(userC2hRespQ), dut.c2hStreamSrv.response);

    Reg#(Bool) startedXdma <- mkReg(False, clocked_by divClk.slowClock, reset_by slowReset);
    Reg#(Bool) startedUser <- mkReg(False);
    Reg#(Bit#(10)) counter <- mkReg(0);

    FSM runUserSideLogic <- mkFSM(
        (seq

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h0,
                len: 2,
                dataStream: DataStream{
                    data: 'h12345678,
                    isFirst:True,
                    isLast: True,
                    byteEn: 'h3
                }
            });

            userC2hRespQ.deq;

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h40,
                len: 'h40,
                dataStream: DataStream{
                    data: 'h1234,
                    isFirst:True,
                    isLast: False,
                    byteEn: -1
                }
            });

            userC2hReqQ.enq(UserLogicDmaC2hReq{
                addr: 'h40,
                len: 'h40,
                dataStream: DataStream{
                    data: 'habcd,
                    isFirst:False,
                    isLast: True,
                    byteEn: -1
                }
            });

            userC2hRespQ.deq;

            delay(200);

            userH2cReqQ.enq(UserLogicDmaH2cReq{
                addr: 'h0,
                len: 129
            });
            userH2cRespQ.deq;
            userH2cRespQ.deq;

        endseq)
    );

    // FSM runXdmaSideLogic <- mkFSM(
    //     (seq
    //         xdmaH2cReqQ.deq;
    //         xdmaH2cRespQ.enq(
    //             UserLogicDmaH2cWideResp{
    //                 dataStream: DataStreamWide {
    //                     data: ?,
    //                     byteEn: ?,
    //                     isFirst: True,
    //                     isLast: True
    //                 }
    //             }
    //         );
    //     endseq),
    //     clocked_by divClk.slowClock, reset_by slowReset
    // );


    // rule doFakeXdma if (!startedXdma);
    //     startedXdma <= True;
    //     runXdmaSideLogic.start;
    // endrule

    rule doTest if (!startedUser && counter > 10);
        startedUser <= True;
        runUserSideLogic.start;
    endrule

    rule doKeepRunning;
        counter <= counter + 1;
    endrule
    
endmodule


interface FakeXdma;
    interface UserLogicDmaReadWideSrv xdmaH2cSrv;
    interface UserLogicDmaWriteWideSrv xdmaC2hSrv;
endinterface



typedef SizeOf#(UserLogicDmaLen)                            FAKE_XDMA_MAX_BURST_WIDTH;          // 1MB
typedef DATA_BUS_WIDE_WIDTH                                 FAKE_XDMA_BEAT_DATA_BIT_WIDTH;      // 512-bit
typedef TDiv#(FAKE_XDMA_BEAT_DATA_BIT_WIDTH, BYTE_WIDTH)    FAKE_XDMA_BEAT_DATA_BYTE_WIDTH;     // 64B



typedef Bit#(TAdd#(TLog#(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH),1)) FakeXdmaBeatByteOffset;
typedef Bit#(TAdd#(TLog#(FAKE_XDMA_BEAT_DATA_BIT_WIDTH),1)) FakeXdmaBeatBitOffset;

typedef TSub#(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH,1) FAKE_XDMA_BEAT_DATA_BYTE_OFFFSET_MASK;


typedef struct {
    Bool isFirst;
    Bool isLast;
    FakeXdmaBeatByteOffset beatValidByteCnt;
} FakeXdmaMemReadBeatExtraInfo deriving(Bits, FShow);

typedef struct {
    FakeXdmaBeatByteOffset leftShiftCnt;
    FakeXdmaBeatByteOffset rightShiftCnt;
} FakeXdmaMemReadStreamExtraInfo deriving(Bits, FShow);

module mkFakeXdma(FakeXdma ifc);
    FIFOF#(UserLogicDmaH2cReq) xdmaH2cReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cWideResp) xdmaH2cRespQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hWideReq) xdmaC2hReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hResp) xdmaC2hRespQ <- mkFIFOF;

    BRAM_Configure cfg = defaultValue;
    cfg.allowWriteResponseBypass = False;
    cfg.memorySize = 1024*1024; // 64 MB, word size is 64B
    // BRAM2PortBE#(ADDR, DATA_WIDE, SizeOf#(ByteEnWide)) hostMem <- mkBRAM2ServerBE(cfg);

    BRAM2PortBE#(Bit#(32), Bit#(512), 64) hostMem <- mkBRAM2ServerBE(cfg);

    Reg#(Bool) currentIsH2cReg <- mkReg(True);
    Reg#(Bool) currentNotFinished <- mkReg(False);
    FIFOF#(Tuple4#(Bool, ADDR, UserLogicDmaLen, DataStreamWide)) unionedReqQ <- mkFIFOF;
    FIFOF#(FakeXdmaMemReadBeatExtraInfo) respBeatInfoQ <- mkFIFOF;
    FIFOF#(FakeXdmaMemReadStreamExtraInfo) respStreamInfoQ <- mkFIFOF;

    Reg#(UserLogicDmaLen) bytesLeftReg <- mkRegU;
    Reg#(ADDR) currentAddrReg <- mkRegU;

    Reg#(Tuple2#(DATA_WIDE, FakeXdmaMemReadBeatExtraInfo)) prevMemReadRespReg <- mkRegU;

    FIFOF#(DATA_WIDE) memReadRespQ <- mkFIFOF;
    mkConnection(memReadRespQ.enq, hostMem.portA.response.get);  // convert get to fifof to use notEmpty

    Integer readRespHandleStateHandleFirst=0;
    Integer readRespHandleStateHandleMiddle=1;
    Integer readRespHandleStateHandleLast=2;
    Integer readRespHandleStateHandleEmptyBeat=2;
    Reg#(Bit#(2)) readRespHandleStateReg <- mkReg(0);


    rule ruleArbitter;
        
        if (currentIsH2cReg) begin
            
            if (xdmaH2cReqQ.notEmpty) begin
                xdmaH2cReqQ.deq;
                let req = xdmaH2cReqQ.first;
                unionedReqQ.enq(tuple4(True, req.addr, req.len, ?));
                if (xdmaC2hReqQ.notEmpty) begin
                    currentIsH2cReg <= !currentIsH2cReg;
                end
            end else begin
                currentIsH2cReg <= !currentIsH2cReg;
            end
        end else begin
            if (xdmaC2hReqQ.notEmpty) begin
                xdmaC2hReqQ.deq;
                let req = xdmaC2hReqQ.first;
                unionedReqQ.enq(tuple4(False, req.addr, req.len, req.dataStream));
                if (req.dataStream.isLast && xdmaH2cReqQ.notEmpty) begin
                    currentIsH2cReg <= !currentIsH2cReg;
                end
            end else begin
                currentIsH2cReg <= !currentIsH2cReg;
            end
        end
    endrule


    rule handleReq;
        let {isH2c, addr, len, stream} = unionedReqQ.first;
        FakeXdmaBeatByteOffset firstBeatSkipOffset = truncate(addr & fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_OFFFSET_MASK)));
        FakeXdmaBeatByteOffset firstBeatValidByteCnt = fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH)) - firstBeatSkipOffset; 
        if (isH2c) begin
            if (currentNotFinished == False) begin
                UserLogicDmaLen byteLeft = len;

                let isLastBeat = byteLeft <= zeroExtend(firstBeatValidByteCnt);
                let curAddr = addr;

                hostMem.portA.request.put(BRAMRequestBE{
                    writeen: 0,
                    responseOnWrite: False,
                    address: truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide))))),
                    datain: ?
                });
                respBeatInfoQ.enq(FakeXdmaMemReadBeatExtraInfo{isFirst: True, isLast: isLastBeat, beatValidByteCnt: isLastBeat ? truncate(byteLeft) : firstBeatValidByteCnt});
                respStreamInfoQ.enq(FakeXdmaMemReadStreamExtraInfo{leftShiftCnt: firstBeatValidByteCnt, rightShiftCnt: firstBeatSkipOffset});
                if (isLastBeat) begin
                    currentNotFinished <= False;
                    unionedReqQ.deq;
                end else begin
                    currentNotFinished <= True;
                    bytesLeftReg <= byteLeft - zeroExtend(firstBeatValidByteCnt);
                    currentAddrReg <= curAddr + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                end
            end else begin
                // For a big request, we have to split it into multi BRAM read requests
                let isLastBeat = bytesLeftReg <= fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                FakeXdmaBeatByteOffset beatValidByteCnt = isLastBeat ? truncate(bytesLeftReg) : fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                let curAddr = currentAddrReg;
                hostMem.portA.request.put(BRAMRequestBE{
                    writeen: 0,
                    responseOnWrite: False,
                    address: truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide))))),
                    datain: ?
                });
                respBeatInfoQ.enq(FakeXdmaMemReadBeatExtraInfo{isFirst: False, isLast: isLastBeat, beatValidByteCnt: beatValidByteCnt});
                if (isLastBeat) begin
                    currentNotFinished <= False;
                    unionedReqQ.deq;
                end else begin
                    currentNotFinished <= True;
                    bytesLeftReg <= bytesLeftReg - zeroExtend(beatValidByteCnt);
                    currentAddrReg <= currentAddrReg + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                end
            end
        end else begin
            
            unionedReqQ.deq;
            let curAddr = currentAddrReg;
            if (stream.isFirst) begin
                currentAddrReg <= addr + fromInteger(valueOf(SizeOf#(ByteEnWide)));
                curAddr = addr;
            end
            if (stream.isLast) begin
                xdmaC2hRespQ.enq(?);
            end
            hostMem.portA.request.put(BRAMRequestBE{
                writeen: stream.byteEn,
                responseOnWrite: False,
                address: unpack(truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide)))))),
                datain: stream.data
            });
        end
    endrule

    rule ruleHandleRespFirst if (readRespHandleStateReg == fromInteger(readRespHandleStateHandleFirst));
        
        let newMemReadResp = memReadRespQ.first;
        memReadRespQ.deq;
        respBeatInfoQ.deq;
        prevMemReadRespReg <= tuple2(newMemReadResp, respBeatInfoQ.first);
        readRespHandleStateReg <= fromInteger(readRespHandleStateHandleMiddle);
    endrule

    rule ruleHandleRespMiddle if (readRespHandleStateReg == fromInteger(readRespHandleStateHandleMiddle));
        let readStreamInfo = respStreamInfoQ.first;
        let {prevMemReadResp, prevBeatInfo} = prevMemReadRespReg;

        FakeXdmaBeatBitOffset leftShiftBit = zeroExtend(readStreamInfo.leftShiftCnt) << 3;
        FakeXdmaBeatBitOffset rightShiftBit = zeroExtend(readStreamInfo.rightShiftCnt) << 3;


        if (prevBeatInfo.isLast) begin

            let outData = (prevMemReadResp >> rightShiftBit);
            let outEn = (
                prevBeatInfo.isFirst ? 
                (1 << prevBeatInfo.beatValidByteCnt) :
                (1 << (prevBeatInfo.beatValidByteCnt - readStreamInfo.rightShiftCnt)
            ) - 1);

            xdmaH2cRespQ.enq(UserLogicDmaH2cWideResp{
                dataStream: DataStreamWide{
                    data: outData,
                    isFirst: prevBeatInfo.isFirst,
                    isLast: True,
                    byteEn: outEn
                }
            });
            respStreamInfoQ.deq;

            if (memReadRespQ.notEmpty) begin
                memReadRespQ.deq;
                respBeatInfoQ.deq;
                prevMemReadRespReg <= tuple2(memReadRespQ.first, respBeatInfoQ.first);
            end else begin
                readRespHandleStateReg <= fromInteger(readRespHandleStateHandleFirst);
            end

        end else begin

            let newBeatInfo = respBeatInfoQ.first;
            let newMemReadResp = memReadRespQ.first;
            
            Bool hasMoreData = readStreamInfo.rightShiftCnt < newBeatInfo.beatValidByteCnt;
            let outData = (prevMemReadResp >> rightShiftBit) | (newMemReadResp << leftShiftBit);
            let outEn = hasMoreData ? -1 : (1 << (readStreamInfo.leftShiftCnt + newBeatInfo.beatValidByteCnt)) - 1;

            xdmaH2cRespQ.enq(UserLogicDmaH2cWideResp{
                dataStream: DataStreamWide{
                    data: outData,
                    isFirst: prevBeatInfo.isFirst,
                    isLast: !hasMoreData,
                    byteEn: outEn
                }
            });

            if (hasMoreData) begin
                // keep current state
            end else begin
                readRespHandleStateReg <= fromInteger(readRespHandleStateHandleFirst);
                respStreamInfoQ.deq;
            end

            memReadRespQ.deq;
            respBeatInfoQ.deq;
            prevMemReadRespReg <= tuple2(newMemReadResp, respBeatInfoQ.first);

        end
    endrule

    interface xdmaH2cSrv = toGPServer(xdmaH2cReqQ, xdmaH2cRespQ);
    interface xdmaC2hSrv = toGPServer(xdmaC2hReqQ, xdmaC2hRespQ);
endmodule
import FIFOF :: *;
import ClientServer :: * ;
import GetPut :: *;
import Clocks :: * ;
import Vector :: *;
import BRAM :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;
import RdmaUtils :: *;

import DataTypes :: *;
import SemiFifo :: *;
import BusConversion :: *;
import AxiStreamTypes :: *;
import Axi4LiteTypes :: *;
import Headers :: *;
import Gearbox :: *;
import AlignedFIFOs :: * ;

import PrimUtils :: *;
import Connectable :: * ;
import StmtFSM::*;
import Randomizable :: * ;
import MockHost :: *;
import Ports :: *;


typedef Bit#(64) XdmaDescBypAddr;
typedef Bit#(28) XdmaDescBypLength;
typedef struct {
    Bool eop;
    Bit#(2) _rsv;
    Bool completed;
    Bool stop;
} XdmaDescBypCtl deriving(Bits);


typedef struct {
    Bit#(1) _rsv;
    Bool running;
    Bool irqPending;
    Bool packetDone;
    Bool descDone;
    Bool descStop;
    Bool descCplt;
    Bool busy;
} XdmaChannelStatus deriving(Bits);

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

(* synthesize *)
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
        end 
        else begin
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
        end 
        else begin
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
                end 
                else if (!dmaWriteRespQ.notFull) begin
                    // $error("This rule should not be fired when dmaWriteRespQ is full\n");
                end 
                else begin
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
        Bits#(t_custom, sz_custom),
        FShow#(t_in_req),FShow#(t_in_resp),FShow#(t_out_req),FShow#(t_out_resp), FShow#(t_custom)
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
        // $display("==========>>,", fshow(outReq), fshow(customData));
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
    interface Client#(CsrReadRequest#(t_csr_addr), CsrReadResponse#(t_csr_data)) csrReadClt;
    interface Client#(CsrWriteRequest#(t_csr_addr, t_csr_data), CsrWriteResponse) csrWriteClt; 
endinterface 

module mkXdmaAxiLiteBridgeWrapper(Clock slowClock, Reset slowReset, XdmaAxiLiteBridgeWrapper#(t_csr_addr, t_csr_data) ifc) 
    provisos (
        Bits#(t_csr_addr, sz_csr_addr),
        Bits#(t_csr_data, sz_csr_data),
        Mul#(sz_csr_strb, BYTE_WIDTH, sz_csr_data),
        Div#(sz_csr_data, BYTE_WIDTH, sz_csr_strb),
        Div#(TMul#(sz_csr_strb, BYTE_WIDTH), BYTE_WIDTH, sz_csr_strb)
    );

    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;


    SyncFIFOIfc#(Axi4LiteWrAddr#(sz_csr_addr)) cntrlWrAddrFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteWrData#(sz_csr_strb)) cntrlWrDataFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteWrResp) cntrlWrRespFifo <- mkSyncFIFO(2, fastClock,fastReset, slowClock);
    SyncFIFOIfc#(Axi4LiteRdAddr#(sz_csr_addr)) cntrlRdAddrFifo <- mkSyncFIFO(2, slowClock,slowReset, fastClock);
    SyncFIFOIfc#(Axi4LiteRdData#(sz_csr_strb)) cntrlRdDataFifo <- mkSyncFIFO(2, fastClock,fastReset, slowClock);

    FIFOF#(CsrWriteRequest#(t_csr_addr, t_csr_data)) writeReqQ <- mkFIFOF;
    FIFOF#(CsrWriteResponse) writeRespQ <- mkFIFOF;
    FIFOF#(CsrReadRequest#(t_csr_addr)) readReqQ <- mkFIFOF;
    FIFOF#(CsrReadResponse#(t_csr_data)) readRespQ <- mkFIFOF;

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

        readReqQ.enq(CsrReadRequest{
            addr: unpack(cntrlRdAddrFifo.first.arAddr)
        });
    endrule

    rule forwardReadResp;
        readRespQ.deq;
        cntrlRdDataFifo.enq(Axi4LiteRdData{rResp: 0, rData: unpack(pack(readRespQ.first.data))});
    endrule

    rule handleWrite;
        cntrlWrAddrFifo.deq;
        cntrlWrDataFifo.deq;
        writeReqQ.enq(CsrWriteRequest{
            addr: unpack(cntrlWrAddrFifo.first.awAddr),
            data: unpack(cntrlWrDataFifo.first.wData)
        });
    endrule

    rule forwardWriteResp;
        writeRespQ.deq;
        cntrlWrRespFifo.enq(0);
    endrule

    interface cntrlAxil = cntrlAxilSlave;
    interface csrWriteClt = toGPClient(writeReqQ, writeRespQ);
    interface csrReadClt = toGPClient(readReqQ, readRespQ);
endmodule


typedef struct {
    DmaReqSrcType initiator;
    QPN sqpn;
} UserLogicBluerdmaDmaProxyCustomDataH2c deriving(Bits, FShow);

typedef struct {
    DmaReqSrcType initiator;
    QPN sqpn;
    PSN psn;
} UserLogicBluerdmaDmaProxyCustomDataC2h deriving(Bits, FShow);


interface BluerdmaDmaProxyForRQ;
    interface Server#(DmaReadReqNew, DmaReadRespNew) blueSideReadSrv;
    interface Server#(DmaWriteReqNew, DmaWriteRespNew) blueSideWriteSrv;
    interface UserLogicDmaReadClt userlogicSideReadClt;
    interface UserLogicDmaWriteClt userlogicSideWriteClt;
endinterface


module mkBluerdmaDmaProxyForRQ(BluerdmaDmaProxyForRQ);

    function Tuple2#(UserLogicDmaH2cReq, Maybe#(UserLogicBluerdmaDmaProxyCustomDataH2c)) reqTransFnH2c(DmaReadReqNew req);
        return tuple2(
            UserLogicDmaH2cReq{
                addr: req.startAddr,
                len: zeroExtend(pack(req.len))
            },
            tagged Valid unpack(0)
        );
    endfunction

    function Tuple2#(DmaReadRespNew, Bool) respTransFnH2c(UserLogicDmaH2cResp resp, UserLogicBluerdmaDmaProxyCustomDataH2c customData);
        return tuple2(
            DmaReadRespNew{
                initiator: ?,
                sqpn: ?,
                isRespErr: False,
                dataStream: reverseStream(resp.dataStream)
            },
            resp.dataStream.isLast
        );
    endfunction 


    function Tuple2#(UserLogicDmaC2hReq, Maybe#(UserLogicBluerdmaDmaProxyCustomDataC2h)) reqTransFnC2h(DmaWriteReqNew req);
        
        return tuple2(
            UserLogicDmaC2hReq{
                addr: req.metaData.startAddr,
                len: zeroExtend(pack(req.metaData.len)),
                dataStream: reverseStream(req.dataStream)
            },
            req.dataStream.isFirst ? tagged Valid unpack(0) : tagged Invalid
        );
    endfunction

    function Tuple2#(DmaWriteRespNew, Bool) respTransFnC2h(UserLogicDmaC2hResp resp, UserLogicBluerdmaDmaProxyCustomDataC2h customData);
        return tuple2(
            DmaWriteRespNew{
                isRespErr: False
            },
            True
        );
    endfunction 

    StreamReqProxy#(
        DmaReadReqNew, DmaReadRespNew, UserLogicDmaH2cReq, UserLogicDmaH2cResp
    ) h2cProxy <- mkStreamReqProxy(reqTransFnH2c, respTransFnH2c);

    StreamReqProxy#(
        DmaWriteReqNew, DmaWriteRespNew, UserLogicDmaC2hReq, UserLogicDmaC2hResp
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

(* synthesize *)
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
            end 
            else begin
                out[0] = tagged Valid c2hReqQ.first;
                c2hReqGearbox.enq(out);
                c2hReqQ.deq;
                isCurrentC2hReqAnEvenBeat <= !isCurrentC2hReqAnEvenBeat;
            end
        end 
        else begin
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
                end 
                else begin
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
                end 
                else begin
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



interface FakeXdma;
    interface AxiStream512PipeIn   axiStreamTxUdp;
    interface Get#(AxiStream512)   axiStreamRxUdp;
    interface UserLogicDmaReadWideSrv xdmaH2cSrv;
    interface UserLogicDmaWriteWideSrv xdmaC2hSrv;
    interface Client#(CsrAddr, CsrData) barReadClt;
    interface Client#(Tuple2#(CsrAddr, CsrData), Bool) barWriteClt;
endinterface



typedef SizeOf#(UserLogicDmaLen)                            FAKE_XDMA_MAX_BURST_WIDTH;          // 1MB
typedef DATA_BUS_WIDE_WIDTH                                 FAKE_XDMA_BEAT_DATA_BIT_WIDTH;      // 512-bit
typedef TDiv#(FAKE_XDMA_BEAT_DATA_BIT_WIDTH, BYTE_WIDTH)    FAKE_XDMA_BEAT_DATA_BYTE_WIDTH;     // 64B



typedef Bit#(TAdd#(TLog#(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH),1)) FakeXdmaBeatByteNum;
typedef Bit#(TAdd#(TLog#(FAKE_XDMA_BEAT_DATA_BIT_WIDTH),1)) FakeXdmaBeatBitNum;

typedef TSub#(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH,1) FAKE_XDMA_BEAT_DATA_BYTE_OFFFSET_MASK;


typedef struct {
    Bool isFirst;
    Bool isLast;
    FakeXdmaBeatByteNum beatValidByteCnt;
} FakeXdmaMemReadBeatExtraInfo deriving(Bits, FShow);

typedef struct {
    FakeXdmaBeatByteNum leftShiftByteCnt;
    FakeXdmaBeatByteNum rightShiftByteCnt;
} FakeXdmaMemReadStreamExtraInfo deriving(Bits, FShow);

typedef struct {
    Bool isFirst;
    Bool isLast;
    // FakeXdmaBeatByteNum beatValidByteCnt;
    ByteEnWide byteEn;
} FakeXdmaMemWriteBeatExtraInfo deriving(Bits, FShow);


function FakeXdmaBeatByteNum calcFragByteNumFromByteEnWide(ByteEnWide fragByteEn);
    FakeXdmaBeatByteNum byteEnBitNum = 0;
    for (
        Integer idx = 0;
        idx < valueOf(DATA_BUS_WIDE_BYTE_WIDTH);
        idx = idx + 1
    ) begin
        if (fragByteEn[idx] == 1) begin
            byteEnBitNum = fromInteger(idx+1);
        end
    end
    return byteEnBitNum;
endfunction

module mkFakeXdma(Integer id, FakeXdma ifc);
    FIFOF#(UserLogicDmaH2cReq) xdmaH2cReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cWideResp) xdmaH2cRespQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hWideReq) xdmaC2hReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaC2hResp) xdmaC2hRespQ <- mkFIFOF;

    BRAM_Configure cfg = defaultValue;
    cfg.allowWriteResponseBypass = False;
    cfg.memorySize = 1024*1024; // 64 MB, word size is 64B
    // BRAM2PortBE#(ADDR, DATA_WIDE, SizeOf#(ByteEnWide)) hostMem <- mkBRAM2ServerBE(cfg);
    MockHost#(Bit#(32), Bit#(512), 64, CsrAddr, CsrData) mockHost <- mkMockHost(cfg);
    let hostMem = mockHost.hostMem;
    

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
    Reg#(Bit#(1)) readRespHandleStateReg <- mkReg(0);

    Reg#(Tuple2#(DATA_WIDE, FakeXdmaMemWriteBeatExtraInfo)) prevMemWriteReqReg <- mkRegU;

    Reg#(Bool) writeReqNeedExtraBeat <- mkReg(False);


    rule ruleArbitter;
        if (currentIsH2cReg) begin
            
            if (xdmaH2cReqQ.notEmpty) begin
                xdmaH2cReqQ.deq;
                let req = xdmaH2cReqQ.first;
                $display("dma %d arbiter receive H2C request: Addr=", id, fshow(req.addr), "Len=", fshow(req.len));
                unionedReqQ.enq(tuple4(True, req.addr, req.len, ?));
                if (xdmaC2hReqQ.notEmpty) begin
                    currentIsH2cReg <= !currentIsH2cReg;
                end
            end 
            else begin
                currentIsH2cReg <= !currentIsH2cReg;
            end
        end 
        else begin
            if (xdmaC2hReqQ.notEmpty) begin
                xdmaC2hReqQ.deq;
                let req = xdmaC2hReqQ.first;
                $display("dma %d arbiter receive C2H request: Addr=", id, fshow(req.addr), "Len=", fshow(req.len), "Data=", fshow(req.dataStream));
                unionedReqQ.enq(tuple4(False, req.addr, req.len, req.dataStream));
                if (req.dataStream.isLast && xdmaH2cReqQ.notEmpty) begin
                    currentIsH2cReg <= !currentIsH2cReg;
                end
            end 
            else begin
                currentIsH2cReg <= !currentIsH2cReg;
            end
        end
    endrule


    rule handleReq if (!writeReqNeedExtraBeat);
        let {isH2c, addr, len, stream} = unionedReqQ.first;
        immAssert(
            len != 0,
            "DMA request len is 0 @ mkFakeXdma",
            $format(
                "request should not be 0 in length, request = ", fshow(unionedReqQ.first), "isH2c=", fshow(isH2c)
            )
        );

        FakeXdmaBeatByteNum addrAlignOffset = truncate(addr & fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_OFFFSET_MASK)));
        FakeXdmaBeatByteNum addrAlignRemainder = fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH)) - addrAlignOffset; 
        if (isH2c) begin
            
            if (currentNotFinished == False) begin
                UserLogicDmaLen byteLeft = len;

                let isLastBeat = byteLeft <= zeroExtend(addrAlignRemainder);
                let curAddr = addr;

                hostMem.portA.request.put(BRAMRequestBE{
                    writeen: 0,
                    responseOnWrite: False,
                    address: truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide))))),
                    datain: ?
                });
                respBeatInfoQ.enq(FakeXdmaMemReadBeatExtraInfo{isFirst: True, isLast: isLastBeat, beatValidByteCnt: isLastBeat ? truncate(byteLeft) : addrAlignRemainder});
                respStreamInfoQ.enq(FakeXdmaMemReadStreamExtraInfo{leftShiftByteCnt: addrAlignRemainder, rightShiftByteCnt: addrAlignOffset});
                if (isLastBeat) begin
                    currentNotFinished <= False;
                    
                    unionedReqQ.deq;
                end 
                else begin
                    currentNotFinished <= True;
                    
                    bytesLeftReg <= byteLeft - zeroExtend(addrAlignRemainder);
                    currentAddrReg <= curAddr + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                end
            end 
            else begin
                // For a big request, we have to split it into multi BRAM read requests
                let isLastBeat = bytesLeftReg <= fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                FakeXdmaBeatByteNum beatValidByteCnt = isLastBeat ? truncate(bytesLeftReg) : fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
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
                end 
                else begin
                    currentNotFinished <= True;
                    
                    bytesLeftReg <= bytesLeftReg - zeroExtend(beatValidByteCnt);
                    currentAddrReg <= currentAddrReg + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                end
            end
        end 
        else begin
            
            unionedReqQ.deq;
            FakeXdmaBeatByteNum curReqValidByteCnt = calcFragByteNumFromByteEnWide(stream.byteEn);
            FakeXdmaBeatByteNum leftShiftByte = zeroExtend(addrAlignOffset);
            FakeXdmaBeatByteNum rightShiftByte = zeroExtend(addrAlignRemainder);
            FakeXdmaBeatBitNum leftShiftBit = zeroExtend(leftShiftByte) << 3;
            FakeXdmaBeatBitNum rightShiftBit = zeroExtend(rightShiftByte) << 3;

            if (stream.isFirst) begin
                let outData = stream.data << leftShiftBit;
                let byteEn = stream.byteEn << leftShiftByte;
                let curAddr = addr;
                hostMem.portA.request.put(BRAMRequestBE{
                    writeen: byteEn,
                    responseOnWrite: False,
                    address: unpack(truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide)))))),
                    datain: outData
                });

                currentAddrReg <= curAddr + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                prevMemWriteReqReg <= tuple2(stream.data, FakeXdmaMemWriteBeatExtraInfo{isFirst: stream.isFirst, isLast: stream.isLast, byteEn: stream.byteEn});
            end 
            else begin
                let {prevReqData, prevReqBeatInfo} = prevMemWriteReqReg;
                let outData = stream.data << leftShiftBit | prevReqData >> rightShiftBit;
                let byteEn = stream.byteEn << leftShiftByte | prevReqBeatInfo.byteEn >> rightShiftByte;

                let curAddr = currentAddrReg;
                
                hostMem.portA.request.put(BRAMRequestBE{
                    writeen: byteEn,
                    responseOnWrite: False,
                    address: unpack(truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide)))))),
                    datain: outData
                });

                currentAddrReg <= currentAddrReg + fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH));
                prevMemWriteReqReg <= tuple2(stream.data, FakeXdmaMemWriteBeatExtraInfo{isFirst: stream.isFirst, isLast: stream.isLast, byteEn: stream.byteEn});
            end

            
            let hasMoreData = unpack(pack({1'b0, stream.byteEn})[rightShiftByte]);
            if (stream.isLast) begin
                if (hasMoreData) begin
                    writeReqNeedExtraBeat <= True;
                end 
                else begin
                    xdmaC2hRespQ.enq(UserLogicDmaC2hResp{});
                end
            end
        end
    endrule

    rule handleReqExtraBeat if (writeReqNeedExtraBeat);
        writeReqNeedExtraBeat <= False;
        let addr = currentAddrReg;
        

        let {prevReqData, prevReqBeatInfo} = prevMemWriteReqReg;

        FakeXdmaBeatByteNum addrAlignOffset = truncate(addr & fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_OFFFSET_MASK)));
        FakeXdmaBeatByteNum addrAlignRemainder = fromInteger(valueOf(FAKE_XDMA_BEAT_DATA_BYTE_WIDTH)) - addrAlignOffset; 
        FakeXdmaBeatByteNum rightShiftByte = zeroExtend(addrAlignRemainder);
        FakeXdmaBeatBitNum rightShiftBit = zeroExtend(rightShiftByte) << 3;
        
        let outData = prevReqData >> rightShiftBit;
        let byteEn = prevReqBeatInfo.byteEn >> rightShiftByte;
        let curAddr = currentAddrReg;
        hostMem.portA.request.put(BRAMRequestBE{
            writeen: byteEn,
            responseOnWrite: False,
            address: unpack(truncate(curAddr >> fromInteger(valueOf(TLog#(SizeOf#(ByteEnWide)))))),
            datain: outData
        });
        xdmaC2hRespQ.enq(UserLogicDmaC2hResp{});
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

        FakeXdmaBeatBitNum leftShiftBit = zeroExtend(readStreamInfo.leftShiftByteCnt) << 3;
        FakeXdmaBeatBitNum rightShiftBit = zeroExtend(readStreamInfo.rightShiftByteCnt) << 3;

        
        if (prevBeatInfo.isLast) begin

            let outData = (prevMemReadResp >> rightShiftBit);
            let outEn = (
                prevBeatInfo.isFirst ? 
                (1 << prevBeatInfo.beatValidByteCnt) -1 :
                (1 << (prevBeatInfo.beatValidByteCnt - readStreamInfo.rightShiftByteCnt)
            ) - 1);

            let resp = UserLogicDmaH2cWideResp{
                dataStream: DataStreamWide{
                    data: outData,
                    isFirst: prevBeatInfo.isFirst,
                    isLast: True,
                    byteEn: outEn
                }
            };
            xdmaH2cRespQ.enq(resp);
            respStreamInfoQ.deq;

            if (memReadRespQ.notEmpty) begin
                memReadRespQ.deq;
                respBeatInfoQ.deq;
                
                prevMemReadRespReg <= tuple2(memReadRespQ.first, respBeatInfoQ.first);
            end 
            else begin
                readRespHandleStateReg <= fromInteger(readRespHandleStateHandleFirst);
            end

        end 
        else begin
            
            let newBeatInfo = respBeatInfoQ.first;
            let newMemReadResp = memReadRespQ.first;
            
            Bool hasMoreData = readStreamInfo.rightShiftByteCnt < newBeatInfo.beatValidByteCnt;
            let outData = (prevMemReadResp >> rightShiftBit) | (newMemReadResp << leftShiftBit);
            let outEn = hasMoreData ? -1 : (1 << (readStreamInfo.leftShiftByteCnt + newBeatInfo.beatValidByteCnt)) - 1;
            
            let resp = UserLogicDmaH2cWideResp{
                dataStream: DataStreamWide{
                    data: outData,
                    isFirst: prevBeatInfo.isFirst,
                    isLast: !hasMoreData,
                    byteEn: outEn
                }
            };
            xdmaH2cRespQ.enq(resp);

            if (hasMoreData) begin
                // keep current state
            end 
            else begin
                readRespHandleStateReg <= fromInteger(readRespHandleStateHandleFirst);
                respStreamInfoQ.deq;
            end

            memReadRespQ.deq;
            respBeatInfoQ.deq;
            
            prevMemReadRespReg <= tuple2(newMemReadResp, respBeatInfoQ.first);

        end
    endrule

    interface axiStreamTxUdp = mockHost.axiStreamTxUdp;
    interface axiStreamRxUdp = mockHost.axiStreamRxUdp;
    interface xdmaH2cSrv = toGPServer(xdmaH2cReqQ, xdmaH2cRespQ);
    interface xdmaC2hSrv = toGPServer(xdmaC2hReqQ, xdmaC2hRespQ);
    interface barWriteClt = mockHost.barWriteClt;
    interface barReadClt = mockHost.barReadClt;
endmodule





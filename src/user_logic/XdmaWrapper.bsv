import FIFOF :: *;
import ClientServer :: * ;
import GetPut :: *;
import Clocks :: * ;
import Vector :: *;

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

module mkXdmaAxiLiteBridgeWrapper(ClockDividerIfc divClk, Reset slowReset, RegisterBlock#(t_csr_addr, t_csr_data) regBlock, XdmaAxiLiteBridgeWrapper#(t_csr_addr, t_csr_data) ifc) 
    provisos (
        Bits#(t_csr_addr, sz_csr_addr),
        Bits#(t_csr_data, sz_csr_data),
        Mul#(sz_csr_strb, BYTE_WIDTH, sz_csr_data),
        Div#(sz_csr_data, BYTE_WIDTH, sz_csr_strb),
        Div#(TMul#(sz_csr_strb, 8), 8, sz_csr_strb)
    );

    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;
    Clock slowClock = divClk.slowClock;

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


module mkXdmaGearbox(ClockDividerIfc divClk, Reset slowReset, XdmaGearbox ifc);
    
    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;
    Clock slowClock = divClk.slowClock;
    
    let h2cStreamReqQStore <- mkRegStore(fastClock, slowClock);
    let c2hStreamRespQStore <- mkRegStore(slowClock, fastClock);

    AlignedFIFO#(UserLogicDmaH2cReq) h2cStreamReqQ <- mkAlignedFIFO(
        divClk.fastClock, fastReset,
        divClk.slowClock, slowReset,
        h2cStreamReqQStore,
        divClk.clockReady,
        True
    );

    Gearbox#(XDMA_GEARBOX_WIDE_VECTOR_LEN, XDMA_GEARBOX_NARROW_VECTOR_LEN, Maybe#(UserLogicDmaH2cResp)) h2cRespGearbox <- mkNto1Gearbox(
        divClk.slowClock, slowReset,
        divClk.fastClock, fastReset
    );


    Gearbox#(XDMA_GEARBOX_NARROW_VECTOR_LEN, XDMA_GEARBOX_WIDE_VECTOR_LEN, Maybe#(UserLogicDmaC2hReq)) c2hReqGearbox <- mk1toNGearbox(
        divClk.fastClock, fastReset,    
        divClk.slowClock, slowReset
    );

    AlignedFIFO#(UserLogicDmaC2hResp) c2hStreamRespQ <- mkAlignedFIFO(
        divClk.slowClock, slowReset,
        divClk.fastClock, fastReset,
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

                Bool isTailPartValid = isZeroR(tailPartEn);
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
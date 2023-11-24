import Vector :: *;
import UserLogicSettings :: *;
import UserLogicTypes :: *;
import DataTypes :: *;
import Headers :: *;
import FIFOF :: *;
import Arbitration :: *;
import PAClib :: *;
import PrimUtils :: *;
import ClientServer :: *;
import GetPut :: *;
import ConfigReg :: * ;
import Randomizable :: *;
import PrimUtils :: *;
import Utils :: *;


interface H2CRingBufFifoCntrlIfc#(type t_elem);
    method Action fillBuf(t_elem elem);
    method Bool notEmpty;
endinterface

interface H2CRingBuf#(type t_elem);
    interface PipeOut#(t_elem) pipeout;
    interface H2CRingBufFifoCntrlIfc#(t_elem) cntrl;
endinterface

module mkH2CRingBuf(Integer buf_depth, H2CRingBuf#(t_elem) ifc) provisos (Bits#(t_elem, sz_elem));
    FIFOF#(t_elem) bufQ <- mkSizedFIFOF(buf_depth);

    interface pipeout = toPipeOut(bufQ);

    interface H2CRingBufFifoCntrlIfc cntrl;
        method Action fillBuf(t_elem elem);
            bufQ.enq(elem);
        endmethod
        method Bool notEmpty = bufQ.notEmpty;
    endinterface
endmodule

interface C2HRingBufFifoIfc#(type t_elem);
    method Action enq(t_elem elem);
    method Bool notFull;
endinterface

interface C2HRingBuf#(type t_elem);
    interface C2HRingBufFifoIfc#(t_elem) fifo;
    interface PipeOut#(t_elem) cntrl;
endinterface

module mkC2HRingBuf(Integer buf_depth, C2HRingBuf#(t_elem) ifc) provisos (Bits#(t_elem, sz_elem));
    FIFOF#(t_elem) bufQ <- mkSizedFIFOF(buf_depth);

    interface C2HRingBufFifoIfc fifo;
        method Action enq(t_elem elem);
             bufQ.enq(elem);
        endmethod
        method Bool notFull = bufQ.notFull;
    endinterface

    interface cntrl = toPipeOut(bufQ);
endmodule

function Bool isRingbufNotEmpty(Fix4kBRingBufPointer head, Fix4kBRingBufPointer tail);
    return !(head == tail);
endfunction

function Bool isRingbufNotFull(Fix4kBRingBufPointer head, Fix4kBRingBufPointer tail);
    return !((head.idx == tail.idx) && (head.guard != tail.guard));
endfunction


function Tuple2#(PageNumber4k, PageOffset4k) getPageNumberAndOffset4k(ADDR addr);
    return unpack(pack(addr));
endfunction

typedef struct {
    Bool guard;
    UInt#(w) idx;
} RingbufPointer#(numeric type w) deriving(Bits, Eq);

instance Arith#(RingbufPointer#(w)) provisos(Alias#(RingbufPointer#(w), data_t),Bits#(data_t, TAdd#(w, 1)));
    function data_t \+ (data_t x, data_t y);
        UInt#(TAdd#(w,1)) tx = unpack(pack(x));
        UInt#(TAdd#(w,1)) ty = unpack(pack(y));
        return unpack(pack(tx + ty));
    endfunction

    function data_t \- (data_t x, data_t y);
        UInt#(TAdd#(w,1)) tx = unpack(pack(x));
        UInt#(TAdd#(w,1)) ty = unpack(pack(y));
        return unpack(pack(tx - ty));
    endfunction

    function data_t \* (data_t x, data_t y);
        return error ("The operator " + quote("*") +
                      " is not defined for " + quote("RingbufPointer") + ".");
    endfunction

    function data_t \/ (data_t x, data_t y);
        return error ("The operator " + quote("/") +
                      " is not defined for " + quote("RingbufPointer") + ".");
    endfunction

    function data_t \% (data_t x, data_t y);
        return error ("The operator " + quote("%") +
                      " is not defined for " + quote("RingbufPointer") + ".");
    endfunction

    function data_t negate (data_t x);
        return error ("The operator " + quote("negate") +
                      " is not defined for " + quote("RingbufPointer") + ".");
    endfunction

endinstance

instance Literal#(RingbufPointer#(w));

   function fromInteger(n) ;
        return RingbufPointer{ guard: False, idx: fromInteger(n) } ;
   endfunction
   function inLiteralRange(a, i);
        UInt#(w) idxPart = ?;
        return inLiteralRange(idxPart, i);
   endfunction
endinstance

typedef RingbufPointer#(USER_LOGIC_RING_BUF_DEEP_WIDTH) Fix4kBRingBufPointer;

interface RingbufH2cMetadata;
    interface Reg#(ADDR) addr;
    interface Reg#(Fix4kBRingBufPointer) head;
    interface Reg#(Fix4kBRingBufPointer) tail;
    interface Reg#(Fix4kBRingBufPointer) tailShadow;
    interface RingbufDmaClt dmaClt;
endinterface

module mkRingbufH2cMetadata(RingbufNumber qIdx, H2CRingBufFifoCntrlIfc#(t_elem) fifoCntrl, RingbufH2cMetadata ifc)
    provisos(
        Bits#(t_elem, sz_elem),
        Bits#(DATA, sz_elem)
    );

    Reg#(ADDR) baseAddrReg <- mkReg(0);
    Reg#(Fix4kBRingBufPointer) headReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailShadowReg <- mkConfigReg(unpack(0));
    FIFOF#(RingbufDmaReq) dmaReqQ <- mkFIFOF;
    FIFOF#(RingbufDmaResp) dmaRespQ <- mkFIFOF;

    Reg#(Bool) ruleState <- mkReg(False);
    Reg#(RingbufReadBlockInnerOffset) tailPosInReadBlockReg <- mkReg(0);

    
    rule sendDmaReq if (ruleState == False);

        // generate a temp constant var as mask, use it to align pointer.
        Fix4kBRingBufPointer ringbufReadBlockInnerOffsetMask = 0;
        ringbufReadBlockInnerOffsetMask.idx = ~((1 << valueOf(TLog#(RINGBUF_DESC_ENTRY_PER_READ_BLOCK))) - 1); 

        if (isRingbufNotEmpty(headReg, tailShadowReg) && !fifoCntrl.notEmpty) begin
            let {curReadBlockStartAddrPgn, _} = getPageNumberAndOffset4k(baseAddrReg);

            PageOffset4k curReadBlockStartAddrOff = zeroExtend(
                tailShadowReg.idx >> valueOf(TLog#(RINGBUF_DESC_ENTRY_PER_READ_BLOCK)) 
            ) << valueOf(RINGBUF_READ_BLOCK_BYTE_WIDTH);
            ADDR curReadBlockStartAddr = unpack({pack(curReadBlockStartAddrPgn), pack(curReadBlockStartAddrOff)});

            let readBlockAlignedTailShadow = tailShadowReg + fromInteger(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK));
            readBlockAlignedTailShadow.idx = readBlockAlignedTailShadow.idx & ringbufReadBlockInnerOffsetMask.idx;

            let availableEntryCnt = headReg - tailShadowReg;
            let avaliableSlotInReadBlock = fromInteger(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK)) - pack(tailShadowReg.idx)[valueOf(TLog#(RINGBUF_DESC_ENTRY_PER_READ_BLOCK))-1:0];
            
            Fix4kBRingBufPointer newTailShadow;
            if (pack(availableEntryCnt) > avaliableSlotInReadBlock) begin
                newTailShadow = readBlockAlignedTailShadow;
            end else begin
                newTailShadow = headReg;
            end

            DataStream ds = unpack(0);
            ds.isLast = True;
            ds.isFirst = True;
            dmaReqQ.enq(RingbufDmaReq{
                    isH2c:True,
                    idx: qIdx,
                    addr: curReadBlockStartAddr,
                    len: fromInteger(valueOf(RINGBUF_BLOCK_READ_LEN)),
                    data: ds
            });

            tailPosInReadBlockReg <= truncate(pack(tailReg));

            tailShadowReg <= newTailShadow;
            ruleState <= True;
        end
    endrule

    rule recvDmaResp if (ruleState == True);
        dmaRespQ.deq;
        let resp = dmaRespQ.first;

        if (tailPosInReadBlockReg > 0) begin
            // skip already consumed descriptors in previous block read.
            tailPosInReadBlockReg <= tailPosInReadBlockReg - 1;
            // $display("skip already handled...tailPosInReadBlockReg=", tailPosInReadBlockReg);
        end else begin
            let newTail = tailReg;
            if (tailReg != tailShadowReg) begin
                // the end of read block may contain invalid descriptors, don't handle descriptors beyond tailShadowReg
                t_elem t = unpack(pack(resp.data.data));
                fifoCntrl.fillBuf(t);
                newTail = tailReg + 1;
                tailReg <= newTail;
                // $display("tail incr...old tailReg=%h, new=%x", tailReg, newTail);
            end else begin
                // $display("skip invalid...tailReg=%h", tailReg);
            end

            if (resp.data.isLast) begin
                ruleState <= False;
                immAssert(
                    newTail == tailShadowReg,
                    "shadowTail assertion @ mkRingbufH2cMetadata",
                    $format(
                        "newTail=%h should == shadowTail=%h, ",
                        newTail, tailShadowReg
                    )
                );
            end
        end
    endrule


    interface addr = baseAddrReg;
    interface head = headReg;
    interface tail = tailReg;
    interface tailShadow = tailShadowReg;
    interface dmaClt = toGPClient(dmaReqQ, dmaRespQ);
endmodule



interface RingbufC2hMetadata;
    interface Reg#(ADDR) addr;
    interface Reg#(Fix4kBRingBufPointer) head;
    interface Reg#(Fix4kBRingBufPointer) tail;
    interface Reg#(Fix4kBRingBufPointer) headShadow;
    interface RingbufDmaClt dmaClt;
endinterface


// TODO: For C2H, doesn't support batch descriptor writeback now. 
module mkRingbufC2hMetadata(RingbufNumber qIdx, PipeOut#(t_elem) fifoCntrl, RingbufC2hMetadata ifc)
    provisos(
        Bits#(t_elem, sz_elem),
        Bits#(DATA, sz_elem)
    );

    Reg#(ADDR) baseAddrReg <- mkReg(0);
    Reg#(Fix4kBRingBufPointer) headReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) headShadowReg <- mkConfigReg(unpack(0));
    FIFOF#(RingbufDmaReq) dmaReqQ <- mkFIFOF;
    FIFOF#(RingbufDmaResp) dmaRespQ <- mkFIFOF;


    Reg#(RingbufReadBlockInnerOffset) headPosInReadBlockReg <- mkReg(0);

    
    rule sendDmaReq;

        if (isRingbufNotFull(headShadowReg, tailReg) && fifoCntrl.notEmpty) begin

            let {curWriteBlockStartAddrPgn, _} = getPageNumberAndOffset4k(baseAddrReg);

            PageOffset4k curWriteBlockStartAddrOff = zeroExtend(
                headShadowReg.idx
            ) << valueOf(TLog#(USER_LOGIC_DESCRIPTOR_BYTE_WIDTH));

            ADDR curWriteStartAddr = unpack({pack(curWriteBlockStartAddrPgn), pack(curWriteBlockStartAddrOff)});

            DataStream ds = unpack(0);
            ds.isLast = True;
            ds.isFirst = True;
            ds.byteEn = genByteEn(fromInteger(valueOf(USER_LOGIC_DESCRIPTOR_BYTE_WIDTH)));
            ds.data = unpack(pack(fifoCntrl.first));
            fifoCntrl.deq;

            dmaReqQ.enq(RingbufDmaReq{
                    isH2c: False,
                    idx: qIdx,
                    addr: curWriteStartAddr,
                    len: fromInteger(valueOf(USER_LOGIC_DESCRIPTOR_BYTE_WIDTH)),
                    data: ds
            });

            headShadowReg <= headShadowReg + 1;
        end
    endrule

    rule recvDmaResp;
        dmaRespQ.deq;
        let resp = dmaRespQ.first;
        $display("recvDmaResp @ Q=%d -- head = %x, tail = %x, head_shadow = %x", qIdx, headReg, tailReg, headShadowReg);
        let newHead = headReg + 1;
        headReg <= newHead;
        $display("head incr...old tailReg=%h, new=%x", headReg, newHead);
    endrule


    interface addr = baseAddrReg;
    interface head = headReg;
    interface tail = tailReg;
    interface headShadow = headShadowReg;
    interface dmaClt = toGPClient(dmaReqQ, dmaRespQ);
endmodule


typedef Client#(RingbufDmaReq, RingbufDmaResp) RingbufDmaClt;


interface RingbufPool#(numeric type h2cCount, numeric type c2hCount, type t_elem);
    interface Vector#(h2cCount, PipeOut#(t_elem)) h2cRings;
    interface Vector#(h2cCount, RingbufH2cMetadata) h2cMetas;
    interface Vector#(c2hCount, C2HRingBufFifoIfc#(t_elem)) c2hRings;
    interface Vector#(c2hCount, RingbufC2hMetadata) c2hMetas;
    interface RingbufDmaClt dmaAccessClt;
endinterface

module mkRingbufPool(
    RingbufPool#(h2cCount, c2hCount, t_elem) ifc
) provisos (
    NumAlias#(TAdd#(h2cCount, c2hCount), totalRingCnt),
    Add#(1, anysize, totalRingCnt),
    Add#(TLog#(totalRingCnt), 1, TLog#(TAdd#(1, totalRingCnt))),
    Bits#(t_elem, sz_elem),
    Bits#(DATA, sz_elem)
);
    
    Vector#(totalRingCnt, RingbufDmaClt) dmaAccessCltVec = newVector;

    Vector#(h2cCount, PipeOut#(t_elem)) h2cPipeouts = newVector;
    Vector#(h2cCount, RingbufH2cMetadata) h2cMetaData = newVector;
    for (Integer i=0; i< valueOf(h2cCount); i=i+1) begin
        H2CRingBuf#(t_elem) t <- mkH2CRingBuf(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK));
        h2cPipeouts[i] = t.pipeout;
        h2cMetaData[i] <- mkRingbufH2cMetadata(fromInteger(i), t.cntrl);
        dmaAccessCltVec[i] = h2cMetaData[i].dmaClt;
    end

    Vector#(c2hCount, C2HRingBufFifoIfc#(t_elem)) c2hFifos = newVector;
    Vector#(c2hCount, RingbufC2hMetadata) c2hMetaData = newVector;
    for (Integer i=0; i< valueOf(h2cCount); i=i+1) begin
        C2HRingBuf#(t_elem) t <- mkC2HRingBuf(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK));
        c2hFifos[i] = t.fifo;
        c2hMetaData[i] <- mkRingbufC2hMetadata(fromInteger(i), t.cntrl);
        dmaAccessCltVec[i+valueOf(h2cCount)] = c2hMetaData[i].dmaClt;
    end


    function Bool isRingbufDmaReqFinished(RingbufDmaReq req);
        return req.data.isLast;
    endfunction

    function Bool isRingbufDmaRespFinished(RingbufDmaResp resp);
        return resp.data.isLast;
    endfunction
    

    let arbitratedClient <- mkClientArbiter(
        dmaAccessCltVec,
        isRingbufDmaReqFinished,
        isRingbufDmaRespFinished
    );

    interface h2cRings = h2cPipeouts;
    interface h2cMetas = h2cMetaData;
    interface c2hRings = c2hFifos;
    interface c2hMetas = c2hMetaData;
    interface dmaAccessClt = arbitratedClient;
endmodule



(* synthesize *)
module mkTestRingbuf(Empty) ;
    RingbufPool#(1,1, RingbufRawDescriptor) pool <- mkRingbufPool;

    Reg#(UInt#(20)) cntReg <- mkReg(1);
    Reg#(UInt#(3)) respCntReg <- mkReg(0);
    FIFOF#(RingbufDmaReq) pipelineH2CFifo<- mkFIFOF;
    FIFOF#(RingbufDmaReq) pipelineC2HFifo<- mkFIFOF;
    Reg#(UInt#(7)) expectH2cRecvReg <- mkReg(0); 
    Reg#(UInt#(7)) expectC2hRecvReg <- mkReg(0); 
    Randomize#(Bit#(10)) randomGen <- mkGenericRandomizer;

    Reg#(UInt#(32)) softwareMoveHeadCntReg <- mkReg(0);
    Reg#(UInt#(32)) hardwareMoveHeadCntReg <- mkReg(0);
    Reg#(UInt#(32)) dmaDelaySimulateReg <- mkReg(0);

    Reg#(Bool) initializedReg <- mkReg(False);

    rule init if (!initializedReg);
        randomGen.cntrl.init;
        initializedReg <= True;
    endrule

    rule stop;
        cntReg <= cntReg + 1;
    endrule

    // rule ruleSofrwareModifyH2cHead;
    //     if (isRingbufNotFull(pool.h2cMetas[0].head, pool.h2cMetas[0].tail)) begin
    //         let random <- randomGen.next;
            
    //         // make a two stage random, first one the qeueu is almost empty, second one the queue is almost full
    //         if (
    //             (softwareMoveHeadCntReg < 20000 && random < 50) || 
    //             (softwareMoveHeadCntReg >= 20000 && random > 500)
    //         ) begin
    //             softwareMoveHeadCntReg <= softwareMoveHeadCntReg + 1;
    //             pool.h2cMetas[0].head <= pool.h2cMetas[0].head + 1;
    //         end

    //         if (softwareMoveHeadCntReg > 40000) begin
    //             $display("Passed");
    //             $finish();
    //         end
    //     end
    // endrule 

    rule ruleFakeUserLogicWriteC2hHead;
        let random <- randomGen.next; 
        // make a two stage random, first one the qeueu is almost empty, second one the queue is almost full
  
        if (
            (hardwareMoveHeadCntReg < 20000 && random < 50) || 
            (hardwareMoveHeadCntReg >= 20000 && random > 500)
        ) begin
            $display("aaaaa");   
            hardwareMoveHeadCntReg <= hardwareMoveHeadCntReg + 1;
            pool.c2hRings[0].enq(zeroExtend(pack(hardwareMoveHeadCntReg)));
        end
    endrule

    rule ruleGetToFifoRelay;
        let req <- pool.dmaAccessClt.request.get;
        if (req.isH2c) begin
            pipelineH2CFifo.enq(req);
        end 
        else begin
            pipelineC2HFifo.enq(req);
        end 
    endrule

    // rule ruleFakeDmaEngineGeneratingH2CResponse;

    //     if (dmaDelaySimulateReg > 0) begin
    //         dmaDelaySimulateReg <= dmaDelaySimulateReg -1;
    //     end
    //     if (pipelineH2CFifo.notEmpty && dmaDelaySimulateReg == 0) begin
    //         if ((respCntReg & 'h7) == 7) begin
    //         pipelineH2CFifo.deq;
    //             let random <- randomGen.next;
    //             dmaDelaySimulateReg <= unpack(zeroExtend(random & 'h1F));
    //         end

    //         UInt#(32) reqAddr = unpack(truncate(pipelineH2CFifo.first.addr));

    //         RingbufDmaResp resp = unpack(0);
    //         respCntReg <= respCntReg + 1;
            
    //         resp.data.isFirst = (respCntReg & 'h7) == 0;
    //         resp.data.isLast = (respCntReg & 'h7) == 7;
    //         resp.data.data = extend(pack(reqAddr) + extend(pack(respCntReg)) * 32);

    //         pool.dmaAccessClt.response.put(resp);
    //     end
    // endrule

    rule ruleFakeDmaEngineGeneratingC2HResponse;
        if (pipelineC2HFifo.notEmpty) begin
            expectC2hRecvReg <= expectC2hRecvReg + 1;

            let expectNumber = expectC2hRecvReg;
            UInt#(7) descNumber = unpack(truncate(pipelineC2HFifo.first.data.data));
            immAssert(
                expectNumber == descNumber,
                "c2h descriptor write error @ mkTestRingbuf",
                $format(
                    "expectRecv=%h should == received=%h, ",
                    expectNumber, descNumber
                )
            );

            pipelineC2HFifo.deq;
            RingbufDmaResp resp = unpack(0);
            resp.data.isFirst = True;
            resp.data.isLast = True;
            pool.dmaAccessClt.response.put(resp);
        end
    endrule

    // rule fakeUserLogicReadFifo;
    //     let descriptor = pool.h2cRings[0].first;
    //     pool.h2cRings[0].deq;

    //     Bit#(32) descNumber = truncate(descriptor);
    //     Bit#(32) expectNumber = extend(pack(expectH2cRecvReg)) * 32;
    //     $display(fshow(descriptor), expectH2cRecvReg, descNumber);
    //     immAssert(
    //         expectNumber == descNumber,
    //         "h2c descriptor read error @ mkTestRingbuf",
    //         $format(
    //             "expectRecv=%h should == received=%h, ",
    //             expectNumber, descNumber
    //         )
    //     );
    //     expectH2cRecvReg <= expectH2cRecvReg + 1;
    // endrule

    rule fakeSoftwareMoveC2hTail;
        let random <- randomGen.next;
        if (isRingbufNotEmpty(pool.c2hMetas[0].head, pool.c2hMetas[0].tail)) begin
            if (random < 50) begin 
                pool.c2hMetas[0].tail <= pool.c2hMetas[0].tail + 1;
            end
            else if (random < 100) begin
                pool.c2hMetas[0].tail <= pool.c2hMetas[0].head;
            end
        end
    endrule
endmodule

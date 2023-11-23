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

interface H2CRingBufFifoIfc#(numeric type element_width);
    method Bit#(element_width) first;
    method Action deq;
    method Bool notFull;
    method Bool notEmpty;
endinterface

interface H2CRingBufFifoCntrlIfc#(numeric type element_width);
    method Action fillBuf(Bit#(element_width) elem);
    method Bool notEmpty;
endinterface

interface H2CRingBuf#(numeric type element_width);
    interface H2CRingBufFifoIfc#(element_width) fifo;
    interface H2CRingBufFifoCntrlIfc#(element_width) cntrl;
endinterface

module mkH2CRingBuf(Integer buf_depth, H2CRingBuf#(element_width) ifc) ;
    FIFOF#(Bit#(element_width)) bufQ <- mkSizedFIFOF(buf_depth);

    interface H2CRingBufFifoIfc fifo;
        method Bit#(element_width) first = bufQ.first;
        method Action deq = bufQ.deq;
        method Bool notFull = bufQ.notFull;
        method Bool notEmpty = bufQ.notEmpty;
    endinterface

    interface H2CRingBufFifoCntrlIfc cntrl;
        method Action fillBuf(Bit#(element_width) elem);
            bufQ.enq(elem);
        endmethod
        method Bool notEmpty = bufQ.notEmpty;
    endinterface
endmodule

interface C2HRingBufFifoIfc#(numeric type element_width);
    method Action enq(Bit#(element_width) elem);
    method Bool notFull;
    method Bool notEmpty;
endinterface

interface C2HRingBufFifoCntrlIfc#(numeric type element_width);
    method Bit#(element_width) first;
    method Action deq;
endinterface

interface C2HRingBuf#(numeric type element_width);
    interface C2HRingBufFifoIfc#(element_width) fifo;
    interface C2HRingBufFifoCntrlIfc#(element_width) cntrl;
endinterface


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

module mkRingbufH2cMetadata(RingbufNumber qIdx, H2CRingBufFifoCntrlIfc#(element_width) fifoCntrl, RingbufH2cMetadata ifc)
    provisos(Bits#(DATA, element_width));

    

    Reg#(ADDR) baseAddrReg <- mkReg(0);
    Reg#(Fix4kBRingBufPointer) headReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailReg <- mkConfigReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailShadowReg <- mkConfigReg(unpack(0));
    FIFOF#(RingbufDmaReq) dmaReqQ <- mkFIFOF;
    FIFOF#(RingbufDmaResp) dmaRespQ <- mkFIFOF;

    Reg#(Bool) ruleState <- mkReg(False);
    Reg#(RingbufReadBlockInnerOffset) tailPosInReadBlockReg <- mkReg(0);

    
    rule sendDmaReq if (ruleState == False);

        // generate a temp constant var as mask, use it to align tailShadow pointer.
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

            
            $display("newTailShadow = %x, availableEntryCnt = %x", newTailShadow, availableEntryCnt);

            $display("curReadBlockStartAddr = %x", curReadBlockStartAddr);

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

        $display("recvDmaResp @ Q=%d -- head = %x, tail = %x, tail_shadow = %x", qIdx, headReg, tailReg, tailShadowReg);


        if (tailPosInReadBlockReg > 0) begin
            // skip already consumed descriptors in previous block read.
            tailPosInReadBlockReg <= tailPosInReadBlockReg - 1;
            $display("skip already handled...tailPosInReadBlockReg=", tailPosInReadBlockReg);
        end else begin
            let newTail = tailReg;
            if (tailReg != tailShadowReg) begin
                // the end of read block may contain invalid descriptors, don't handle descriptors beyond tailShadowReg
                fifoCntrl.fillBuf(resp.data.data);
                newTail = tailReg + 1;
                tailReg <= newTail;
                $display("tail incr...old tailReg=%h, new=%x", tailReg, newTail);
            end else begin
                $display("skip invalid...tailReg=%h", tailReg);
            end

            if (resp.data.isLast) begin
                $display("resp last...");
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

typedef Client#(RingbufDmaReq, RingbufDmaResp) RingbufDmaClt;


interface RingbufPool#(numeric type h2cCount, numeric type c2hCount, numeric type element_width);
    interface Vector#(h2cCount, H2CRingBufFifoIfc#(element_width)) h2cRings;
    interface Vector#(h2cCount, RingbufH2cMetadata) h2cMetas;
    interface RingbufDmaClt dmaAccessClt;
endinterface

module mkRingbufPool(
    RingbufPool#(h2cCount, c2hCount, element_width) ifc
) provisos (
    Add#(1, anysize, h2cCount),
    Add#(TLog#(h2cCount), 1, TLog#(TAdd#(1, h2cCount))),
    Bits#(DATA, element_width)
);
    
    Vector#(h2cCount, RingbufDmaClt) dmaAccessCltVec = newVector;
    Vector#(h2cCount, H2CRingBufFifoIfc#(element_width)) h2cFifos = newVector;
    Vector#(h2cCount, RingbufH2cMetadata) h2cMetaData = newVector;
    for (Integer i=0; i< valueOf(h2cCount); i=i+1) begin
        H2CRingBuf#(element_width) t <- mkH2CRingBuf(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK));
        h2cFifos[i] = t.fifo;
        h2cMetaData[i] <- mkRingbufH2cMetadata(fromInteger(i), t.cntrl);
        dmaAccessCltVec[i] = h2cMetaData[i].dmaClt;
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

    interface h2cRings = h2cFifos;
    interface h2cMetas = h2cMetaData;
    interface dmaAccessClt = arbitratedClient;
endmodule


(* synthesize *)
module mkTestRingbuf(Empty) ;
    RingbufPool#(1,1, USER_LOGIC_DESCRIPTOR_BIT_WIDTH) pool <- mkRingbufPool;

    Reg#(UInt#(20)) cntReg <- mkReg(1);
    Reg#(UInt#(3)) respCntReg <- mkReg(0);
    FIFOF#(RingbufDmaReq) pipelineFifo<- mkFIFOF;
    Reg#(UInt#(7)) expectRecvReg <- mkReg(0); 
    Randomize#(Bit#(10)) randomGen <- mkGenericRandomizer;

    Reg#(UInt#(32)) softwareMoveHeadCntReg <- mkReg(0);
    Reg#(UInt#(32)) dmaDelaySimulateReg <- mkReg(0);

    Reg#(Bool) initializedReg <- mkReg(False);

    rule init if (!initializedReg);
        randomGen.cntrl.init;
        initializedReg <= True;
    endrule

    rule stop;
        cntReg <= cntReg + 1;
        // if (cntReg == 0) begin
        //     $display("finish @ cnt = %d", cntReg);
        //     $finish();
        // end
    endrule

    rule ruleSofrwareModifyH2cHead;
        if (isRingbufNotFull(pool.h2cMetas[0].head, pool.h2cMetas[0].tail)) begin
            let random <- randomGen.next;
            
            // make a two stage random, first one the qeueu is almost empty, second one the queue is almost full
            if (
                (softwareMoveHeadCntReg < 20000 && random < 50) || 
                (softwareMoveHeadCntReg >= 20000 && random > 500)
            ) begin
                softwareMoveHeadCntReg <= softwareMoveHeadCntReg + 1;
                pool.h2cMetas[0].head <= pool.h2cMetas[0].head + 1;
            end

            if (softwareMoveHeadCntReg > 40000) begin
                $display("Passed");
                $finish();
            end
        end
       
    endrule 

    rule ruleGetToFifoRelay;
        let req <- pool.dmaAccessClt.request.get;
        pipelineFifo.enq(req);
    endrule

    rule ruleFakeDmaEngineGeneratingResponse;

        if (dmaDelaySimulateReg > 0) begin
            dmaDelaySimulateReg <= dmaDelaySimulateReg -1;
        end
        if (pipelineFifo.notEmpty && dmaDelaySimulateReg == 0) begin
            if ((respCntReg & 'h7) == 7) begin
                pipelineFifo.deq;
                let random <- randomGen.next;
                dmaDelaySimulateReg <= unpack(zeroExtend(random & 'h1F));
            end

            UInt#(32) reqAddr = unpack(truncate(pipelineFifo.first.addr));

            RingbufDmaResp resp = unpack(0);
            respCntReg <= respCntReg + 1;
            
            resp.data.isFirst = (respCntReg & 'h7) == 0;
            resp.data.isLast = (respCntReg & 'h7) == 7;
            resp.data.data = extend(pack(reqAddr) + extend(pack(respCntReg)) * 32);

            pool.dmaAccessClt.response.put(resp);
        end
    endrule

    rule fakeUserLogicReadFifo;
        let descriptor = pool.h2cRings[0].first;
        pool.h2cRings[0].deq;

        Bit#(32) descNumber = truncate(descriptor);
        Bit#(32) expectNumber = extend(pack(expectRecvReg)) * 32;
        $display(fshow(descriptor), expectRecvReg, descNumber);
        immAssert(
            expectNumber == descNumber,
            "h2c descriptor read error @ mkTestRingbuf",
            $format(
                "expectRecv=%h should == received=%h, ",
                expectNumber, descNumber
            )
        );
        expectRecvReg <= expectRecvReg + 1;
    endrule


endmodule

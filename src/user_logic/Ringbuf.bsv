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

module mkH2CRingBuf(H2CRingBuf#(element_width)) ;
    Reg#(Bit#(element_width)) t <- mkRegU;

    interface H2CRingBufFifoIfc fifo;
        method Bit#(element_width) first;
            return t;
        endmethod
        method Action deq;
        endmethod
        method Bool notFull;
            return False;
        endmethod
        method Bool notEmpty;
            return False;
        endmethod
    endinterface

    interface H2CRingBufFifoCntrlIfc cntrl;
        method Action fillBuf(Bit#(element_width) elem);
        endmethod
        method Bool notEmpty;
            return False;
        endmethod
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
    Reg#(Fix4kBRingBufPointer) headReg <- mkReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailReg <- mkReg(unpack(0));
    Reg#(Fix4kBRingBufPointer) tailShadowReg<- mkReg(unpack(0));
    FIFOF#(RingbufDmaReq) dmaReqQ <- mkFIFOF;
    FIFOF#(RingbufDmaResp) dmaRespQ <- mkFIFOF;

    Reg#(Bool) ruleState <- mkReg(False);
    Reg#(RingbufReadBlockInnerOffset) tailPosInReadBlockReg <- mkReg(0);

    
    rule sendDmaReq if (ruleState == False);
        
        if (isRingbufNotEmpty(headReg, tailShadowReg) && !fifoCntrl.notEmpty) begin

            let {curReadBlockStartAddrPgn, _} = getPageNumberAndOffset4k(baseAddrReg);

            PageOffset4k curReadBlockStartAddrOff = zeroExtend(tailShadowReg.idx) << valueOf(RINGBUF_READ_BLOCK_BYTE_WIDTH);
            ADDR curReadBlockStartAddr = unpack({pack(curReadBlockStartAddrPgn), pack(curReadBlockStartAddrOff)});


            let availableEntryCnt = (headReg - tailShadowReg).idx;
            
            Fix4kBRingBufPointer newTailShadow = availableEntryCnt <= fromInteger(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK)) ? 
                                            headReg : tailShadowReg + fromInteger(valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK));

            DataStream ds = unpack(0);
            ds.isLast = True;
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
        end else begin
            fifoCntrl.fillBuf(resp.data.data);
            let newTail = tailReg + 1;
            
            if (resp.data.isLast) begin
                ruleState <= False;
                tailReg <= newTail;
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
    interface Vector#(h2cCount, H2CRingBuf#(element_width)) h2cRings;
endinterface

module mkRingbufPool(
    RingbufPool#(h2cCount, c2hCount, element_width) ifc
) provisos (
    Add#(1, a__, h2cCount),
    Add#(TLog#(h2cCount), 1, TLog#(TAdd#(1, h2cCount))),
    Bits#(DATA, element_width)
);
    
    Vector#(h2cCount, RingbufDmaClt) dmaAccessCltVec = newVector;

    Vector#(h2cCount, H2CRingBuf#(element_width)) h2cFifos = newVector;
    

    Vector#(h2cCount, RingbufH2cMetadata) h2cMetaData = newVector;
    for (Integer i=0; i< valueOf(h2cCount); i=i+1) begin
        h2cFifos[i] <- mkH2CRingBuf;
        h2cMetaData[i] <- mkRingbufH2cMetadata(fromInteger(i), h2cFifos[i].cntrl);
    end


    function Bool isRingbufDmaReqFinished(RingbufDmaReq req) = req.data.isLast;
    function Bool isRingbufDmaRespFinished(RingbufDmaResp resp) = resp.data.isLast;

    let arbitratedClient <- mkClientArbiter(
        dmaAccessCltVec,
        isRingbufDmaReqFinished,
        isRingbufDmaRespFinished
    );

    interface h2cRings = h2cFifos;
endmodule

typedef 256 GGG;
(* synthesize *)
module mkBsvTop(Empty);
    RingbufPool#(2,2, USER_LOGIC_DESCRIPTOR_BIT_WIDTH) pool <- mkRingbufPool;
endmodule

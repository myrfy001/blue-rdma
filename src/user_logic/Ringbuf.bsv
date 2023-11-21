import Vector :: *;
import UserLogicSettings :: *;
import DataTypes :: *;
import Headers :: *;
import FIFOF :: *;
import Arbitration :: *;

interface H2CRingBufFifoIfc#(numeric type element_width);
    method Bit#(element_width) first;
    method Action deq;
    method Bool notFull;
    method Bool notEmpty;
endinterface

interface H2CRingBufCntrlIfc#(numeric type element_width);
    method Action fillBuf(Bit#(element_width) elem);
    method Bool notEmpty;
endinterface

interface H2CRingBuf#(numeric type element_width);
    interface H2CRingBufFifoIfc#(element_width) fifo;
    interface H2CRingBufCntrlIfc#(element_width) cntrl;
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

    interface H2CRingBufCntrlIfc cntrl;
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

interface C2HRingBufCntrlIfc#(numeric type element_width);
    method Bit#(element_width) first;
    method Action deq;
endinterface

interface C2HRingBuf#(numeric type element_width);
    interface C2HRingBufFifoIfc#(element_width) fifo;
    interface C2HRingBufCntrlIfc#(element_width) cntrl;
endinterface

function Tuple2(Bool, UInt#(TSub#(sz, 1))) getRingbufPointerGuardAndIndex(UInt#(sz) pointer);
    return unpack(pack(pointer));
endfunction

function Bool isRingbufNotEmpty(UInt#(sz) head, UInt#(sz) tail);
    return !(head == tail);
endfunction

function Bool isRingbufNotFull(UInt#(sz) head, UInt#(sz) tail);
    let {headGuard, headIdx} = getRingbufPointerGuardAndIndex(head);
    let {tailGuard, tailIdx} = getRingbufPointerGuardAndIndex(tail);
    return !((headIdx == tailIdx) && (headGuard != tailGuard));
endfunction

typedef UInt#(TAdd#(USER_LOGIC_RING_BUF_LEN_WIDTH,1)) RingbufCounter;

interface RingbufMetadata;
    interface Reg#(ADDR) addr;
    interface Reg#(RingbufCounter) head;
    interface Reg#(RingbufCounter) tail;
    interface Reg#(RingbufCounter) tailShadow;
    interface PipeOut#(RingbufDmaReq) dmaReqPipeOut;
endinterface

module mkRingbufMetadata(RingbufIndex qIdx, Bool isH2C, Bool qReady, RingbufMetadata ifc);
    Reg#(ADDR) baseAddrReg <- mkReg(0);
    Reg#(RingbufCounter) headReg <- mkReg(0);
    Reg#(RingbufCounter) tailReg <- mkReg(0);
    Reg#(RingbufCounter) tailShadowReg<- mkReg(0);
    FIFOF#(RingbufDmaReq) dmaReqQ <- mkFIFOF;
    
    rule sendDmaReq;
        if (isH2C) begin
            if (isRingbufNotEmpty(headReg, tailShadowReg) && qReady) begin
                let availableEntryCnt = headReg - tailShadowReg;
                
                RingbufPointer newTailShadow;
                RingbufDMABlockAccessLen readLen;

                if availableEntryCnt <= valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK) begin
                    newTailShadow = headReg;
                    readLen = availableEntryCnt;
                end else begin 
                    newTailShadow = tailShadowReg + valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK);
                    readLen = valueOf(RINGBUF_DESC_ENTRY_PER_READ_BLOCK);
                end
                
                let {newTailShadowGuard, newTailShadowIdx} = getRingbufPointerGuardAndIndex(newTailShadow);
                let {tailShadowGuard, tailShadowIdx} = getRingbufPointerGuardAndIndex(tailShadowReg);
                // check if wrap back occur
                if (newTailShadowGuard != tailShadowGuard) begin
                    newTailShadow = {newTailShadowGuard, '0};
                    readLen = valueOf(USER_LOGIC_RING_BUF_LEN) - 
                    
                    ;
                end

                dmaReqQ.enq(RingbufDmaReq{
                    isH2c:isH2C,
                    idx: qIdx,
                    addr: baseAddrReg + ,
                    len: ,
                    data: ?
                });
            end
        end
    endrule

    interface addr = baseAddrReg;
    interface head = headReg;
    interface tail = tailReg;
    interface tailShadow = tailShadowReg;
    interface  h2cArbitIfc = toPipeOut(dmaReqQ);
endmodule




interface RingbufPool;
   
endinterface

module mkRingbufPool(
    Vector#(h2cCount, H2CRingBufCntrlIfc#(element_width)) h2cFifo,
    RingbufPool ifc
);
    
    Vector#(h2cCount, PipeOut#(WorkComp)) qpRecvWorkCompPipeOutVec = newVector;

    function Bool arbiterFakeFinishFunc(WorkComp wc) = True;
    PipeOutArbiter arbiter <- mkPipeOutArbiter(, arbiterFakeFinishFunc);

    Vector#(h2cCount, RingbufMetadata) h2cMetaData;
    FIFOF#(Bool) tq <- mkFIFOF;
    for (Integer i=0; i< valueOf(h2cCount); i=i+1) begin
        h2cMetaData[i] <- mkRingbufMetadata(i, True, !h2cFifo[i].notEmpty, tq);
    end

endmodule

typedef 2 GGG;
module mkBsvTop(Empty);
    Vector#(GGG,H2CRingBuf#(GGG)) v1 <- replicateM(mkH2CRingBuf);

    Vector#(GGG, H2CRingBufCntrlIfc#(GGG)) v2;
    for (Integer i=0; i<valueOf(GGG); i=i+1) begin
        v2[i] = v1[i].cntrl;
    end
    RingbufPool pool <- mkRingbufPool(v2);
endmodule

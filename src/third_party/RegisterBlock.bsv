
import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import Vector :: *;
import Ringbuf :: *;
import ClientServer :: *;

import UserLogicTypes :: *;
import UserLogicSettings :: *;


typedef enum {
    CsrIdxRbBaseAddrLow = 'h0,
    CsrIdxRbBaseAddrHigh = 'h1,
    CsrIdxRbHead = 'h2,
    CsrIdxRbTail = 'h3,
    CsrIdxMaxGuard = 'h3FF
} CsrPageIndexForRingbuf deriving(Bits, Eq, FShow);

typedef struct {
    Bool isH2c;
    UInt#(RINGBUF_NUMBER_WIDTH) queueIndex;
    CsrPageIndexForRingbuf regIndex;
} CsrRingbufRegsAddress deriving (Bits, Eq, FShow);


interface RegisterBlock#(type t_addr, type t_data);
    interface Server#(CsrWriteRequest#(t_addr, t_data), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(t_addr), CsrReadResponse#(t_data)) csrReadSrv;
endinterface


module mkRegisterBlock(
    Vector#(h2cCount, RingbufH2cMetadata) h2cMetas, 
    Vector#(c2hCount, RingbufC2hMetadata) c2hMetas,
     RegisterBlock#(CsrAddr, CsrData) ifc
);
    FIFOF#(CsrWriteRequest#(CsrAddr, CsrData)) writeReqQ <- mkFIFOF;
    FIFOF#(CsrWriteResponse) writeRespQ <- mkFIFOF;
    FIFOF#(CsrReadRequest#(CsrAddr)) readReqQ <- mkFIFOF;
    FIFOF#(CsrReadResponse#(CsrData)) readRespQ <- mkFIFOF;

    rule ruleHandleWrite;
        CsrRingbufRegsAddress regAddr = unpack(truncate(pack(writeReqQ.first.addr)>>2));
        let data = writeReqQ.first.data;
        writeReqQ.deq;
        case (regAddr.regIndex) matches
            CsrIdxRbBaseAddrLow: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].addr[31:0] <= unpack(data);
                end 
                else begin
                    c2hMetas[regAddr.queueIndex].addr[31:0] <= unpack(data);
                end
            end
            CsrIdxRbBaseAddrHigh: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].addr[63:32] <= unpack(data);
                end 
                else begin
                    c2hMetas[regAddr.queueIndex].addr[63:32] <= unpack(data);
                end
            end 
            CsrIdxRbHead: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].head <= unpack(truncate(data));
                end 
                else begin
                    c2hMetas[regAddr.queueIndex].head <= unpack(truncate(data));
                end
            end
            CsrIdxRbTail: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].tail <= unpack(truncate(data));
                end 
                else begin
                    c2hMetas[regAddr.queueIndex].tail <= unpack(truncate(data));
                end
            end
            default: begin 
                $display("CSR write unknown addr: %x", writeReqQ.first.addr);
                $finish;
            end
        endcase

        writeRespQ.enq(CsrWriteResponse{flag: 0});
    endrule

    rule ruleHandleRead;

        CsrRingbufRegsAddress regAddr = unpack(truncate(pack(readReqQ.first.addr)>>2));
        readReqQ.deq;
        CsrData retData = ?;
        case (regAddr.regIndex) matches
            CsrIdxRbBaseAddrLow: begin
                if (regAddr.isH2c) begin
                    retData = zeroExtend(pack(h2cMetas[regAddr.queueIndex].addr[31:0]));
                end 
                else begin
                    retData = zeroExtend(pack(c2hMetas[regAddr.queueIndex].addr[31:0]));
                end
            end
            CsrIdxRbBaseAddrHigh: begin
                if (regAddr.isH2c) begin
                    retData = zeroExtend(pack(h2cMetas[regAddr.queueIndex].addr[63:32]));
                end 
                else begin
                    retData = zeroExtend(pack(c2hMetas[regAddr.queueIndex].addr[63:32]));
                end
            end 
            CsrIdxRbHead: begin
                if (regAddr.isH2c) begin
                    retData = zeroExtend(pack(h2cMetas[regAddr.queueIndex].head));
                end 
                else begin
                    retData = zeroExtend(pack(c2hMetas[regAddr.queueIndex].head));
                end
            end
            CsrIdxRbTail: begin
                if (regAddr.isH2c) begin
                    retData = zeroExtend(pack(h2cMetas[regAddr.queueIndex].tail));
                end 
                else begin
                    retData = zeroExtend(pack(c2hMetas[regAddr.queueIndex].tail));
                end
            end
            default: begin 
                $display("CSR read unknown addr: %x", readReqQ.first.addr);
                $finish;
            end
        endcase



        readRespQ.enq(CsrReadResponse{data: retData});
    endrule

    interface csrWriteSrv = toGPServer(writeReqQ, writeRespQ);
    interface csrReadSrv = toGPServer(readReqQ, readRespQ);
endmodule

import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import Vector :: *;
import Ringbuf :: *;
import ClientServer :: *;

import UserLogicTypes :: *;
import UserLogicSettings :: *;


typedef enum {
    CsrIdxRbBaseAddrLow = 'h000,
    CsrIdxRbBaseAddrHigh = 'h001,
    CsrIdxRbHead = 'h002,
    CsrIdxRbTail = 'h003
} CsrPageIndexForRingbuf deriving(Bits, Eq, FShow);

typedef struct {
    Bool isH2c;
    UInt#(RINGBUF_NUMBER_WIDTH) queueIndex;
    CsrPageIndexForRingbuf regIndex;
} CsrRingbufRegsAddress deriving (Bits, Eq, FShow);


interface RegisterBlock#(type t_addr, type t_data);
    interface Server#(CsrWriteRequest#(t_addr, t_data), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(t_addr), CsrReadResponse#(t_data)) csrReadSrv;
    // method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest#(t_addr, t_data) req);
    // method ActionValue#(CsrReadResponse#(t_data)) readReg(CsrReadRequest#(t_addr) req);
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
                end else begin
                    c2hMetas[regAddr.queueIndex].addr[31:0] <= unpack(data);
                end
            end
            CsrIdxRbBaseAddrHigh: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].addr[63:32] <= unpack(data);
                end else begin
                    c2hMetas[regAddr.queueIndex].addr[63:32] <= unpack(data);
                end
            end 
            CsrIdxRbHead: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].head <= unpack(truncate(data));
                end else begin
                    c2hMetas[regAddr.queueIndex].head <= unpack(truncate(data));
                end
            end
            CsrIdxRbTail: begin
                if (regAddr.isH2c) begin
                    h2cMetas[regAddr.queueIndex].tail <= unpack(truncate(data));
                end else begin
                    c2hMetas[regAddr.queueIndex].tail <= unpack(truncate(data));
                end
            end
            default: begin 
                $display("unknown addr");
            end
        endcase

        writeRespQ.enq(CsrWriteResponse{flag: 0});
    endrule

    // rule ruleHandleRead;
    //     let _ = readReqWire;
    //     readRespWire <= unpack(0);
    // endrule

    interface csrWriteSrv = toGPServer(writeReqQ, writeRespQ);
    interface csrReadSrv = toGPServer(readReqQ, readRespQ);
endmodule
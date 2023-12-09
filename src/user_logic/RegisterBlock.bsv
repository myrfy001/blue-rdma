
import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import Vector :: *;
import Ringbuf :: *;
import ClientServer :: *;

import UserLogicTypes :: *;



typedef enum {
    CsrIdxRbBaseAddrLow = 'h0000,
    CsrIdxRbBaseAddrHigh = 'h0001,
    CsrIdxRbHead = 'h0002,
    CsrIdxRbTail = 'h0003
} CsrPageIndexForRingbuf deriving(Bits, Eq);


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
        CsrPageIndexForRingbuf regIdx = unpack(truncate(pack(writeReqQ.first.addr)>>2));
        let data = writeReqQ.first.data;
        writeReqQ.deq;
        case (regIdx) matches
            CsrIdxRbBaseAddrLow: h2cMetas[0].addr[31:0] <= unpack(data);
            CsrIdxRbBaseAddrHigh: h2cMetas[0].addr[63:32] <= unpack(data);
            CsrIdxRbHead: h2cMetas[0].head <= unpack(truncate(data));
            CsrIdxRbTail: h2cMetas[0].tail <= unpack(truncate(data));
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
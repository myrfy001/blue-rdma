
import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import Vector :: *;
import Ringbuf :: *;

import UserLogicTypes :: *;


typedef enum {
    CsrIdxRbBaseAddrLow = 'h0000,
    CsrIdxRbBaseAddrHigh = 'h0001,
    CsrIdxRbHead = 'h0002,
    CsrIdxRbTail = 'h0003
} CsrPageIndexForRingbuf deriving(Bits, Eq);


interface RegisterBlock#(type t_addr, type t_data);
    // interface Server#(CsrWriteRequest#(t_addr, t_data), CsrWriteRespons) csrWriteSrv;
    // interface Server#(CsrReadRequest#(t_addr), CsrReadResponse#(t_data)) csrReadSrv;
    method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest#(t_addr, t_data) req);
    method ActionValue#(CsrReadResponse#(t_data)) readReg(CsrReadRequest#(t_addr) req);
endinterface


module mkRegisterBlock(
    Vector#(h2cCount, RingbufH2cMetadata) h2cMetas, 
    Vector#(c2hCount, RingbufC2hMetadata) c2hMetas,
     RegisterBlock#(CsrAddr, CsrData) ifc
);

    method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest#(CsrAddr, CsrData) req) ;
        CsrPageIndexForRingbuf regIdx = unpack(truncate(pack(req.addr)>>2));
        case (regIdx) matches
            CsrIdxRbBaseAddrLow: h2cMetas[0].addr[31:0] <= unpack(req.data);
            CsrIdxRbBaseAddrHigh: h2cMetas[0].addr[63:32] <= unpack(req.data);
            CsrIdxRbHead: h2cMetas[0].head <= unpack(truncate(req.data));
            // CsrIdxRbTail: h2cMetas[0].tail <= unpack(truncate(req.data));
            default: begin 
                $display("unknown addr");
            end
        endcase

        return CsrWriteResponse{flag: 0};
        
    endmethod

    method ActionValue#(CsrReadResponse#(CsrData)) readReg(CsrReadRequest#(CsrAddr) req);
        
        // t_data outData = 'hFFFFFFFF;
        // case (unpack(truncate(req.addr>>2))) matches
        //     CtlRegIdCmdExecuteStatus: begin
                
        //     end
        //     default: begin 
        //         $display("unknown addr");
        //     end
        // endcase

        return unpack(0);
        
    endmethod
    
endmodule
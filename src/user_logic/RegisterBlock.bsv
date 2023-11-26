
import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;


import UserLogicTypes :: *;


typedef enum {
    CtlRegIdCmdSize = 'h0000,
    CtlRegIdCmdAddrLow = 'h0001,
    CtlRegIdCmdAddrHigh = 'h0002,
    CtlRegIdCmdTypeAndId = 'h0003,
    CtlRegIdCmdExecuteStatus = 'h0004
} ControlRegisterAddress deriving(Bits, Eq);


interface RegisterBlock#(type t_addr, type t_data);
    interface Server#(CsrWriteRequest#(t_addr, t_data), CsrWriteRespons) csrWriteSrv;
    interface Server#(CsrReadRequest#(t_addr), CsrReadResponse#(t_data)) csrReadSrv;
endinterface


module mkRegisterBlock(RegisterBlock#(t_addr, t_data))
    provisos (
        Bits#(t_addr, sz_addr),
        Bits#(t_data, sz_data)
    );


    Reg#(t_data) ctlRegCmdSize <- mkRegU;
    Reg#(t_data) ctlRegCmdAddrL <- mkRegU;
    Reg#(t_data) ctlRegCmdAddrH <- mkRegU;

    method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest#(t_addr, t_data) req) ;
        case (unpack(truncate(req.addr>>2))) matches
            // CtlRegIdCmdSize: ctlRegCmdSize <= unpack(req.data);
            // CtlRegIdCmdAddrLow: ctlRegCmdAddrL <= unpack(req.data);
            // CtlRegIdCmdAddrHigh: ctlRegCmdAddrH <= unpack(req.data);
            CtlRegIdCmdTypeAndId: begin
                
            end 
            default: begin 
                $display("unknown addr");
            end
        endcase

        return CsrWriteResponse{flag: 0};
        
    endmethod

    method ActionValue#(CsrReadResponse#(t_data)) readReg(CsrReadRequest#(t_addr) req);
        
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
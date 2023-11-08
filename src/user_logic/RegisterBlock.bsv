
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


interface RegisterBlock;
    interface Get#(RdmaControlCmdEntry) pendingControlCmd;
    interface Put#(RdmaCmdExecuteResponse) pendingControlCmdResp;
    method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest req);
    method ActionValue#(CsrReadResponse) readReg(CsrReadRequest req);
endinterface


module mkRegisterBlock(RegisterBlock) ;


    Reg#(Bit#(CONTROL_REG_DATA_WIDTH)) ctlRegCmdSize <- mkRegU;
    Reg#(Bit#(HOST_ADDR_WIDTH)) ctlRegCmdAddr <- mkRegU;

    FIFOF#(RdmaControlCmdEntry) pendingCmdQ <- mkFIFOF;
    FIFOF#(RdmaCmdExecuteResponse) pendingCmdRespQ <- mkFIFOF;

    


    method ActionValue#(CsrWriteResponse) writeReg(CsrWriteRequest req);

        case (unpack(truncate(req.addr>>2))) matches
            CtlRegIdCmdSize: ctlRegCmdSize <= req.data;
            CtlRegIdCmdAddrLow: ctlRegCmdAddr[31:0] <= req.data;
            CtlRegIdCmdAddrHigh: ctlRegCmdAddr[63:32] <= req.data;
            CtlRegIdCmdTypeAndId: begin
                if (pendingCmdQ.notFull) begin
                    pendingCmdQ.enq(RdmaControlCmdEntry{
                        ctlRegCmdSize: ctlRegCmdSize,
                        ctlRegCmdAddr: ctlRegCmdAddr,
                        ctlRegCmdTypeAndId: unpack(req.data)
                    });
                end
            end 
            default: begin 
                $display("unknown addr");
            end
        endcase

        return CsrWriteResponse{flag: 0};
        
    endmethod

    method ActionValue#(CsrReadResponse) readReg(CsrReadRequest req);
        
        Bit#(CONTROL_REG_DATA_WIDTH) outData = 32'hFFFFFFFF;
        case (unpack(truncate(req.addr>>2))) matches
            CtlRegIdCmdExecuteStatus: begin
                Bool hasResp = False;
                if (pendingCmdRespQ.notEmpty) begin
                    pendingCmdRespQ.deq;
                    hasResp = True;
                end

                Bool cmdQFull = !pendingCmdQ.notFull;
                outData = {pack(cmdQFull), pack(hasResp), 6'h0 ,pendingCmdRespQ.first.errorCode, pendingCmdRespQ.first.finishedReqId};
            end
            default: begin 
                $display("unknown addr");
            end
        endcase

        return CsrReadResponse{data:outData};
        
    endmethod
    
    
    interface pendingControlCmd = toGet(pendingCmdQ);
    interface pendingControlCmdResp = toPut(pendingCmdRespQ);
endmodule
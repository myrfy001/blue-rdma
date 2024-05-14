import Settings :: *;
import Clocks :: * ;

interface GlobalSoftReset;
    method Action doReset;
    method Bool resetOut;
endinterface

(* synthesize *)
module mkGlobalSoftReset#(Clock boardSysCLk, Reset boardSysReset)(GlobalSoftReset);

    // according to Xilinx PG195, reset signal should last 50ms.
    Reg#(UInt#(TLog#(BOARD_SOFT_RESET_COUNTER_VALUE))) delayCounteReg <- mkReg(
        fromInteger(valueOf(BOARD_SOFT_RESET_COUNTER_VALUE)),
        clocked_by boardSysCLk,
        reset_by boardSysReset
    );
    Reg#(Bool) isResetingReg[2] <- mkCReg(2, False, clocked_by boardSysCLk, reset_by boardSysReset);

    SyncBitIfc#(Bool) resetReqSyncBit <- mkSyncBitFromCC(boardSysCLk);
    SyncBitIfc#(Bool) resetSignalOutSyncBit <- mkSyncBitToCC(boardSysCLk, boardSysReset);

    rule doCounter if (isResetingReg[1]);
        if (delayCounteReg == 0) begin
            isResetingReg[1] <= False;
            delayCounteReg <= fromInteger(valueOf(BOARD_SOFT_RESET_COUNTER_VALUE));
        end
        else begin
            delayCounteReg <= delayCounteReg - 1;
        end
    endrule

    (* no_implicit_conditions, fire_when_enabled *)        
    rule forwardResetReqCDC;
        if (!isResetingReg[0]) begin
            if (resetReqSyncBit.read) begin
                isResetingReg[0] <= True;
            end
        end
    endrule

    (* no_implicit_conditions, fire_when_enabled *)        
    rule forwardResetOutSignalCDC;
        resetSignalOutSyncBit.send(isResetingReg[0]);
    endrule

    method Action doReset;
        resetReqSyncBit.send(True);
    endmethod
    method Bool resetOut = !(resetSignalOutSyncBit.read);
endmodule
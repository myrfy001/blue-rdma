import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import ClientServer :: *;

import RegisterBlock :: *;
import XdmaWrapper :: *;

import DataTypes :: *;
import UserLogicTypes :: *;
import AddressTranslate :: *;

interface ControlCmdManager;
   
endinterface

module mkControlCmdManager#(
        RegisterBlock regBlock,
        DmaRouter dmaRouter,
        PgtManager pgtManager
    )(ControlCmdManager);

    rule forwardCmdFetchDmaReq;
        let inReq <- regBlock.pendingControlCmd.get;
        let outReq = DmaReadReq {
            initiator: DMA_SRC_CONTROL_PATH_LOGIC,
            sqpn: extend(pack(inReq.ctlRegCmdTypeAndId.cmdType)),
            startAddr: inReq.ctlRegCmdAddr,
            len: truncate(inReq.ctlRegCmdSize),
            wrID: extend(inReq.ctlRegCmdTypeAndId.reqId)
        };
        dmaRouter.dmaCtrlPathReadSrv.request.put(outReq);
    endrule

    rule dispatchCmdFetchedFromDma;
        let in_resp <- dmaRouter.dmaCtrlPathReadSrv.response.get;
        let out_resp = DmaFetchedCmd {
            cmdType: unpack(truncate(in_resp.sqpn)),
            reqId: unpack(truncate(in_resp.wrID)),
            dataStream: in_resp.dataStream
        };

        case (out_resp.cmdType)
            RdmaCsrCmdTypeModifyFirstStagePgt: begin
                pgtManager.pgtModifySrv.request.put(out_resp);
            end
        endcase 
    endrule

    rule forwardClientExecuteRespForPgt;
        let in_resp <- pgtManager.pgtModifySrv.response.get;
        regBlock.pendingControlCmdResp.put(in_resp);
    endrule
endmodule
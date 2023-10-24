import ClientServer :: *;
import GetPut :: *;

import Axi4LiteTypes :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import XdmaWrapper :: *;
import RegisterBlock :: *;



interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    interface RawAxi4LiteSlave#(CONTROL_REG_ADDR_WIDTH, CONTROL_REG_DATA_STRB_WIDTH) axilRegBlock;
endinterface

(* synthesize *)
module mkBsvTop(BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH));
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper;

    RegisterBlock#(CONTROL_REG_ADDR_WIDTH, CONTROL_REG_DATA_STRB_WIDTH) regBlock<- mkRegisterBlock;

    rule doTestSendReq;
        let req = DmaReadReq {
            initiator: DMA_SRC_RQ_RD,
            sqpn: 1,
            startAddr: 0,
            len: 16,
            wrID: 0
        };
        
        xdmaWrap.dmaReadSrv.request.put(req);
    endrule

    rule doTestRecvReq;
        let _ <- xdmaWrap.dmaReadSrv.response.get;
    endrule



    interface xdmaChannel = xdmaWrap.xdmaChannel;
    interface axilRegBlock = regBlock.axilRegBlock;
endmodule
import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import Vector :: *;

import Axi4LiteTypes :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import XdmaWrapper :: *;
import RegisterBlock :: *;
// import ControlCmdManager :: *;
import UserLogicTypes :: *;
import AddressTranslate :: *;
import Ringbuf :: *;
import Arbitration :: *;


interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    // interface RawAxi4LiteSlave#(CONTROL_REG_ADDR_WIDTH, CONTROL_REG_DATA_STRB_WIDTH) axilRegBlock;
endinterface

(* synthesize *)
module mkBsvTop(BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH));
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper;
    
    RegisterBlock regBlock <- mkRegisterBlock;
    // XdmaAxiLiteBridgeWrapper xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(regBlock);

    BluerdmaDmaProxy bluerdmaDmaProxy <- mkBluerdmaDmaProxy;
    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;
    
    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(2, RingbufDmaH2cClt) dmaAccessH2cCltVec = newVector;
    Vector#(2, RingbufDmaC2hClt) dmaAccessC2hCltVec = newVector;

    dmaAccessH2cCltVec[0] = bluerdmaDmaProxy.userlogicSideReadClt;
    dmaAccessH2cCltVec[1] = ringbufPool.dmaAccessH2cClt;
    dmaAccessC2hCltVec[0] = bluerdmaDmaProxy.userlogicSideWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    BluerdmaDmaProxy blueRdmaProxy <- mkBluerdmaDmaProxy;
    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter(dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter(dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);




    TLB tlb <- mkTLB;
    PgtManager pgtManager <- mkPgtManager(tlb);
    // ControlCmdManager controlCmdManager <- mkControlCmdManager(regBlock, dmaRouter, pgtManager);

    mkConnection(xdmaWrap.dmaReadSrv, xdmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, xdmaWriteClt);

    


    
    // rule doTestH2cRecvReq;
    //     let _ <- xdmaWrap.dmaReadSrv.response.get;
    // endrule


    // rule doTestC2hSendReq;
    //     let req = DmaWriteReq {
    //         metaData: DmaWriteMetaData {
    //             initiator: DMA_SRC_RQ_RD,
    //             sqpn: 1,
    //             startAddr: 0,
    //             len: 16,
    //             psn: 0
    //         },
    //         dataStream: DataStream {
    //             data: 'h61616262,
    //             byteEn: 'b1111,
    //             isFirst: True,
    //             isLast: True
    //         }
    //     };
        
    //     xdmaWrap.dmaWriteSrv.request.put(req);
    // endrule

    // rule doTestC2hRecvReq;
    //     let _ <- xdmaWrap.dmaWriteSrv.response.get;
    // endrule



    interface xdmaChannel = xdmaWrap.xdmaChannel;
    // interface axilRegBlock = xdmaAxiLiteWrap.axiLiteBridge;
endmodule
import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Clocks :: * ;
import Vector :: *;

import Axi4LiteTypes :: *;

import PipeIn :: *;
import RQ :: *;
import QPContext :: *;
import MetaData :: *;
import DataTypes :: *;
import InputPktHandle :: *;
import RdmaUtils :: *;

import ExtractAndPrependPipeOut :: *;

import MemRegionAndAddressTranslate :: *;
import PayloadConAndGen :: *;
import XdmaWrapper :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;
import Arbitration :: *;
import Ringbuf :: *;
import UserLogicUtils :: *;








interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    // interface RawAxi4LiteSlave#(CSR_ADDR_WIDTH, CSR_DATA_STRB_WIDTH) axilRegBlock;
    interface Clock slowClockIfc;
    interface Put#(DataStream) rdmaDataStreamInput;
    // interface DataStreamPipeOut rdmaDataStreamPipeOut;
endinterface


(* synthesize *)
module mkBsvTop(Clock slowClock, Reset slowReset, BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) ifc);
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by slowClock, reset_by slowReset);
    // XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(slowClock, slowReset);
    TopCoreIfc bsvTopCore <- mkTopCore(slowClock, slowReset);
    // mkConnection(xdmaAxiLiteWrap.csrWriteClt, bsvTopCore.csrWriteSrv);
    // mkConnection(xdmaAxiLiteWrap.csrReadClt, bsvTopCore.csrReadSrv);
    mkConnection(xdmaWrap.dmaReadSrv, bsvTopCore.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, bsvTopCore.dmaWriteClt);

    interface xdmaChannel = xdmaWrap.xdmaChannel;
    interface slowClockIfc = slowClock;
    // interface axilRegBlock = xdmaAxiLiteWrap.cntrlAxil;
    interface rdmaDataStreamInput = bsvTopCore.rdmaDataStreamInput;
    // interface rdmaDataStreamPipeOut = bsvTopCore.rdmaDataStreamPipeOut;
endmodule









interface TopCoreIfc;
    interface Put#(DataStream) rdmaDataStreamInput;

    // interface XdmaChannel#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaChannel;
    // interface RawAxi4LiteSlave#(CSR_ADDR_WIDTH, CSR_DATA_STRB_WIDTH) axilRegBlock;

    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;

    // tmp interfaces for debuging while still in developing
    interface Server#(QPCWriteReqCommon, Bool) qpcWriteCommonSrv;
    interface Server#(MrTableModifyReq, MrTableModifyResp) mrModifySrv;
    interface Server#(PgtModifyReq, PgtModifyResp) pgtModifySrv;

endinterface

(* synthesize *)
module mkTopCore(Clock slowClock, Reset slowReset, TopCoreIfc ifc);

    BluerdmaDmaProxyForRQ bluerdmaDmaProxyForRQ <- mkBluerdmaDmaProxyForRQ;

    QPContext qpc <- mkQPContext;
    RQ rq <- mkRQ;

    FIFOF#(DataStream) inputDataStreamQ <- mkFIFOF;

    let headerAndMetaDataAndPayloadPipeOut <- mkExtractHeaderFromRdmaPktPipeOut;
    mkConnection(toPut(headerAndMetaDataAndPayloadPipeOut.rdmaPktPipeIn), toGet(inputDataStreamQ));

    let inputRdmaPktBufAndHeaderValidation <- mkInputRdmaPktBufAndHeaderValidation;

    mkConnection(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData, inputRdmaPktBufAndHeaderValidation.headerMetaDataPipeIn);
    mkConnection(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream, inputRdmaPktBufAndHeaderValidation.headerDataStreamPipeIn);
    mkConnection(headerAndMetaDataAndPayloadPipeOut.payload, inputRdmaPktBufAndHeaderValidation.payloadPipeIn);

    mkConnection(inputRdmaPktBufAndHeaderValidation.qpcReadCommonClt, qpc.readCommonSrv);
    mkConnection(inputRdmaPktBufAndHeaderValidation.reqPktPipeOut.pktMetaData, rq.pktMetaDataPipeIn);

    
    let payloadConsumer <- mkPayloadConsumer;

    mkConnection(inputRdmaPktBufAndHeaderValidation.reqPktPipeOut.payload, payloadConsumer.payloadPipeIn);
    mkConnection(rq.payloadXonsumerControlPortClt, payloadConsumer.controlPortSrv);


    MemRegionTable mrTable <- mkMemRegionTable;
    mkConnection(mrTable.querySrv, rq.mrTableQueryClt);

    TLB tlb <- mkTLB;
    mkConnection(tlb.translateSrv, rq.pgtQueryClt);

    // XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by slowClock, reset_by slowReset);
    // XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(slowClock, slowReset);



    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(4, RingbufDmaH2cClt) dmaAccessH2cCltVec = newVector;
    Vector#(2, RingbufDmaC2hClt) dmaAccessC2hCltVec = newVector;

    dmaAccessH2cCltVec[0] = bluerdmaDmaProxyForRQ.userlogicSideReadClt;
    dmaAccessH2cCltVec[1] <- mkFakeClient;
    dmaAccessH2cCltVec[2] <- mkFakeClient;
    dmaAccessH2cCltVec[3] <- mkFakeClient;

    dmaAccessC2hCltVec[0] = bluerdmaDmaProxyForRQ.userlogicSideWriteClt;
    dmaAccessC2hCltVec[1] <- mkFakeClient;

    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter(dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter(dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);

    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(slowClock, slowReset);

    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);


    // mkConnection(rdmaTransportLayer.dmaReadClt, bluerdmaDmaProxyForRQ.blueSideReadSrv);   
    mkConnection(payloadConsumer.dmaWriteClt, bluerdmaDmaProxyForRQ.blueSideWriteSrv);

    interface rdmaDataStreamInput = toPut(inputDataStreamQ);

    // interface xdmaChannel = xdmaWrap.xdmaChannel;
    // interface axilRegBlock = xdmaAxiLiteWrap.cntrlAxil;



    interface qpcWriteCommonSrv = qpc.writeCommonSrv;
    interface mrModifySrv = mrTable.modifySrv;
    interface pgtModifySrv = tlb.modifySrv;

    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;

endmodule


import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Clocks :: * ;
import Vector :: *;
import PAClib :: *;
import AlignedFIFOs :: * ;

import Axi4LiteTypes :: *;
import XilinxCmacController :: *;
import UdpIpArpEthCmacRxTx :: *;
import Ports :: *;
import EthernetTypes :: *;
import SemiFifo :: *;
import StreamHandler :: *;
import XilinxAxiStreamAsyncFifo :: *;
import UdpIpEthCmacRxTx :: *;

import Settings :: *;
import RQ :: *;
import QPContext :: *;
import MetaData :: *;
import DataTypes :: *;
import InputPktHandle :: *;
import RdmaUtils :: *;
import SendQ :: *;
import PayloadGen :: *;
import Headers :: *;

import ExtractAndPrependPipeOut :: *;

import MemRegionAndAddressTranslate :: *;
import PayloadCon :: *;
import XdmaWrapper :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;
import Arbitration :: *;
import Ringbuf :: *;
import UserLogicUtils :: *;
import RegisterBlock :: *;
import CmdQueue :: *;
import WorkQueueRingbuf :: *;

import PrimUtils :: *;

// import SimDma :: *;

typedef 4791 TEST_UDP_PORT;
typedef 32 CMAC_SYNC_BRAM_BUF_DEPTH;
typedef 4 CMAC_CDC_SYNC_STAGE;


interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    interface RawAxi4LiteSlave#(CSR_ADDR_WIDTH, CSR_DATA_STRB_WIDTH) axilRegBlock;
    
    
    // Interface with CMAC IP
    (* prefix = "" *)
    interface XilinxCmacController cmacController;
endinterface


(* synthesize *)
module mkBsvTop(
    Clock udpClock, 
    Reset udpReset,
    (* osc   = "cmac_rxtx_clk" *) Clock cmacRxTxClk,
    (* reset = "cmac_rx_reset" *) Reset cmacRxReset,
    (* reset = "cmac_tx_reset" *) Reset cmacTxReset,
    BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) ifc
);

    Clock dmacClock <- exposeCurrentClock;
    Reset dmacReset <- exposeCurrentReset;
    
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by dmacClock, reset_by dmacReset);
    XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(dmacClock, dmacReset);
    RdmaUserLogicWithoutXdmaAndCmacWrapper udpAndRdma <- mkRdmaUserLogicWithoutXdmaAndCmacWrapper(udpClock, udpReset, dmacClock, dmacReset);
    mkConnection(xdmaAxiLiteWrap.csrWriteClt, udpAndRdma.csrWriteSrv);
    mkConnection(xdmaAxiLiteWrap.csrReadClt, udpAndRdma.csrReadSrv);
    mkConnection(xdmaWrap.dmaReadSrv, udpAndRdma.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, udpAndRdma.dmaWriteClt);


    Bool isCmacTxWaitRxAligned = True;
    Bool isEnableFlowControl = False;
    Bool isEnableRsFec = True;

    let axiStream512RxIn <- mkPutToFifoIn(udpAndRdma.axiStreamRxInUdp, clocked_by udpClock, reset_by udpReset);

    let axiStream512SyncFifoForCMAC <- mkDuplexAxiStreamAsyncFifo(
        valueOf(CMAC_SYNC_BRAM_BUF_DEPTH),
        valueOf(CMAC_CDC_SYNC_STAGE),
        udpClock,
        udpReset,
        cmacRxTxClk,
        cmacRxReset,
        cmacTxReset,
        axiStream512RxIn,
        udpAndRdma.axiStreamTxOutUdp,
        clocked_by udpClock,
        reset_by udpReset
    );


    FifoOut#(FlowControlReqVec) txFlowCtrlReqVec <- mkDummyFifoOut;
    FifoIn#(FlowControlReqVec) rxFlowCtrlReqVec <- mkDummyFifoIn;
    let xilinxCmacCtrl <- mkXilinxCmacController(
        isEnableRsFec,
        isEnableFlowControl,
        isCmacTxWaitRxAligned,
        axiStream512SyncFifoForCMAC.dstFifoOut,
        axiStream512SyncFifoForCMAC.dstFifoIn,
        txFlowCtrlReqVec,
        rxFlowCtrlReqVec,
        cmacRxReset,
        cmacTxReset,
        clocked_by cmacRxTxClk
    );

    interface xdmaChannel = xdmaWrap.xdmaChannel;
    interface axilRegBlock = xdmaAxiLiteWrap.cntrlAxil;
    interface cmacController = xilinxCmacCtrl;
endmodule


interface RdmaUserLogicWithoutXdmaAndCmacWrapper;
    interface AxiStream512FifoOut axiStreamTxOutUdp;
    interface Put#(AxiStream512)   axiStreamRxInUdp;
    
    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;

    interface Server#(CsrWriteRequest#(CsrAddr, CsrData), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(CsrAddr), CsrReadResponse#(CsrData)) csrReadSrv;

endinterface





interface UdpWrapper;
    interface UdpIpEthBypassRxTx netTxRxIfc;
endinterface

(* synthesize *)
module mkUdpWrapper(UdpWrapper);
    let udpCore <- mkGenericUdpIpEthBypassRxTx(`IS_SUPPORT_RDMA);
    interface netTxRxIfc = udpCore;
endmodule

interface RqWrapper;
    interface UserLogicDmaWriteClt dmaWriteClt;
    interface MrTableQueryClt mrTableQueryClt;
    interface PgtQueryClt pgtQueryClt;
    interface RqDataStreamWithExtraInfoPipeIn rdmaDataStreamInput;
    interface Server#(WriteReqCommonQPC, Bool) qpcWriteCommonSrv;
    interface PipeOut#(RingbufRawDescriptor) packetMetaDescPipeOutRQ;
    interface Put#(RawPacketReceiveMeta) rawPacketReceiveConfigIn;
    interface Put#(Tuple2#(IndexQP, PSN)) setRqExpectedPsnReqIn;
    interface PipeOut#(AutoAckGenMetaData)  autoAckMetaPipeOut;
endinterface


(* synthesize *)
module mkRqWrapper(RqWrapper);

    // TODO try remove this proxy.
    BluerdmaDmaProxyForRQ bluerdmaDmaProxyForRQ <- mkBluerdmaDmaProxyForRQ;

    RingbufStorage#(DATA, InputStreamFragBufferIdx) recvStreamFragStorage <- mkRingbufStorage("recvStreamFragStorage");
    RQ rqCore <- mkRQ;
    let inputRdmaPktBufAndHeaderValidation <- mkInputRdmaPktBufAndHeaderValidation;
    QPContext qpc <- mkQPContext;
    let payloadConsumer <- mkPayloadConsumer;
    RQReportEntryToRingbufDesc reportDescConvertor <- mkRQReportEntryToRingbufDesc;

    FIFOF#(RqDataStreamWithExtraInfo) inputDataStreamQ <- mkFIFOF;

    let rawPacketFakeHeaderInserterPipeout <- mkRawPacketFakeHeaderStreamInsert(toPipeOut(inputDataStreamQ));

    let headerAndMetaDataAndPayloadPipeOut <- mkExtractHeaderFromRdmaPktPipeOut;

    mkConnection(headerAndMetaDataAndPayloadPipeOut.rdmaPktPipeIn, toGet(rawPacketFakeHeaderInserterPipeout.streamPipeOut));

    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData), inputRdmaPktBufAndHeaderValidation.headerMetaDataPipeIn);
    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream), inputRdmaPktBufAndHeaderValidation.headerDataStreamPipeIn);
    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.payloadStreamFragMetaPipeOut), inputRdmaPktBufAndHeaderValidation.payloadStreamFragMetaPipeIn);
    mkConnection(headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageIdxIn, recvStreamFragStorage.allocSlotIdx);
    mkConnection(headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageDataOut, recvStreamFragStorage.saveData);


    // rule debugDropData;
    //     if (headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData.notEmpty) begin
    //         headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData.deq;
    //         $display("time=%0t", $time, "headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData.deq");
    //     end
    //     if (headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream.notEmpty) begin
    //         headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream.deq;
    //         $display("time=%0t", $time, "headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream.deq");
    //     end
    //     if (headerAndMetaDataAndPayloadPipeOut.payloadStreamFragMetaPipeOut.notEmpty) begin
    //         headerAndMetaDataAndPayloadPipeOut.payloadStreamFragMetaPipeOut.deq;
    //         $display("time=%0t", $time, "headerAndMetaDataAndPayloadPipeOut.payloadStreamFragMetaPipeOut.deq");
    //     end
    // endrule

    // rule debugDropFragStorageAndGenFakeResp;
    //     let _ <- headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageInsertClt.request.get;
    //     headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageInsertClt.response.put(unpack(0));
    //     $display("time=%0t", $time, "headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageInsertClt.request.get");
    // endrule

    mkConnection(inputRdmaPktBufAndHeaderValidation.qpcReadCommonClt, qpc.readCommonSrv);
    mkConnection(toGet(inputRdmaPktBufAndHeaderValidation.reqPktPipeOut.pktMetaData), rqCore.pktMetaDataPipeIn);
    mkConnection(toGet(inputRdmaPktBufAndHeaderValidation.reqPktPipeOut.payloadStreamFragMetaPipeOut), payloadConsumer.payloadStreamFragMetaPipeIn);
    
    mkConnection(rqCore.payloadConsumerControlPortClt, payloadConsumer.controlPortSrv);
    mkConnection(payloadConsumer.readFragClt, recvStreamFragStorage.readFragSrv);
    
    mkConnection(toGet(rqCore.pktReportEntryPipeOut), reportDescConvertor.pktReportEntryPipeIn);

    mkConnection(payloadConsumer.dmaWriteClt, bluerdmaDmaProxyForRQ.blueSideWriteSrv);


    interface dmaWriteClt = bluerdmaDmaProxyForRQ.userlogicSideWriteClt;
    interface mrTableQueryClt = rqCore.mrTableQueryClt;
    interface pgtQueryClt = rqCore.pgtQueryClt;
    interface rdmaDataStreamInput = toPut(inputDataStreamQ);
    interface qpcWriteCommonSrv = qpc.writeCommonSrv;
    interface packetMetaDescPipeOutRQ = reportDescConvertor.ringbufDescPipeOut;
    interface rawPacketReceiveConfigIn = rawPacketFakeHeaderInserterPipeout.rawPacketReceiveConfigIn;
    interface setRqExpectedPsnReqIn = rqCore.setRqExpectedPsnReqIn;
    interface autoAckMetaPipeOut = rqCore.autoAckMetaPipeOut;
endmodule


interface QueuePair;
    interface RqWrapper rqIfc;
    interface SQ sqIfc;
endinterface

(* synthesize *)
module mkQueuePair(QueuePair);
    let rq <- mkRqWrapper;
    let sq <- mkSQ;
    interface rqIfc = rq;
    interface sqIfc = sq;
endmodule


interface RdmaUserLogicWithoutXdmaAndUdpCmacWrapper;
    // SQ
    interface PipeOut#(PktInfo4UDP) sqUdpInfoPipeOut;
    interface DataStreamPipeOut     sqRdmaDataStreamPipeOut;
    interface Put#(WorkQueueElem)   autoAckWqePipeIn;

    // RQ
    interface RqDataStreamWithExtraInfoPipeIn rqInputDataStream;
    interface PipeOut#(AutoAckGenMetaData) autoAckMetaPipeOut;
    
    // DMA Controller
    interface UserLogicDmaReadWideClt   dmaReadClt;
    interface UserLogicDmaWriteWideClt  dmaWriteClt;

    // CSR related
    interface Server#(CsrWriteRequest#(CsrAddr, CsrData), CsrWriteResponse)     csrWriteSrv;
    interface Server#(CsrReadRequest#(CsrAddr), CsrReadResponse#(CsrData))      csrReadSrv;

    // UDP config related
    interface Get#(UdpConfig)    setNetworkParamReqOut;

endinterface


(* synthesize *)
// TODO: refactor ringbuf module to get rid of these compiler attributes.
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp" *) 
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp_1" *) 
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp" *) 
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp_1" *) 
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp" *)
// (* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp_1" *) 
(* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp" *)
// (* preempts = "csrBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp_1" *) 
module mkRdmaUserLogicWithoutXdmaAndUdpCmacWrapper(
    Clock dmacClock, 
    Reset dmacReset, 
    RdmaUserLogicWithoutXdmaAndUdpCmacWrapper ifc
);

    FIFOF#(WorkQueueElem)   autoAckWqePipeInQ <- mkFIFOF;

    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;
    RegisterBlock#(CsrAddr, CsrData) csrBlock <- mkRegisterBlock(ringbufPool.h2cMetas, ringbufPool.c2hMetas);
    CommandQueueController cmdQController <- mkCommandQueueController;

    mkConnection(toGet(ringbufPool.h2cRings[0]), cmdQController.ringbufSrv.request);
    rule forwardCmdQResponseToRingbuf;
        let t <- cmdQController.ringbufSrv.response.get;
        ringbufPool.c2hRings[0].enq(t);
    endrule


    let qp <- mkQueuePair;
    mkConnection(cmdQController.setRawPacketReceiveMetaReqOut, qp.rqIfc.rawPacketReceiveConfigIn);
    mkConnection(cmdQController.setRqExpectedPsnReqOut, qp.rqIfc.setRqExpectedPsnReqIn);

    DmaReqAddrTranslator addrTranslatorForSQ <- mkDmaReadReqAddrTranslator;
    function Bool alwaysTrue(anytype t) = True;

    MemRegionTable mrTable <- mkMemRegionTable;

    Vector#(2, MrTableQueryClt)  mrTableQueryCltVec = newVector;
    mrTableQueryCltVec[0] = qp.rqIfc.mrTableQueryClt;
    mrTableQueryCltVec[1] = addrTranslatorForSQ.mrTableClt;
    let mrTableQueryArbitClt <- mkClientArbiter("mrTableQueryArbitClt", False, 10, mrTableQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(mrTable.querySrv, mrTableQueryArbitClt);

    TLB tlb <- mkTLB;
    Vector#(2, PgtQueryClt)  tlbQueryCltVec = newVector;
    tlbQueryCltVec[0] = qp.rqIfc.pgtQueryClt;
    tlbQueryCltVec[1] = addrTranslatorForSQ.addrTransClt;
    let tlbQueryArbitClt <- mkClientArbiter("tlbQueryArbitClt", False, 10, tlbQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(tlb.translateSrv, tlbQueryArbitClt);

    MrAndPgtManager mrAndPgtManager <- mkMrAndPgtManager;
    mkConnection(mrAndPgtManager.mrModifyClt, mrTable.modifySrv);
    mkConnection(mrAndPgtManager.pgtModifyClt, tlb.modifySrv);
    mkConnection(cmdQController.mrAndPgtManagerClt, mrAndPgtManager.mrAndPgtModifyDescSrv);
    mkConnection(cmdQController.qpcModifyClt, qp.rqIfc.qpcWriteCommonSrv);


    WorkQueueRingbufController workQueueRingbufController <- mkWorkQueueRingbufController;
    mkConnection(workQueueRingbufController.sqRingBuf, toGet(ringbufPool.h2cRings[1]));


    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(4, UserLogicDmaReadClt)  dmaAccessH2cCltVec = newVector;
    Vector#(2, UserLogicDmaWriteClt) dmaAccessC2hCltVec = newVector;


    // dmaAccessH2cCltVec[0] <- mkFakeClient;
    dmaAccessH2cCltVec[0] = addrTranslatorForSQ.sqReqOutputClt;
    dmaAccessH2cCltVec[1] = ringbufPool.dmaAccessH2cClt;
    dmaAccessH2cCltVec[2] = mrAndPgtManager.pgtDmaReadClt;
    dmaAccessH2cCltVec[3] <- mkFakeClient;

    dmaAccessC2hCltVec[0] = qp.rqIfc.dmaWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter("xdmaReadClt", False, 10, dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter("xdmaWriteClt", False, 10, dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);
`ifdef IS_250MHZ_512BITS
    XdmaGearbox xdmaGearbox <- mkXdmaBypassGearbox;
`else
    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(dmacClock, dmacReset);
`endif
    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);
    
    mkConnection(qp.sqIfc.dmaReadClt, addrTranslatorForSQ.sqReqInputSrv);

    // rule debug;
    //     if (!qp.sqIfc.sendQ.dataStreamPipeOutSQ.notEmpty) begin
    //         $display("time=%0t: ", $time, "EMPTY_QUEUE_DETECTED: qp.sqIfc.sendQ.dataStreamPipeOutSQ");
    //     end
    //     if (!qp.sqIfc.sendQ.udpInfoPipeOutSQ.notEmpty) begin
    //         $display("time=%0t: ", $time, "EMPTY_QUEUE_DETECTED: qp.sqIfc.sendQ.udpInfoPipeOutSQ");
    //     end 
    // endrule



    rule arbitUserWqeAndAutoAckWqe;
        if (autoAckWqePipeInQ.notEmpty) begin
            autoAckWqePipeInQ.deq;
            qp.sqIfc.sendQ.wqeSrv.request.put(autoAckWqePipeInQ.first);
            $display("time=%0t: ", $time, "arbiter enqueue WQE to SQ, ACK WQE=", fshow(autoAckWqePipeInQ.first));
        end
        else if (workQueueRingbufController.workReq.notEmpty) begin
            workQueueRingbufController.workReq.deq;
            qp.sqIfc.sendQ.wqeSrv.request.put(workQueueRingbufController.workReq.first);
            $display("time=%0t: ", $time, "arbiter enqueue WQE to SQ, User WQE=", fshow(workQueueRingbufController.workReq.first));
        end
    endrule

    rule forwardRecvQueuePktReportDescToRingbuf;
        let t = qp.rqIfc.packetMetaDescPipeOutRQ.first;
        qp.rqIfc.packetMetaDescPipeOutRQ.deq;
        ringbufPool.c2hRings[1].enq(t);
    endrule

    rule forwardSendQueueReportDescToRingbuf;
        let _ <- qp.sqIfc.sendQ.wqeSrv.response.get;
    endrule


    // SQ
    interface sqUdpInfoPipeOut = qp.sqIfc.sendQ.udpInfoPipeOutSQ;
    interface sqRdmaDataStreamPipeOut = qp.sqIfc.sendQ.dataStreamPipeOutSQ;
    interface autoAckWqePipeIn = toPut(autoAckWqePipeInQ);

    // RQ
    interface rqInputDataStream = qp.rqIfc.rdmaDataStreamInput;
    interface autoAckMetaPipeOut = qp.rqIfc.autoAckMetaPipeOut;

    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;
    interface csrWriteSrv = csrBlock.csrWriteSrv;
    interface csrReadSrv = csrBlock.csrReadSrv;

    interface setNetworkParamReqOut = cmdQController.setNetworkParamReqOut;

endmodule

typedef enum {
    UdpReceivingChannelSelectStateIdle          = 0,
    UdpReceivingChannelSelectStateRecvRdmaData  = 1,
    UdpReceivingChannelSelectStateRecvRawData   = 2
} UdpReceivingChannelSelectState deriving(Bits, Eq);

(* synthesize *)
module mkRdmaUserLogicWithoutXdmaAndCmacWrapper(
    Clock udpClock, 
    Reset udpReset, 
    Clock dmacClock, 
    Reset dmacReset, 
    RdmaUserLogicWithoutXdmaAndCmacWrapper ifc
);

    let rdmaClock <- exposeCurrentClock;
    let rdmaReset <- exposeCurrentReset;

    let rdma <- mkRdmaUserLogicWithoutXdmaAndUdpCmacWrapper(dmacClock, dmacReset);
    let udp <- mkUdpWrapper(clocked_by udpClock, reset_by udpReset);

    let udpGearbox <- mkUdpGearbox(rdmaClock, rdmaReset, udpClock, udpReset);

    Reg#(UdpReceivingChannelSelectState)  isReceivingRawPacketReg <- mkReg(UdpReceivingChannelSelectStateIdle);

    RingbufStorage#(RecvPacketSrcMacIpBufferEntry, RecvPacketSrcMacIpBufferIdx) recvMacIpStorage <- mkRingbufStorage("recvMacIpStorage");

    // FIFOF#(Ports::DataStream) udpTxStreamBufQ <- mkFIFOF;
    FIFOF#(UdpIpMetaData) udpTxIpMetaBufQ <- mkFIFOF;
    FIFOF#(MacMetaDataWithBypassTag) udpTxMacMetaBufQ <- mkFIFOF;
    FIFOF#(RqDataStreamWithExtraInfo) udpRxStreamBufQ <- mkFIFOF;

    FIFOF#(AutoAckGenMetaData) sendAutoAckMacIpStorageReadPipeQ <- mkFIFOF;

    FIFOF#(DataStream) rdmaPacketDataStreamRelyQ <- mkFIFOF;
    FIFOF#(DataStream) rawPacketDataStreamRelyQ <- mkFIFOF;



    // Tx Clock transform
    mkConnection(toGet(rdma.sqRdmaDataStreamPipeOut), udpGearbox.txIn);

    Store#(UInt#(0), UdpIpMetaData, 0) txIpMetaSyncFifoStore <- mkRegStore(rdmaClock, udpClock);
    Store#(UInt#(0), MacMetaDataWithBypassTag, 0) txMacMetaSyncFifoStore <- mkRegStore(rdmaClock, udpClock);
    
    ClockDividerIfc divClk <- mkClockDivider(valueOf(TDiv#(UDP_FREQ, RDMA_FREQ)), clocked_by udpClock, reset_by udpReset);

    AlignedFIFO#(UdpIpMetaData) txIpMetaSyncQ <- mkAlignedFIFO(
        rdmaClock, rdmaReset,
        udpClock, udpReset,
        txIpMetaSyncFifoStore,
        True,
        divClk.clockReady
    );

    AlignedFIFO#(MacMetaDataWithBypassTag) txMacMetaSyncQ <- mkAlignedFIFO(
        rdmaClock, rdmaReset,
        udpClock, udpReset,
        txMacMetaSyncFifoStore,
        True,
        divClk.clockReady
    );

    mkConnection(toGet(udpTxIpMetaBufQ), toPut(txIpMetaSyncQ));
    mkConnection(toGet(udpTxMacMetaBufQ), toPut(txMacMetaSyncQ));

    // Rx Clock transform
    mkConnection(toGet(udp.netTxRxIfc.dataStreamRxOut), udpGearbox.rxIn);
    mkConnection(toGet(udp.netTxRxIfc.rawPktStreamRxOut), udpGearbox.rxRawIn);

    Store#(UInt#(0), UdpIpMetaData, 0) rxIpMetaSyncFifoStore <- mkRegStore(udpClock, rdmaClock);
    Store#(UInt#(0), MacMetaData, 0) rxMacMetaSyncFifoStore <- mkRegStore(udpClock, rdmaClock);

    AlignedFIFO#(UdpIpMetaData) rxIpMetaSyncQ <- mkAlignedFIFO(
        udpClock, udpReset,
        rdmaClock, rdmaReset,
        rxIpMetaSyncFifoStore,
        divClk.clockReady,
        True
    );

    AlignedFIFO#(MacMetaData) rxMacMetaSyncQ <- mkAlignedFIFO(
        udpClock, udpReset,
        rdmaClock, rdmaReset,
        rxMacMetaSyncFifoStore,
        divClk.clockReady,
        True
    );

    // Config Clock transform
    Store#(UInt#(0), UdpConfig, 0) udpConfigSyncFifoStore <- mkRegStore(rdmaClock, udpClock);
    AlignedFIFO#(UdpConfig) udpConfigSyncQ <- mkAlignedFIFO(
        rdmaClock, rdmaReset,
        udpClock, udpReset,
        udpConfigSyncFifoStore,
        True,
        divClk.clockReady
    );




    mkConnection(toGet(udp.netTxRxIfc.udpIpMetaDataRxOut), toPut(rxIpMetaSyncQ), clocked_by udpClock, reset_by udpReset);
    mkConnection(toGet(udp.netTxRxIfc.macMetaDataRxOut), toPut(rxMacMetaSyncQ), clocked_by udpClock, reset_by udpReset);

    mkConnection(toGet(udpGearbox.txOut), udp.netTxRxIfc.dataStreamTxIn, clocked_by udpClock, reset_by udpReset);
    mkConnection(toGet(txIpMetaSyncQ), udp.netTxRxIfc.udpIpMetaDataTxIn, clocked_by udpClock, reset_by udpReset);
    mkConnection(toGet(txMacMetaSyncQ), udp.netTxRxIfc.macMetaDataTxIn, clocked_by udpClock, reset_by udpReset);
    mkConnection(toGet(udpRxStreamBufQ), rdma.rqInputDataStream);

    mkConnection(toGet(rdma.setNetworkParamReqOut), toPut(udpConfigSyncQ));
    mkConnection(toGet(udpConfigSyncQ), toPut(udp.netTxRxIfc.udpConfig), clocked_by udpClock, reset_by udpReset);

    // rule forwardTxStream;
    //     rdma.sqRdmaDataStreamPipeOut.deq;
    //     let data = dataStream2DataStreamEn(rdma.sqRdmaDataStreamPipeOut.first);
    //     $display("time=%0t: ", $time,"rdma put data to udp = ", fshow(data));
    //     udpTxStreamBufQ.enq(Ports::DataStream{
    //         data: swapEndian(data.data),
    //         byteEn: swapEndianBit(data.byteEn),
    //         isFirst:    data.isFirst,
    //         isLast:     data.isLast
    //     });
    // endrule

    rule forwardTxMeta;
        rdma.sqUdpInfoPipeOut.deq;
        let meta = rdma.sqUdpInfoPipeOut.first;
        $display("time=%0t: ", $time,"rdma_out_meta = ", fshow(meta));

        IpAddr dstIP = unpack(0);

        if (meta.ipAddr matches tagged IPv4 .ipv4) begin
            dstIP = unpack(pack(ipv4));
        end 
        else begin
            $display("Error: Dest IP addr is not IPv4");
            $finish;
        end

        if (!meta.isRawPkt) begin
            udpTxIpMetaBufQ.enq(UdpIpMetaData{
                dataLen: zeroExtend(meta.pktLen),
                ipAddr:  dstIP,
                ipDscp:  0,
                ipEcn:   0,
                dstPort: fromInteger(valueOf(TEST_UDP_PORT)),
                srcPort: fromInteger(valueOf(TEST_UDP_PORT))
            });
        end

        udpTxMacMetaBufQ.enq(MacMetaDataWithBypassTag{
            macMetaData: MacMetaData{
                macAddr: unpack(pack(meta.macAddr)),
                ethType: fromInteger(valueOf(ETH_TYPE_IP))
            },
            isBypass: meta.isRawPkt
        });

    endrule


    rule forwardRdmaRxStream;
        if (isReceivingRawPacketReg == UdpReceivingChannelSelectStateIdle) begin
            if (udpGearbox.rxOut.notEmpty) begin
                let srcMacIpIdx <- recvMacIpStorage.allocSlotIdx.get;
                rxIpMetaSyncQ.deq;
                rxMacMetaSyncQ.deq;

                recvMacIpStorage.saveData.put(tuple2(srcMacIpIdx, RecvPacketSrcMacIpBufferEntry{
                    ip     : tagged IPv4 unpack(pack(rxIpMetaSyncQ.first.ipAddr)),
                    macAddr: unpack(pack(rxMacMetaSyncQ.first.macAddr))
                }));

                let data = udpGearbox.rxOut.first;
                udpGearbox.rxOut.deq;
                let outData = dataStreamEn2DataStream(DataTypes::DataStreamEn {
                    data: swapEndian(data.data),
                    byteEn: swapEndianBit(data.byteEn),
                    isLast: data.isLast,
                    isFirst: data.isFirst
                });
                udpRxStreamBufQ.enq(tuple3(outData, False, srcMacIpIdx));
                $display("time=%0t: ", $time,"udp put to rqWrapper rdmaData = ", fshow(outData), ", origin data = ", fshow(data));

                if (!data.isLast) begin
                    isReceivingRawPacketReg <= UdpReceivingChannelSelectStateRecvRdmaData;
                end
            end
            else if (udpGearbox.rxOut.notEmpty) begin
                let srcMacIpIdx <- recvMacIpStorage.allocSlotIdx.get;
                recvMacIpStorage.saveData.put(tuple2(srcMacIpIdx, RecvPacketSrcMacIpBufferEntry{
                    ip     : tagged IPv4 unpack(0),
                    macAddr: unpack(0)
                }));

                let data = udpGearbox.rxOut.first;
                udpGearbox.rxOut.deq;
                let outData = dataStreamEn2DataStream(DataTypes::DataStreamEn {
                    data: swapEndian(data.data),
                    byteEn: swapEndianBit(data.byteEn),
                    isLast: data.isLast,
                    isFirst: data.isFirst
                });
                udpRxStreamBufQ.enq(tuple3(outData, True, srcMacIpIdx));
                $display("time=%0t: ", $time,"udp put to rqWrapper rawData = ", fshow(outData), ", origin data = ", fshow(data));

                if (!data.isLast) begin
                    isReceivingRawPacketReg <= UdpReceivingChannelSelectStateRecvRawData;
                end
            end
        end
        else if (isReceivingRawPacketReg == UdpReceivingChannelSelectStateRecvRdmaData) begin
            let data = udpGearbox.rxOut.first;
            udpGearbox.rxOut.deq;
            let outData = dataStreamEn2DataStream(DataTypes::DataStreamEn {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            });
            udpRxStreamBufQ.enq(tuple3(outData, False, ?));
            $display("time=%0t: ", $time,"udp put to rqWrapper rdmaData = ", fshow(outData), ", origin data = ", fshow(data));
            if (data.isLast) begin
                isReceivingRawPacketReg <= UdpReceivingChannelSelectStateIdle;
            end
        end
        else if (isReceivingRawPacketReg == UdpReceivingChannelSelectStateRecvRawData) begin
            let data = udpGearbox.rxOut.first;
            udpGearbox.rxOut.deq;
            let outData = dataStreamEn2DataStream(DataTypes::DataStreamEn {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            });
            udpRxStreamBufQ.enq(tuple3(outData, True, ?));
            $display("time=%0t: ", $time,"udp put to rqWrapper rawData = ", fshow(outData), ", origin data = ", fshow(data));
            if (data.isLast) begin
                isReceivingRawPacketReg <= UdpReceivingChannelSelectStateIdle;
            end
        end
    endrule

    rule sendAutoAckMacIpStorageReadReq;
        let autoAckMeta = rdma.autoAckMetaPipeOut.first;
        rdma.autoAckMetaPipeOut.deq;
        recvMacIpStorage.readFragSrv.request.put(tuple2(autoAckMeta.srcMacIpIdx, False));
        sendAutoAckMacIpStorageReadPipeQ.enq(autoAckMeta);
    endrule

    rule generateAutoAckWqe;
        let macIp <- recvMacIpStorage.readFragSrv.response.get;

        let autoAckMeta = sendAutoAckMacIpStorageReadPipeQ.first;
        sendAutoAckMacIpStorageReadPipeQ.deq;

        ScatterGatherList sgl = unpack(0);
        sgl[0].isFirst = True;
        sgl[0].isLast = True;

        let autoGeneratedWQE = WorkQueueElem {
            pkey: autoAckMeta.pkey,
            opcode: IBV_WR_RDMA_ACK,
            flags: enum2Flag(IBV_SEND_NO_FLAGS),
            qpType: IBV_QPT_RC,
            psn: autoAckMeta.expectedPsn,
            pmtu: IBV_MTU_256,
            dqpIP: macIp.ip,
            macAddr: macIp.macAddr,
            sgl: sgl,
            totalLen: 0,
            raddr: 0,
            rkey: 0,
            sqpn: 0, // TODO: remove it
            dqpn: autoAckMeta.qpn,
            comp: tagged Invalid,
            swap: tagged Invalid,
            immDtOrInvRKey: tagged Invalid,
            srqn: tagged Invalid, // for XRC
            qkey: tagged Invalid, // for UD
            isFirst: True,
            isLast: True
        };
        rdma.autoAckWqePipeIn.put(autoGeneratedWQE);
    endrule
    
   

    interface axiStreamTxOutUdp = udp.netTxRxIfc.axiStreamTxOut;
    interface axiStreamRxInUdp = udp.netTxRxIfc.axiStreamRxIn;


    interface dmaReadClt = rdma.dmaReadClt;
    interface dmaWriteClt = rdma.dmaWriteClt;
    interface csrWriteSrv = rdma.csrWriteSrv;
    interface csrReadSrv = rdma.csrReadSrv;
endmodule

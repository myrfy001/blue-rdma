import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Clocks :: * ;
import Vector :: *;
import PAClib :: *;

import Axi4LiteTypes :: *;
import XilinxCmacController :: *;
import UdpIpArpEthCmacRxTx :: *;
import Ports :: *;
import EthernetTypes :: *;
import SemiFifo :: *;
import StreamHandler :: *;
import XilinxAxiStreamAsyncFifo :: *;
import UdpIpEthCmacRxTx :: *;

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
    Clock slowClock, 
    Reset slowReset, 
    (* osc   = "cmac_rxtx_clk" *) Clock cmacRxTxClk,
    (* reset = "cmac_rx_reset" *) Reset cmacRxReset,
    (* reset = "cmac_tx_reset" *) Reset cmacTxReset,
    BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) ifc
);

    
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by slowClock, reset_by slowReset);
    XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(slowClock, slowReset);
    RdmaUserLogicWithoutXdmaAndCmacWrapper rdmaAndUserlogic <- mkRdmaUserLogicWithoutXdmaAndCmacWrapper(slowClock, slowReset);
    mkConnection(xdmaAxiLiteWrap.csrWriteClt, rdmaAndUserlogic.csrWriteSrv);
    mkConnection(xdmaAxiLiteWrap.csrReadClt, rdmaAndUserlogic.csrReadSrv);
    mkConnection(xdmaWrap.dmaReadSrv, rdmaAndUserlogic.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, rdmaAndUserlogic.dmaWriteClt);

    let udpClk <- exposeCurrentClock;
    let udpReset <- exposeCurrentReset;


    Bool isCmacTxWaitRxAligned = True;
    Bool isEnableFlowControl = False;
    Bool isEnableRsFec = True;

    let axiStream512RxIn <- mkPutToFifoIn(rdmaAndUserlogic.axiStreamRxInUdp);

    let axiStream512SyncFifoForCMAC <- mkDuplexAxiStreamAsyncFifo(
        valueOf(CMAC_SYNC_BRAM_BUF_DEPTH),
        valueOf(CMAC_CDC_SYNC_STAGE),
        udpClk,
        udpReset,
        cmacRxTxClk,
        cmacRxReset,
        cmacTxReset,
        axiStream512RxIn,
        rdmaAndUserlogic.axiStreamTxOutUdp
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


interface RdmaUserLogicWithoutXdmaAndUdpCmacWrapper;
    // SQ
    interface PipeOut#(PktInfo4UDP) sqUdpInfoPipeOut;
    interface DataStreamPipeOut     sqRdmaDataStreamPipeOut;

    // RQ
    interface RqDataStreamWithRawPacketFlagPipeIn rqInputDataStream;
    
    // DMA Controller
    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;

    // CSR related
    interface Server#(CsrWriteRequest#(CsrAddr, CsrData), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(CsrAddr), CsrReadResponse#(CsrData)) csrReadSrv;

endinterface


(* synthesize *)
module mkUdpWrapper(UdpIpEthBypassRxTx);
    let t <- mkGenericUdpIpEthBypassRxTx(`IS_SUPPORT_RDMA);
    return t;
endmodule

interface RqWrapper;
    interface UserLogicDmaWriteClt dmaWriteClt;
    interface MrTableQueryClt mrTableQueryClt;
    interface PgtQueryClt pgtQueryClt;
    interface RqDataStreamWithRawPacketFlagPipeIn inputDataStream;
    interface Server#(WriteReqCommonQPC, Bool) qpcWriteCommonSrv;
    interface PipeOut#(RingbufRawDescriptor) packetMetaDescPipeOut;
endinterface


(* synthesize *)
module mkRqWrapper(RqWrapper);

    // TODO try remove this proxy.
    BluerdmaDmaProxyForRQ bluerdmaDmaProxyForRQ <- mkBluerdmaDmaProxyForRQ;

    ReceivedStreamFragStorage recvStreamFragStorage <- mkReceivedStreamFragStorage;
    RQ rqCore <- mkRQ;
    let inputRdmaPktBufAndHeaderValidation <- mkInputRdmaPktBufAndHeaderValidation;
    QPContext qpc <- mkQPContext;
    let payloadConsumer <- mkPayloadConsumer;
    RQReportEntryToRingbufDesc reportDescConvertor <- mkRQReportEntryToRingbufDesc;

    FIFOF#(Tuple2#(DataTypes::DataStream,Bool)) inputDataStreamQ <- mkFIFOF;

    let headerAndMetaDataAndPayloadPipeOut <- mkExtractHeaderFromRdmaPktPipeOut;

    mkConnection(headerAndMetaDataAndPayloadPipeOut.rdmaPktPipeIn, toGet(inputDataStreamQ));

    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerMetaData), inputRdmaPktBufAndHeaderValidation.headerMetaDataPipeIn);
    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.headerAndMetaData.headerDataStream), inputRdmaPktBufAndHeaderValidation.headerDataStreamPipeIn);
    mkConnection(toGet(headerAndMetaDataAndPayloadPipeOut.payloadStreamFragMetaPipeOut), inputRdmaPktBufAndHeaderValidation.payloadStreamFragMetaPipeIn);
    mkConnection(headerAndMetaDataAndPayloadPipeOut.payloadStreamFragStorageInsertClt, recvStreamFragStorage.insertFragSrv);

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
    
    mkConnection(rqCore.payloadXonsumerControlPortClt, payloadConsumer.controlPortSrv);
    mkConnection(payloadConsumer.readFragClt, recvStreamFragStorage.readFragSrv);
    
    mkConnection(toGet(rqCore.pktReportEntryPipeOut), reportDescConvertor.pktReportEntryPipeIn);

    mkConnection(payloadConsumer.dmaWriteClt, bluerdmaDmaProxyForRQ.blueSideWriteSrv);


    interface dmaWriteClt = bluerdmaDmaProxyForRQ.userlogicSideWriteClt;
    interface mrTableQueryClt = rqCore.mrTableQueryClt;
    interface pgtQueryClt = rqCore.pgtQueryClt;
    interface inputDataStream = toPut(inputDataStreamQ);
    interface qpcWriteCommonSrv = qpc.writeCommonSrv;
    interface packetMetaDescPipeOut = reportDescConvertor.ringbufDescPipeOut;
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
    Clock slowClock, 
    Reset slowReset, 
    RdmaUserLogicWithoutXdmaAndUdpCmacWrapper ifc
);

    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;
    RegisterBlock#(CsrAddr, CsrData) csrBlock <- mkRegisterBlock(ringbufPool.h2cMetas, ringbufPool.c2hMetas);
    CommandQueueController cmdQController <- mkCommandQueueController;

    mkConnection(toGet(ringbufPool.h2cRings[0]), cmdQController.ringbufSrv.request);
    rule forwardCmdQResponseToRingbuf;
        let t <- cmdQController.ringbufSrv.response.get;
        ringbufPool.c2hRings[0].enq(t);
    endrule


    let queuePair <- mkQueuePair;


    DmaReqAddrTranslator addrTranslatorForSQ <- mkDmaReadReqAddrTranslator;
    function Bool alwaysTrue(anytype t) = True;

    MemRegionTable mrTable <- mkMemRegionTable;

    Vector#(2, MrTableQueryClt)  mrTableQueryCltVec = newVector;
    mrTableQueryCltVec[0] = queuePair.rqIfc.mrTableQueryClt;
    mrTableQueryCltVec[1] = addrTranslatorForSQ.mrTableClt;
    let mrTableQueryArbitClt <- mkClientArbiter("mrTableQueryArbitClt", False, 10, mrTableQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(mrTable.querySrv, mrTableQueryArbitClt);

    TLB tlb <- mkTLB;
    Vector#(2, PgtQueryClt)  tlbQueryCltVec = newVector;
    tlbQueryCltVec[0] = queuePair.rqIfc.pgtQueryClt;
    tlbQueryCltVec[1] = addrTranslatorForSQ.addrTransClt;
    let tlbQueryArbitClt <- mkClientArbiter("tlbQueryArbitClt", False, 10, tlbQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(tlb.translateSrv, tlbQueryArbitClt);

    MrAndPgtManager mrAndPgtManager <- mkMrAndPgtManager;
    mkConnection(mrAndPgtManager.mrModifyClt, mrTable.modifySrv);
    mkConnection(mrAndPgtManager.pgtModifyClt, tlb.modifySrv);
    mkConnection(cmdQController.mrAndPgtManagerClt, mrAndPgtManager.mrAndPgtModifyDescSrv);
    mkConnection(cmdQController.qpcModifyClt, queuePair.rqIfc.qpcWriteCommonSrv);


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

    dmaAccessC2hCltVec[0] = queuePair.rqIfc.dmaWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter("xdmaReadClt", False, 10, dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter("xdmaWriteClt", True, 10, dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);

    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(slowClock, slowReset);

    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);
    
    mkConnection(queuePair.sqIfc.dmaReadClt, addrTranslatorForSQ.sqReqInputSrv);
    mkConnection(workQueueRingbufController.workReq, queuePair.sqIfc.sendQ.srvPort.request);

    // rule debug;
    //     if (!queuePair.sqIfc.sendQ.rdmaDataStreamPipeOut.notEmpty) begin
    //         $display("time=%0t: ", $time, "EMPTY_QUEUE_DETECTED: queuePair.sqIfc.sendQ.rdmaDataStreamPipeOut");
    //     end
    //     if (!queuePair.sqIfc.sendQ.udpInfoPipeOut.notEmpty) begin
    //         $display("time=%0t: ", $time, "EMPTY_QUEUE_DETECTED: queuePair.sqIfc.sendQ.udpInfoPipeOut");
    //     end 
    // endrule


    rule forwardRecvQueuePktReportDescToRingbuf;
        let t = queuePair.rqIfc.packetMetaDescPipeOut.first;
        queuePair.rqIfc.packetMetaDescPipeOut.deq;
        ringbufPool.c2hRings[1].enq(t);
    endrule

    rule forwardSendQueueReportDescToRingbuf;
        let _ <- queuePair.sqIfc.sendQ.srvPort.response.get;
    endrule


    // SQ
    interface sqUdpInfoPipeOut = queuePair.sqIfc.sendQ.udpInfoPipeOut;
    interface sqRdmaDataStreamPipeOut = queuePair.sqIfc.sendQ.rdmaDataStreamPipeOut;

    // RQ
    interface rqInputDataStream = queuePair.rqIfc.inputDataStream;


    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;
    interface csrWriteSrv = csrBlock.csrWriteSrv;
    interface csrReadSrv = csrBlock.csrReadSrv;

endmodule

typedef enum {
    UdpReceivingChannelSelectStateIdle          = 0,
    UdpReceivingChannelSelectStateRecvRdmaData  = 1,
    UdpReceivingChannelSelectStateRecvRawData   = 2
} UdpReceivingChannelSelectState deriving(Bits, Eq);

(* synthesize *)
module mkRdmaUserLogicWithoutXdmaAndCmacWrapper(
    Clock slowClock, 
    Reset slowReset, 
    RdmaUserLogicWithoutXdmaAndCmacWrapper ifc
);

    let userLogic <- mkRdmaUserLogicWithoutXdmaAndUdpCmacWrapper(slowClock, slowReset);
    let udp <- mkUdpWrapper;

    Reg#(Bool)  udpParamNotSetReg <- mkReg(True);
    Reg#(UdpReceivingChannelSelectState)  isReceivingRawPacketReg <- mkReg(UdpReceivingChannelSelectStateIdle);


    FIFOF#(Ports::DataStream) udpTxStreamBufQ <- mkFIFOF;
    FIFOF#(UdpIpMetaData) udpTxIpMetaBufQ <- mkFIFOF;
    FIFOF#(MacMetaDataWithBypassTag) udpTxMacMetaBufQ <- mkFIFOF;
    FIFOF#(Tuple2#(DataTypes::DataStream, Bool)) udpRxStreamBufQ <- mkFIFOF;

    mkConnection(toGet(udpTxStreamBufQ), udp.dataStreamTxIn);
    mkConnection(toGet(udpTxIpMetaBufQ), udp.udpIpMetaDataTxIn);
    mkConnection(toGet(udpTxMacMetaBufQ), udp.macMetaDataTxIn);
    mkConnection(toGet(udpRxStreamBufQ), userLogic.rqInputDataStream);


    rule setInitParamUDP if (udpParamNotSetReg);
        udp.udpConfig.put(UdpConfig{
            macAddr: 'hAABBCCDDEEFF,
            ipAddr: 'h11223344,
            netMask: 32'hFFFFFFFF,
            gateWay: 1
        });
        udpParamNotSetReg <= False;
    endrule

    rule forawrdTxStream;
        userLogic.sqRdmaDataStreamPipeOut.deq;
        let data = userLogic.sqRdmaDataStreamPipeOut.first;
        $display("time=%0t: ", $time,"rdma put data to udp = ", fshow(data));
        udpTxStreamBufQ.enq(Ports::DataStream{
            data: swapEndian(data.data),
            byteEn: swapEndianBit(data.byteEn),
            isFirst:    data.isFirst,
            isLast:     data.isLast
        });
    endrule

    rule forwardTxMeta;
        userLogic.sqUdpInfoPipeOut.deq;
        let meta = userLogic.sqUdpInfoPipeOut.first;
        $display("time=%0t: ", $time,"rdma_out_meta = ", fshow(meta));

        IpAddr dstIP = unpack(0);

        if (meta.ipAddr matches tagged IPv4 .ipv4) begin
            dstIP = unpack(pack(ipv4));
        end 
        else begin
            $display("Error: Dest IP addr is not IPv4");
            $finish;
        end

        udpTxIpMetaBufQ.enq(UdpIpMetaData{
            dataLen: zeroExtend(meta.pktLen),
            ipAddr:  dstIP,
            ipDscp:  0,
            ipEcn:   0,
            dstPort: fromInteger(valueOf(TEST_UDP_PORT)),
            srcPort: fromInteger(valueOf(TEST_UDP_PORT))
        });
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
            if (udp.dataStreamRxOut.notEmpty) begin
                udp.udpIpMetaDataRxOut.deq;
                udp.macMetaDataRxOut.deq;
                let data = udp.dataStreamRxOut.first;
                udp.dataStreamRxOut.deq;
                let outData = DataTypes::DataStream {
                    data: swapEndian(data.data),
                    byteEn: swapEndianBit(data.byteEn),
                    isLast: data.isLast,
                    isFirst: data.isFirst
                };
                udpRxStreamBufQ.enq(tuple2(outData, False));
                $display("time=%0t: ", $time,"udp put to rqWrapper rdmaData = ", fshow(outData));

                if (!data.isLast) begin
                    isReceivingRawPacketReg <= UdpReceivingChannelSelectStateRecvRdmaData;
                end
            end
            else if (udp.rawPktStreamRxOut.notEmpty) begin

                let data = udp.rawPktStreamRxOut.first;
                udp.rawPktStreamRxOut.deq;
                let outData = DataTypes::DataStream {
                    data: swapEndian(data.data),
                    byteEn: swapEndianBit(data.byteEn),
                    isLast: data.isLast,
                    isFirst: data.isFirst
                };
                udpRxStreamBufQ.enq(tuple2(outData, True));
                $display("time=%0t: ", $time,"udp put to rqWrapper rawData = ", fshow(outData));

                if (!data.isLast) begin
                    isReceivingRawPacketReg <= UdpReceivingChannelSelectStateRecvRawData;
                end
            end
        end
        else if (isReceivingRawPacketReg == UdpReceivingChannelSelectStateRecvRdmaData) begin
            let data = udp.dataStreamRxOut.first;
            udp.dataStreamRxOut.deq;
            let outData = DataTypes::DataStream {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            };
            udpRxStreamBufQ.enq(tuple2(outData, False));
            $display("time=%0t: ", $time,"udp put to rqWrapper rdmaData = ", fshow(outData));
            if (data.isLast) begin
                isReceivingRawPacketReg <= UdpReceivingChannelSelectStateIdle;
            end
        end
        else if (isReceivingRawPacketReg == UdpReceivingChannelSelectStateRecvRawData) begin
            let data = udp.rawPktStreamRxOut.first;
            udp.rawPktStreamRxOut.deq;
            let outData = DataTypes::DataStream {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            };
            udpRxStreamBufQ.enq(tuple2(outData, True));
            $display("time=%0t: ", $time,"udp put to rqWrapper rawData = ", fshow(outData));
            if (data.isLast) begin
                isReceivingRawPacketReg <= UdpReceivingChannelSelectStateIdle;
            end
        end
    endrule
    
   

    interface axiStreamTxOutUdp = udp.axiStreamTxOut;
    interface axiStreamRxInUdp = udp.axiStreamRxIn;


    interface dmaReadClt = userLogic.dmaReadClt;
    interface dmaWriteClt = userLogic.dmaWriteClt;
    interface csrWriteSrv = userLogic.csrWriteSrv;
    interface csrReadSrv = userLogic.csrReadSrv;
endmodule

function Bit#(width) swapEndian(Bit#(width) data) provisos(Mul#(8, byteNum, width));
    Vector#(byteNum, Bit#(BYTE_WIDTH)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

function Bit#(width) swapEndianBit(Bit#(width) data) provisos(Mul#(1, byteNum, width));
    Vector#(byteNum, Bit#(1)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

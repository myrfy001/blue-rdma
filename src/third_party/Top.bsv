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

import PipeIn :: *;
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

typedef 4791 TEST_UDP_PORT;

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
    TopCoreRdma bsvTopCore <- mkTopCore(slowClock, slowReset);
    mkConnection(xdmaAxiLiteWrap.csrWriteClt, bsvTopCore.csrWriteSrv);
    mkConnection(xdmaAxiLiteWrap.csrReadClt, bsvTopCore.csrReadSrv);
    mkConnection(xdmaWrap.dmaReadSrv, bsvTopCore.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, bsvTopCore.dmaWriteClt);

    let udpClk <- exposeCurrentClock;
    let udpReset <- exposeCurrentReset;


    Bool isCmacTxWaitRxAligned = True;
    Integer syncBramBufDepth = 32;
    Integer cdcSyncStages = 4;
    let isEnableFlowControl = False;
    Bool isEnableRsFec = True;

    let axiStream512TxOut <- mkDoubleAxiStreamPipeOut(bsvTopCore.axiStreamOutUdp);
    let axiStreamRxIn <- mkPutToPipeIn(bsvTopCore.axiStreamInUdp);
    let axiStream512RxIn <- mkDoubleAxiStreamPipeIn(axiStreamRxIn);

    let axiStream512Sync <- mkDuplexAxiStreamAsyncFifo(
        syncBramBufDepth,
        cdcSyncStages,
        udpClk,
        udpReset,
        cmacRxTxClk,
        cmacRxReset,
        cmacTxReset,
        axiStream512RxIn,
        axiStream512TxOut
    );


    SemiFifo::PipeOut#(FlowControlReqVec) txFlowCtrlReqVec <- mkDummyPipeOut;
    SemiFifo::PipeIn#(FlowControlReqVec) rxFlowCtrlReqVec <- mkDummyPipeIn;
    let xilinxCmacCtrl <- mkXilinxCmacController(
        isEnableRsFec,
        isEnableFlowControl,
        isCmacTxWaitRxAligned,
        axiStream512Sync.dstPipeOut,
        axiStream512Sync.dstPipeIn,
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


interface TopCoreRdma;
    interface AxiStream256PipeOut axiStreamOutUdp;
    interface Put#(AxiStream256)   axiStreamInUdp;
    
    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;

    interface Server#(CsrWriteRequest#(CsrAddr, CsrData), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(CsrAddr), CsrReadResponse#(CsrData)) csrReadSrv;

endinterface



(* synthesize *)
// TODO: refactor ringbuf module to get rid of these compiler attributes.
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp" *)
// (* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_2.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp" *)
// (* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_3.recvDmaResp_1" *) 
module mkTopCore(
    Clock slowClock, 
    Reset slowReset, 
    TopCoreRdma ifc
);

    // TODO try remove this proxy.
    BluerdmaDmaProxyForRQ bluerdmaDmaProxyForRQ <- mkBluerdmaDmaProxyForRQ;

    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;
    RegisterBlock#(CsrAddr, CsrData) regBlock <- mkRegisterBlock(ringbufPool.h2cMetas, ringbufPool.c2hMetas);
    CommandQueueController cmdQController <- mkCommandQueueController;

    mkConnection(toGet(ringbufPool.h2cRings[0]), cmdQController.ringbufSrv.request);
    rule forwardCmdQResponseToRingbuf;
        let t <- cmdQController.ringbufSrv.response.get;
        ringbufPool.c2hRings[0].enq(t);
    endrule

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


    RQReportEntryToRingbufDesc reportDescConvertor <- mkRQReportEntryToRingbufDesc;

    mkConnection(rq.pktReportEntryPipeOut, reportDescConvertor.pktReportEntryPipeIn);

    let payloadConsumer <- mkPayloadConsumer;

    mkConnection(inputRdmaPktBufAndHeaderValidation.reqPktPipeOut.payload, payloadConsumer.payloadPipeIn);
    mkConnection(rq.payloadXonsumerControlPortClt, payloadConsumer.controlPortSrv);

    DmaReqAddrTranslator addrTranslatorForSQ <- mkDmaReadReqAddrTranslator;

    function Bool alwaysTrue(anytype t) = True;

    MemRegionTable mrTable <- mkMemRegionTable;

    Vector#(2, Client#(MrTableQueryReq, Maybe#(MemRegionTableEntry)))  mrTableQueryCltVec = newVector;
    mrTableQueryCltVec[0] = rq.mrTableQueryClt;
    mrTableQueryCltVec[1] = addrTranslatorForSQ.mrTableClt;
    let mrTableQueryArbitClt <- mkClientArbiter(mrTableQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(mrTable.querySrv, mrTableQueryArbitClt);

    TLB tlb <- mkTLB;
    Vector#(2, Client#(PgtAddrTranslateReq, ADDR))  tlbQueryCltVec = newVector;
    tlbQueryCltVec[0] = rq.pgtQueryClt;
    tlbQueryCltVec[1] = addrTranslatorForSQ.addrTransClt;
    let tlbQueryArbitClt <- mkClientArbiter(tlbQueryCltVec, alwaysTrue, alwaysTrue);
    mkConnection(tlb.translateSrv, tlbQueryArbitClt);

    MrAndPgtManager mrAndPgtManager <- mkMrAndPgtManager;
    mkConnection(mrAndPgtManager.mrModifyClt, mrTable.modifySrv);
    mkConnection(mrAndPgtManager.pgtModifyClt, tlb.modifySrv);
    mkConnection(cmdQController.mrAndPgtManagerClt, mrAndPgtManager.mrAndPgtModifyDescSrv);
    mkConnection(cmdQController.qpcModifyClt, qpc.writeCommonSrv);


    WorkQueueRingbufController workQueueRingbufController <- mkWorkQueueRingbufController;


    mkConnection(workQueueRingbufController.sqRingBuf, toGet(ringbufPool.h2cRings[1]));



    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(4, UserLogicDmaReadClt)  dmaAccessH2cCltVec = newVector;
    Vector#(2, UserLogicDmaWriteClt) dmaAccessC2hCltVec = newVector;

    dmaAccessH2cCltVec[0] = addrTranslatorForSQ.sqReqOutputClt;
    dmaAccessH2cCltVec[1] = ringbufPool.dmaAccessH2cClt;
    dmaAccessH2cCltVec[2] = mrAndPgtManager.pgtDmaReadClt;
    dmaAccessH2cCltVec[3] <- mkFakeClient;

    dmaAccessC2hCltVec[0] = bluerdmaDmaProxyForRQ.userlogicSideWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter(dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter(dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);

    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(slowClock, slowReset);

    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);

    mkConnection(payloadConsumer.dmaWriteClt, bluerdmaDmaProxyForRQ.blueSideWriteSrv);


    let sq <- mkSQ();
    mkConnection(sq.dmaReadClt, addrTranslatorForSQ.sqReqInputSrv);
    mkConnection(workQueueRingbufController.workReq, sq.sendQ.srvPort.request);



    let udp <- mkGenericUdpIpEthRxTx(`IS_SUPPORT_RDMA);

    Reg#(Bool)  udpParamNotSetReg <- mkReg(True);

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
        sq.sendQ.rdmaDataStreamPipeOut.deq;
        let data = sq.sendQ.rdmaDataStreamPipeOut.first;
        $display("rdma_A_out_data = ", fshow(data));
        udp.dataStreamIn.put(Ports::DataStream{
            data: swapEndian(data.data),
            byteEn: swapEndianBit(data.byteEn),
            isFirst:    data.isFirst,
            isLast:     data.isLast
        });
    endrule

    rule forwardTxMeta;
        sq.sendQ.udpInfoPipeOut.deq;
        let meta = sq.sendQ.udpInfoPipeOut.first;
        $display("rdma_A_out_meta = ", fshow(meta));

        IpAddr dstIP = unpack(0);

        if (meta.ipAddr matches tagged IPv4 .ipv4) begin
            dstIP = unpack(pack(ipv4));
        end 
        else begin
            $display("Error: Dest IP addr is not IPv4");
            $finish;
        end

        udp.udpIpMetaDataIn.put(UdpIpMetaData{
            dataLen: zeroExtend(meta.pktLen),
            ipAddr:  dstIP,
            ipDscp:  0,
            ipEcn:   0,
            dstPort: fromInteger(valueOf(TEST_UDP_PORT)),
            srcPort: fromInteger(valueOf(TEST_UDP_PORT))
        });
        udp.macMetaDataIn.put(MacMetaData{
            macAddr: unpack(pack(meta.macAddr)),
            ethType: fromInteger(valueOf(ETH_TYPE_IP))
        });

    endrule

    rule forwardRxStream;

        if (udp.udpIpMetaDataOut.notEmpty) begin
            udp.udpIpMetaDataOut.deq;
            $display("udp recv meta = ", fshow(udp.udpIpMetaDataOut.first));
        end

        if (udp.macMetaDataOut.notEmpty) begin
            udp.macMetaDataOut.deq;
            $display("udp recv mac meta = ", fshow(udp.macMetaDataOut.first));
        end

        if (udp.dataStreamOut.notEmpty) begin
            let data = udp.dataStreamOut.first;
            udp.dataStreamOut.deq;

            let outData = DataStream {
                data: swapEndian(data.data),
                byteEn: swapEndianBit(data.byteEn),
                isLast: data.isLast,
                isFirst: data.isFirst
            };
            inputDataStreamQ.enq(outData);
            $display("udp recv = ", fshow(outData));
        end
    endrule



    // use descending_urgency here since we need a simple fix-priority arbitter here.
    (* descending_urgency = "forwardRecvQueuePktReportDescToRingbuf, forwardSendQueueReportDescToRingbuf" *)
    rule forwardRecvQueuePktReportDescToRingbuf;
        let t = reportDescConvertor.ringbufDescPipeOut.first;
        reportDescConvertor.ringbufDescPipeOut.deq;
        ringbufPool.c2hRings[1].enq(t);
    endrule

    rule forwardSendQueueReportDescToRingbuf;
        let sendResp <- sq.sendQ.srvPort.response.get;
        let desc = MeatReportQueueDescSendQueueReport {
            reserved1:      unpack(0),
            hasDmaRespErr:  False,             
            reserved2:      unpack(0),
            descType:       MeatReportQueueDescTypeSendFinished
        };
        ringbufPool.c2hRings[1].enq(pack(desc));
    endrule

    interface axiStreamOutUdp = udp.axiStreamOut;
    interface axiStreamInUdp = udp.axiStreamIn;


    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;
    interface csrWriteSrv = regBlock.csrWriteSrv;
    interface csrReadSrv = regBlock.csrReadSrv;
endmodule

// (* synthesize *)
module mkFakeSQ(SQ);
    FIFOF#(DmaReadReq)   dmaReadReqQ <- mkFIFOF;
    FIFOF#(DmaReadResp) dmaReadRespQ <- mkFIFOF;

    FIFOF#(WorkQueueElem)       wqeQ <- mkFIFOF;
    FIFOF#(SendResp)       sendRespQ <- mkFIFOF;

    FIFOF#(PktInfo4UDP)       udpQ <- mkFIFOF;
    FIFOF#(DataStream)       dataStreamQ <- mkFIFOF;


    Reg#(Bit#(1024)) tReg <- mkRegU;

    rule incReg;
        tReg <= tReg + 1;
    endrule

    rule beat;
        dmaReadReqQ.enq(unpack(truncate(tReg)));
        wqeQ.deq;
        dmaReadRespQ.deq;
        sendRespQ.enq(unpack(truncate(tReg)));
        udpQ.enq(unpack(truncate(tReg)));
        dataStreamQ.enq(unpack(truncate(tReg)));
    endrule

    interface dmaReadClt = toGPClient(dmaReadReqQ, dmaReadRespQ);
    interface SendQ sendQ;
        interface srvPort = toGPServer(wqeQ, sendRespQ);
        interface udpInfoPipeOut = toPipeOut(udpQ);
        interface rdmaDataStreamPipeOut = toPipeOut(dataStreamQ);
        method Bool isEmpty() = unpack(tReg[0]);
    endinterface
    method Action clearAll();
    endmethod
endmodule

function Bit#(width) swapEndian(Bit#(width) data) provisos(Mul#(8, byteNum, width));
    Vector#(byteNum, Bit#(BYTE_WIDTH)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

function Bit#(width) swapEndianBit(Bit#(width) data) provisos(Mul#(1, byteNum, width));
    Vector#(byteNum, Bit#(1)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

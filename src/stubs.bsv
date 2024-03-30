import FIFOF :: *;
import GetPut :: *;

import ClientServer :: *;
import RdmaUtils :: *;
import MetaData :: *;
import DataTypes :: *;
import Headers :: *;
import PAClib :: *;
import PrimUtils :: *;
import QPContext :: *;

import UserLogicTypes :: *;
import RQ :: *;
import SendQ :: *;
import PayloadGen :: *;
import Top :: *;

import Ports :: *;
import UdpIpEthCmacRxTx :: *;
import SemiFifo :: *;


(* synthesize *)
module mkStubTop(Empty);
    mkRQ;
    mkSQ;
    mkUdpWrapper;
endmodule

(* synthesize *)
module mkRQ(RQ ifc);
    FIFOF#(RdmaPktMetaDataAndQPC)     rdmaPktMetaDataInQ  <- mkFIFOF;
    FIFOF#(C2hReportEntry)              pktReportEntryQ     <- mkFIFOF;

    BypassClient#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) mrTableQueryCltInst  <- mkBypassClient("mrTableQueryCltInst in RQ");
    BypassClient#(PgtAddrTranslateReq, ADDR) pgtQueryCltInst                         <- mkBypassClient("RQ pgtQueryCltInst");
    BypassClient#(PayloadConReq, PayloadConResp) payloadConsumerControlClt           <- mkBypassClient("payloadConsumerControlClt");
    

    interface pktMetaDataPipeIn             = toPut(rdmaPktMetaDataInQ);
    interface mrTableQueryClt               = mrTableQueryCltInst.clt;
    interface pgtQueryClt                   = pgtQueryCltInst.clt;
    interface payloadConsumerControlPortClt = payloadConsumerControlClt.clt;
    interface pktReportEntryPipeOut         = toPipeOut(pktReportEntryQ);
endmodule


(* synthesize *)
module mkSQ(SQ);
    FIFOF#(DmaReadReq)   dmaReadReqQ <- mkFIFOF;
    FIFOF#(DmaReadResp) dmaReadRespQ <- mkFIFOF;

    FIFOF#(WorkQueueElem) wqeSrvReqQ <- mkFIFOF;
    FIFOF#(SendResp) wqeSrvRespQ <- mkFIFOF;

    FIFOF#(PktInfo4UDP) udpInfoPipeOutQ <- mkFIFOF;
    FIFOF#(DataTypes::DataStream) dataStreamPipeOutQ <- mkFIFOF;



    // force power-up reset
    Reg#(Bool) clearReg <- mkReg(True);

    rule resetAndClear if (clearReg);
        clearReg <= False;
    endrule

    interface dmaReadClt = toGPClient(dmaReadReqQ, dmaReadRespQ);

    interface SendQ sendQ;
        interface wqeSrv = toGPServer(wqeSrvReqQ, wqeSrvRespQ);
        interface udpInfoPipeOutSQ = toPipeOut(udpInfoPipeOutQ);
        interface dataStreamPipeOutSQ = toPipeOut(dataStreamPipeOutQ);
        method Bool isEmpty() = clearReg;
    endinterface

    method Action clearAll if (!clearReg);
        clearReg <= True;
    endmethod
endmodule


(* synthesize *)
module mkUdpWrapper(UdpWrapper);
    FIFOF#(UdpConfig) udpConfigQ <- mkFIFOF;
    FIFOF#(MacMetaDataWithBypassTag) macMetaDataTxInQ <- mkFIFOF;
    FIFOF#(UdpIpMetaData) udpIpMetaDataTxInQ <- mkFIFOF;
    FIFOF#(Ports::DataStream) dataStreamTxInQ <- mkFIFOF;
    FIFOF#(AxiStream512) axiStreamTxOutQ <- mkFIFOF;

    FIFOF#(AxiStream512) axiStreamRxInQ <- mkFIFOF;
    FIFOF#(MacMetaData) macMetaDataRxOutQ <- mkFIFOF;
    FIFOF#(UdpIpMetaData) udpIpMetaDataRxOutQ <- mkFIFOF;
    FIFOF#(Ports::DataStream) dataStreamRxOutQ <- mkFIFOF;
    FIFOF#(Ports::DataStream) rawPktStreamRxOutQ <- mkFIFOF;

    interface UdpIpEthBypassRxTx netTxRxIfc;
        interface udpConfig = toPut(udpConfigQ);
        // Tx Channel
        interface macMetaDataTxIn = toPut(macMetaDataTxInQ);
        interface udpIpMetaDataTxIn = toPut(udpIpMetaDataTxInQ);
        interface dataStreamTxIn = toPut(dataStreamTxInQ);
        interface axiStreamTxOut = convertFifoToFifoOut(axiStreamTxOutQ);
        
        // Rx Channel
        interface axiStreamRxIn = toPut(axiStreamRxInQ);
        interface macMetaDataRxOut = convertFifoToFifoOut(macMetaDataRxOutQ);
        interface udpIpMetaDataRxOut = convertFifoToFifoOut(udpIpMetaDataRxOutQ);
        interface dataStreamRxOut = convertFifoToFifoOut(dataStreamRxOutQ);
        interface rawPktStreamRxOut = convertFifoToFifoOut(rawPktStreamRxOutQ);
    endinterface
endmodule
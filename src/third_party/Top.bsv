import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;

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


interface TopCoreIfc;
    interface Put#(DataStream) rdmaDataStreamInput;
    interface DmaWriteClt dmaWriteClt;

    interface Server#(QPCWriteReqCommon, Bool) qpcWriteCommonSrv;
    interface Server#(MrTableModifyReq, MrTableModifyResp) mrModifySrv;
    interface Server#(PgtModifyReq, PgtModifyResp) pgtModifySrv;

endinterface

(* synthesize *)
module mkTopCore(TopCoreIfc);
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

    interface rdmaDataStreamInput = toPut(inputDataStreamQ);
    interface dmaWriteClt = payloadConsumer.dmaWriteClt;

    interface qpcWriteCommonSrv = qpc.writeCommonSrv;
    interface mrModifySrv = mrTable.modifySrv;
    interface pgtModifySrv = tlb.modifySrv;

endmodule


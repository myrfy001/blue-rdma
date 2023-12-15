import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Vector :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import UserLogicUtils :: *;
import UserLogicTypes :: *;
import MetaData :: *;
import PrimUtils :: *;
import Controller :: *;
import Ringbuf :: *;

interface CommandQueueController;
    interface Server#(RingbufRawDescriptor, RingbufRawDescriptor) ringbufSrv;
    interface Client#(RingbufRawDescriptor, Bool) pgtManagerClt;
    interface Client#(MetaDataReq, MetaDataResp) metaDataManagerClt;
endinterface



module mkCommandQueueController(CommandQueueController ifc);

    
    FIFOF#(RingbufRawDescriptor) pgtReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) pgtInflightReqQ <- mkFIFOF;
    FIFOF#(Bool) pgtRespQ <- mkFIFOF;

    FIFOF#(MetaDataReq) metaDataReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) metaDataInflightReqQ <- mkFIFOF;
    FIFOF#(MetaDataResp) metaDataRespQ <- mkFIFOF;


    RingbufDescriptorReadProxy descReadProxy <- mkRingbufDescriptorReadProxy;
    RingbufDescriptorWriteProxy descWriteProxy <- mkRingbufDescriptorWriteProxy;
    
    rule dispatchRingbufRequestDescriptors;
        let {reqSegBuf, headDescIdx} <- descReadProxy.getWideDesc;
        RingbufRawDescriptor rawDesc = reqSegBuf[headDescIdx];
        let opcode = getOpcodeFromRingbufDescriptor(rawDesc);
        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateFirstStagePGT: begin
                pgtReqQ.enq(rawDesc);
                pgtInflightReqQ.enq(rawDesc); // TODO, we can simplify this to only include 32-bit user_data field
            end
            CmdQueueOpcodeUpdateSecondStagePGT: begin
                pgtReqQ.enq(rawDesc);
                pgtInflightReqQ.enq(rawDesc); // TODO, we can simplify this to only include 32-bit user_data field
            end
            CmdQueueOpcodePdManagement: begin
                CmdQueueReqDescPdManagement desc = unpack(rawDesc);
                KeyPD pdKey = unpack(truncate(pack(desc.pdHandler)));
                metaDataInflightReqQ.enq(rawDesc);
                metaDataReqQ.enq(tagged Req4PD ReqPD{
                    allocOrNot: desc.isAlloc,
                    pdKey: pdKey,
                    pdHandler: desc.pdHandler
                });
            end
            CmdQueueOpcodeMrManagement: begin
                CmdQueueReqDescMrManagementSeg0 desc0 = unpack(reqSegBuf[1]);
                CmdQueueReqDescMrManagementSeg1 desc1 = unpack(reqSegBuf[0]);
                KeyPartMR lkeyPart = truncate(desc1.lkey);
                KeyPartMR rkeyPart = truncate(desc1.rkey);
                metaDataInflightReqQ.enq(rawDesc);
                metaDataReqQ.enq(tagged Req4MR ReqMR{
                    allocOrNot: desc0.isAlloc,
                    lkeyOrNot: True,
                    mr: MemRegion {
                        laddr: desc0.startAddr,
                        len: desc0.mrLen,
                        accFlags: FlagsType{flags: pack(desc0.accessFlag)},
                        pdHandler: desc0.pdHandler,
                        lkeyPart: lkeyPart,
                        rkeyPart: rkeyPart
                    },
                    lkey: desc1.lkey,
                    rkey: desc1.rkey
                });
            end
            CmdQueueOpcodeQpManagement: begin
                CmdQueueReqDescQpManagementSeg0 desc0 = unpack(reqSegBuf[1]);
                CmdQueueReqDescQpManagementSeg1 desc1 = unpack(reqSegBuf[0]);

                metaDataInflightReqQ.enq(rawDesc);
                metaDataReqQ.enq(tagged Req4QP ReqQP{
                    qpReqType: desc0.qpReqType,
                    pdHandler: desc0.pdHandler,
                    qpn: desc0.qpn,
                    qpAttrMask: FlagsType{flags: pack(desc0.qpAttrMask)},
                    qpAttr: AttrQP{
                        qpState: desc1.qpState,
                        curQpState: desc1.curQpState,
                        pmtu: desc1.pmtu,
                        qkey: desc1.qkey,
                        rqPSN: desc1.rqPSN,
                        sqPSN: desc1.sqPSN,
                        dqpn: desc1.dqpn,
                        qpAccessFlags: desc1.qpAccessFlags,
                        cap: QpCapacity {
                            maxSendWR: desc1.maxSendWR,
                            maxRecvWR: desc1.maxRecvWR,
                            maxSendSGE: desc1.maxSendSGE,
                            maxRecvSGE: desc1.maxRecvSGE,
                            maxInlineData: desc1.maxInlineData
                        },
                        pkeyIndex: desc1.pkeyIndex,
                        sqDraining: desc1.sqDraining,
                        maxReadAtomic: desc1.maxReadAtomic,
                        maxDestReadAtomic: desc1.maxDestReadAtomic,
                        minRnrTimer: desc1.minRnrTimer,
                        timeout: desc1.timeout,
                        retryCnt: desc1.retryCnt,
                        rnrRetry: desc1.rnrRetry
                    },
                    qpInitAttr: QpInitAttr{
                        qpType: desc0.qpType,
                        sqSigAll: desc0.sqSigAll
                    }
                });
            end
        endcase

    endrule

    rule gatherResponse if (descWriteProxy.canSetDesc);
        // TODO should we use a fair algorithm here?
        
        Vector#(CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT, RingbufRawDescriptor) respRawDescSeg = ?;
        

        if (pgtRespQ.notEmpty) begin
            CmdQueueRespDescUpdatePGT respDesc = unpack(pgtInflightReqQ.first);
            respDesc.commonHeader.isSuccessOrNeedSignalCplt = pgtRespQ.first;
            pgtInflightReqQ.deq;
            pgtRespQ.deq;
            respRawDescSeg[0] = pack(respDesc);
            descWriteProxy.setWideDesc(respRawDescSeg, 0);
        end else if (metaDataRespQ.notEmpty) begin
            
            metaDataRespQ.deq;
            metaDataInflightReqQ.deq;
           
            case (metaDataRespQ.first) matches 
                tagged Resp4PD .resp: begin
                    CmdQueueRespDescPdManagement respDesc = unpack(metaDataInflightReqQ.first);
                    respDesc.commonHeader.isSuccessOrNeedSignalCplt = resp.successOrNot;
                    respDesc.pdHandler = resp.pdHandler;
                    respRawDescSeg[0] = pack(respDesc);
                    descWriteProxy.setWideDesc(respRawDescSeg, 0);
                end
                tagged Resp4MR .resp: begin
                    CmdQueueRespDescMrManagement respDesc = unpack(metaDataInflightReqQ.first);
                    respDesc.commonHeader.extraSegmentCnt = 0;
                    respDesc.commonHeader.isSuccessOrNeedSignalCplt = resp.successOrNot;
                    respDesc.lkey = resp.lkey;
                    respDesc.rkey = resp.rkey;
                    respRawDescSeg[0] = pack(respDesc);
                    descWriteProxy.setWideDesc(respRawDescSeg, 0);
                end
                tagged Resp4QP .resp: begin
                    CmdQueueReqDescQpManagementSeg0 rawReq = unpack(metaDataInflightReqQ.first);
                    CmdQueueRespDescQpManagementSeg0 respDesc0 = CmdQueueRespDescQpManagementSeg0{
                        commonHeader: rawReq.commonHeader,
                        pdHandler: resp.pdHandler,
                        qpReqType: ?,
                        qpn: resp.qpn,
                        qpAttrMask: ?,
                        qpType: resp.qpInitAttr.qpType,
                        sqSigAll: resp.qpInitAttr.sqSigAll,
                        reserved1: ?,
                        reserved2: ?,
                        reserved3: ?,
                        reserved4: ?,
                        reserved5: ?
                    };
                    respDesc0.commonHeader.isSuccessOrNeedSignalCplt = resp.successOrNot;
                    
                    CmdQueueRespDescQpManagementSeg1 respDesc1 = CmdQueueRespDescQpManagementSeg1{
                        rnrRetry: resp.qpAttr.rnrRetry,
                        timeout: resp.qpAttr.timeout,
                        retryCnt: resp.qpAttr.retryCnt,
                        minRnrTimer: resp.qpAttr.minRnrTimer,
                        maxDestReadAtomic: resp.qpAttr.maxDestReadAtomic,
                        maxReadAtomic: resp.qpAttr.maxReadAtomic,
                        pkeyIndex: resp.qpAttr.pkeyIndex,
                        sqDraining: resp.qpAttr.sqDraining,
                        maxInlineData: resp.qpAttr.cap.maxInlineData,
                        maxRecvSGE: resp.qpAttr.cap.maxRecvSGE,
                        maxSendSGE: resp.qpAttr.cap.maxSendSGE,
                        qpAccessFlags: resp.qpAttr.qpAccessFlags,
                        dqpn: resp.qpAttr.dqpn,
                        maxRecvWR: resp.qpAttr.cap.maxRecvWR,
                        sqPSN: resp.qpAttr.sqPSN,
                        maxSendWR: resp.qpAttr.cap.maxSendWR,
                        rqPSN: resp.qpAttr.rqPSN,
                        qkey: resp.qpAttr.qkey,
                        pmtu: resp.qpAttr.pmtu,
                        curQpState: resp.qpAttr.curQpState,
                        qpState: resp.qpAttr.qpState,
                        reserved1: ?,
                        reserved2: ?,
                        reserved3: ?
                    };

                    respRawDescSeg[0] = pack(respDesc0);
                    respRawDescSeg[1] = pack(respDesc1);
                    descWriteProxy.setWideDesc(respRawDescSeg, 1);    
                end
            endcase
        end
    endrule

    interface ringbufSrv = toGPServer(descReadProxy.ringbufConnector, descWriteProxy.ringbufConnector);
    interface pgtManagerClt = toGPClient(pgtReqQ, pgtRespQ);
    interface metaDataManagerClt = toGPClient(metaDataReqQ, metaDataRespQ);
endmodule
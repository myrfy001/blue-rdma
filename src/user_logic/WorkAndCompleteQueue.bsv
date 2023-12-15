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

interface WorkRequestAndCompleteController;
    interface Put#(RingbufRawDescriptor) rqRingBuf;
    interface Put#(RingbufRawDescriptor) sqRingBuf;
    interface Get#(RingbufRawDescriptor) rcqRingBuf;
    interface Get#(RingbufRawDescriptor) scqRingBuf;


    interface Get#(RecvReq) recvReq;
    interface Get#(WorkReq) workReq;
    interface Put#(WorkComp) workCompRQ;
    interface Put#(WorkComp) workCompSQ;
endinterface



module mkWorkRequestAndCompleteController(WorkRequestAndCompleteController ifc);

    FIFOF#(RingbufRawDescriptor) rqRingBufQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) sqRingBufQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) rcqRingBufQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) scqRingBufQ <- mkFIFOF;
    
    FIFOF#(RecvReq) recvReqQ <- mkFIFOF;
    FIFOF#(WorkReq) workReqQ <- mkFIFOF;
    FIFOF#(WorkComp) workCompRqQ <- mkFIFOF;
    FIFOF#(WorkComp) workCompSqQ <- mkFIFOF;


    RingbufDescriptorReadProxy#(SQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) sqDescReadProxy <- mkRingbufDescriptorReadProxy;
    // RingbufDescriptorReadProxy#(RQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) rqDescReadProxy <- mkRingbufDescriptorReadProxy;
    // RingbufDescriptorWriteProxy#(CQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) scqDescWriteProxy <- mkRingbufDescriptorWriteProxy;
    // RingbufDescriptorWriteProxy#(CQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) rcqDescWriteProxy <- mkRingbufDescriptorWriteProxy;
    
    rule forwardSQ;
        let {reqSegBuf, headDescIdx} <- sqDescReadProxy.getWideDesc;
        
        SendQueueReqDescSeg0 desc0 = unpack(reqSegBuf[1]);
        SendQueueReqDescSeg1 desc1 = unpack(reqSegBuf[0]);

        workReqQ.enq(WorkReq{
            id: ?,
            opcode: desc0.commonHeader.opCode,
            flags: FlagsType{flags: pack(desc1.flags)},
            raddr: desc0.raddr,
            rkey: desc0.rkey,
            len: desc0.commonHeader.len,
            laddr: desc0.laddr,
            lkey: desc0.lkey,
            sqpn: desc1.sqpn,
            solicited: desc1.solicited,
            comp: tagged Invalid,
            swap: tagged Invalid,
            immDt: tagged Invalid,
            rkey2Inv: tagged Invalid,
            srqn: tagged Invalid,
            dqpn: tagged Invalid,
            qkey: tagged Invalid
        });
    endrule

    rule forwardRQ;
        rqRingBufQ.deq;  
        RecvQueueReqDesc desc0 = unpack(rqRingBufQ.first);

        recvReqQ.enq(RecvReq{
            id: ?,
            len: desc0.commonHeader.len,
            laddr: desc0.laddr,
            lkey: desc0.lkey,
            sqpn: desc0.sqpn
        });
    endrule

    rule forwardSCQ;
        workCompSqQ.deq;
        let rdmaCplt = workCompSqQ.first;
        let desc = CompQueueReqDesc {
            commonHeader: CompQueueDescCommonHead {
                len: rdmaCplt.len,
                extraSegmentCnt: 0,
                valid: True,
                reserved1:?,
                reserved2:?
            },
            opcode: rdmaCplt.opcode,
            flags: rdmaCplt.flags,
            status: rdmaCplt.status,
            pkey: rdmaCplt.pkey,
            qpn: rdmaCplt.qpn,
            reserved1: ?,
            reserved2: ?,
            reserved3: ?,
            reserved4: ?,
            reserved5: ?
        };
        scqRingBufQ.enq(pack(desc));
    endrule

    rule forwardRCQ;
        workCompRqQ.deq;
        let rdmaCplt = workCompRqQ.first;
        let desc = CompQueueReqDesc {
            commonHeader: CompQueueDescCommonHead {
                len: rdmaCplt.len,
                extraSegmentCnt: 0,
                valid: True,
                reserved1:?,
                reserved2:?
            },
            opcode: rdmaCplt.opcode,
            flags: rdmaCplt.flags,
            status: rdmaCplt.status,
            pkey: rdmaCplt.pkey,
            qpn: rdmaCplt.qpn,
            reserved1: ?,
            reserved2: ?,
            reserved3: ?,
            reserved4: ?,
            reserved5: ?
        };
        rcqRingBufQ.enq(pack(desc));
    endrule


    interface rqRingBuf = toPut(rqRingBufQ);
    interface sqRingBuf = toPut(sqRingBufQ);
    interface rcqRingBuf = toGet(rcqRingBufQ);
    interface scqRingBuf = toGet(scqRingBufQ);

    interface recvReq = toGet(recvReqQ);
    interface workReq = toGet(workReqQ);
    interface workCompRQ = toPut(workCompRqQ);
    interface workCompSQ = toPut(workCompSqQ);
endmodule
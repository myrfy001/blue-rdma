import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import Vector :: *;
import Connectable :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import UserLogicUtils :: *;
import UserLogicTypes :: *;
import MetaData :: *;
import PrimUtils :: *;
import Ringbuf :: *;
import RdmaUtils :: *;

import PayloadGen :: *;

interface WorkQueueRingbufController;
    interface Put#(RingbufRawDescriptor) sqRingBuf;
    interface Get#(WorkQueueElem) workReq;
endinterface


(* synthesize *)
module mkWorkQueueRingbufController(WorkQueueRingbufController ifc);

    FIFOF#(RingbufRawDescriptor) sqRingBufQ <- mkFIFOF;
    
    FIFOF#(WorkQueueElem) workReqQ <- mkFIFOF;

    RingbufDescriptorReadProxy#(SQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) sqDescReadProxy <- mkRingbufDescriptorReadProxy;

    mkConnection(sqDescReadProxy.ringbufConnector, toGet(sqRingBufQ));
    
    rule forwardSQ;
        // TODO: refactor the handling logic of multi descriptors, use stream mode instead. 
        let {reqSegBuf, headDescIdx} <- sqDescReadProxy.getWideDesc;

        SendQueueReqDescSeg0 desc0 = unpack(reqSegBuf[headDescIdx]);
        SendQueueReqDescSeg1 desc1 = unpack(reqSegBuf[headDescIdx-1]);
        SendQueueReqDescVariableLenSGE desc2 = unpack(reqSegBuf[headDescIdx-2]);
        SendQueueReqDescVariableLenSGE desc3 = unpack(reqSegBuf[headDescIdx-3]);

        ScatterGatherList sgl = unpack(0);

        sgl[0].laddr   =    desc2.sge1.laddr;
        sgl[0].len     =    desc2.sge1.len;
        sgl[0].lkey    =    desc2.sge1.lkey;
        sgl[1].laddr   =    desc2.sge2.laddr;
        sgl[1].len     =    desc2.sge2.len;
        sgl[1].lkey    =    desc2.sge2.lkey;
        sgl[2].laddr   =    desc3.sge1.laddr;
        sgl[2].len     =    desc3.sge1.len;
        sgl[2].lkey    =    desc3.sge1.lkey;
        sgl[3].laddr   =    desc3.sge2.laddr;
        sgl[3].len     =    desc3.sge2.len;
        sgl[3].lkey    =    desc3.sge2.lkey;

        case (desc1.sgeCnt)
            1: begin             
                sgl[0].isFirst =    True;
                sgl[0].isLast  =    True;
            end
            2: begin             
                sgl[0].isFirst =    True;
                sgl[1].isLast  =    True;
            end
            3: begin             
                sgl[0].isFirst =    True;
                sgl[2].isLast  =    True;
            end
            4: begin             
                sgl[0].isFirst =    True;
                sgl[3].isLast  =    True;
            end
        endcase

        WorkQueueElem req   = unpack(0);
        req.pkey            = desc0.pkey;
        req.opcode          = desc0.commonHeader.opCode;
        req.flags           = unpack(pack(desc1.flags));
        req.qpType          = desc1.qpType;
        req.psn             = desc1.psn;
        req.pmtu            = desc1.pmtu;
        req.dqpIP           = tagged IPv4 desc0.dqpIP;
        req.macAddr         = desc1.macAddr;
        req.sgl             = sgl;
        req.raddr           = desc0.raddr;
        req.rkey            = desc0.rkey;
        req.dqpn            = desc1.dqpn;
        req.isFirst         = desc0.commonHeader.isFirst;
        req.isLast          = desc0.commonHeader.isLast;
        req.totalLen        = desc0.commonHeader.totalLen;

        let hasImmDt = workReqHasImmDt(req.opcode);
        let hasInv   = workReqHasInv(req.opcode);
        let immOrInv = hasImmDt ? tagged Imm desc1.imm : tagged RKey desc1.imm;
        req.immDtOrInvRKey = (hasImmDt || hasInv) ? tagged Valid immOrInv : tagged Invalid;

        workReqQ.enq(req);

        $display("SQ read a new descriptor: ", fshow(req));
    endrule

    interface sqRingBuf = toPut(sqRingBufQ);
    interface workReq = toGet(workReqQ);
endmodule
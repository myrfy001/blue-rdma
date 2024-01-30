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

interface WorkQueueRingbufController;
    interface Put#(RingbufRawDescriptor) sqRingBuf;
    interface Get#(RingbufRawDescriptor) scqRingBuf;

    interface Get#(WorkQueueElem) workReq;
    interface Put#(WorkComp) workCompSQ;
endinterface



module mkWorkQueueRingbufController(WorkQueueRingbufController ifc);

    FIFOF#(RingbufRawDescriptor) sqRingBufQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) scqRingBufQ <- mkFIFOF;
    
    FIFOF#(WorkQueueElem) workReqQ <- mkFIFOF;
    FIFOF#(WorkComp) workCompSqQ <- mkFIFOF;


    RingbufDescriptorReadProxy#(SQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) sqDescReadProxy <- mkRingbufDescriptorReadProxy;

    mkConnection(sqDescReadProxy.ringbufConnector, toGet(sqRingBufQ));
    
    rule forwardSQ;
        let {reqSegBuf, headDescIdx} <- sqDescReadProxy.getWideDesc;
        
        SendQueueReqDescSeg0 desc0 = unpack(reqSegBuf[1]);
        SendQueueReqDescSeg1 desc1 = unpack(reqSegBuf[0]);

        WorkQueueElem req = unpack(0);

        workReqQ.enq(req);

        $display("SQ read a new descriptor: ", fshow(req));
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


    interface sqRingBuf = toPut(sqRingBufQ);
    interface scqRingBuf = toGet(scqRingBufQ);

    interface workReq = toGet(workReqQ);
    interface workCompSQ = toPut(workCompSqQ);
endmodule
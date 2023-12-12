import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import UserLogicUtils :: *;
import UserLogicTypes :: *;
import MetaData :: *;

Integer reqDescFieldOffset_F_PD_ADMIN_IS_ALLOC = 96;
Integer reqDescFieldOffset_F_PD_ADMIN_HANDLER = 64;
Integer respDescFieldOffset_F_CMD_SUCCESS = 64;
Integer respDescFieldOffset_F_PD_ADMIN_HANDLER = 96;


interface CommandQueueController;
    interface Server#(RingbufRawDescriptor, RingbufRawDescriptor) ringbufSrv;
    interface Client#(RingbufRawDescriptor, Bool) pgtManagerClt;
    interface Client#(MetaDataReq, MetaDataResp) metaDataManagerClt;
endinterface

function t_data getFieldFromRawRingbufDescriptor(RingbufRawDescriptor desc, Integer offset) provisos(Bits#(t_data, sz_data));
    let bytes = pack(desc);
    return unpack(bytes[offset + valueOf(sz_data) - 1: offset]);
endfunction
function RingbufRawDescriptor setFieldFromRawRingbufDescriptor(RingbufRawDescriptor desc, Integer offset, t_data value) 
    provisos(
        Bits#(t_data, sz_data),
        Add#(anysize, sz_data, SizeOf#(RingbufRawDescriptor))
    );

    let bytes = pack(desc);
    bytes[offset + valueOf(sz_data) - 1: offset] = pack(value);
    return unpack(bytes);
endfunction

module mkCommandQueueController(CommandQueueController ifc);
    FIFOF#(RingbufRawDescriptor) ringbufReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) ringbufRespQ <- mkFIFOF;
    
    FIFOF#(RingbufRawDescriptor) pgtReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) pgtInflightReqQ <- mkFIFOF;
    FIFOF#(Bool) pgtRespQ <- mkFIFOF;

    FIFOF#(MetaDataReq) metaDataReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) metaDataInflightReqQ <- mkFIFOF;
    FIFOF#(MetaDataResp) metaDataRespQ <- mkFIFOF;

    Vector#(CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT, Reg#(readVReg)) segBuf <- replicateM(mkRegU);

    Reg#(Bool) isFillingSegments <- mkReg(True); 

    Reg#(DescriptorSegmentIndex) totalSegCnt <- mkRegU;
    Reg#(DescriptorSegmentIndex) curSegCnt <- mkReg(0);

    rule fillAllSegments if (isFillingSegments);
        let desc = ringbufReqQ.first;
        ringbufReqQ.deq;
        segBuf[0] <= desc;
        for (Integer i = 0; i < valueOf(CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT) - 1; i=i+1) begin
            segBuf [i+1] <= segBuf[i]
        end
    endrule
    
    
    rule dispatchRingbufRequestDescriptors if !(isFillingSegments);
        
        let opcode = getOpcodeFromRingbufDescriptor(desc);
        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateFirstStagePGT: begin
                pgtReqQ.enq(desc);
                pgtInflightReqQ.enq(desc); // TODO, we can simplify this to only include 32-bit user_data field
            end
            CmdQueueOpcodeUpdateSecondStagePGT: begin
                pgtReqQ.enq(desc);
                pgtInflightReqQ.enq(desc); // TODO, we can simplify this to only include 32-bit user_data field
            end
            CmdQueueOpcodePdManagement: begin
                HandlerPD pdHandler = getFieldFromRawRingbufDescriptor(desc, reqDescFieldOffset_F_PD_ADMIN_HANDLER);
                KeyPD pdKey = unpack(truncate(pack(pdHandler)));
                metaDataReqQ.enq(tagged Req4PD ReqPD{
                    allocOrNot: getFieldFromRawRingbufDescriptor(desc, reqDescFieldOffset_F_PD_ADMIN_IS_ALLOC),
                    pdKey: pdKey,
                    pdHandler: pdHandler
                });
            end
        endcase

    endrule

    rule gatherResponse;
        // TODO should we use a fair algorithm here?
        
        if (pgtRespQ.notEmpty) begin
            RingbufRawDescriptor respDesc = pgtInflightReqQ.first;
            respDesc = setFieldFromRawRingbufDescriptor(respDesc, respDescFieldOffset_F_CMD_SUCCESS, pgtRespQ.first);
            ringbufRespQ.enq(respDesc);
            pgtInflightReqQ.deq;
            pgtRespQ.deq;
        end else if (metaDataRespQ.notEmpty) begin
            RingbufRawDescriptor respDesc = metaDataInflightReqQ.first;
            metaDataRespQ.deq;
            metaDataInflightReqQ.deq;

            case (metaDataRespQ.first) matches 
                tagged Resp4PD .resp: begin
                    respDesc = setFieldFromRawRingbufDescriptor(respDesc, respDescFieldOffset_F_CMD_SUCCESS, resp.successOrNot);
                    respDesc = setFieldFromRawRingbufDescriptor(respDesc, respDescFieldOffset_F_PD_ADMIN_HANDLER, resp.pdHandler);
                end
            endcase 
            
            ringbufRespQ.enq(respDesc);
        end
        
    endrule

    interface ringbufSrv = toGPServer(ringbufReqQ, ringbufRespQ);
    interface pgtManagerClt = toGPClient(pgtReqQ, pgtRespQ);
    interface metaDataManagerClt = toGPClient(metaDataReqQ, metaDataRespQ);
endmodule
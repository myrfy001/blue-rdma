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
import Ringbuf :: *;

interface CommandQueueController;
    interface Server#(RingbufRawDescriptor, RingbufRawDescriptor) ringbufSrv;
    interface Client#(RingbufRawDescriptor, Bool) mrAndPgtManagerClt;
    interface Client#(WriteReqCommonQPC, Bool) qpcModifyClt;
endinterface


(* synthesize *)
module mkCommandQueueController(CommandQueueController ifc);

    
    FIFOF#(RingbufRawDescriptor) mrAndPgtReqQ           <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) mrAndPgtInflightReqQ   <- mkFIFOF;
    FIFOF#(Bool) mrAndPgtRespQ                          <- mkFIFOF;

    FIFOF#(WriteReqCommonQPC) qpcReqQ                   <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) qpcInflightReqQ        <- mkFIFOF;
    FIFOF#(Bool) qpcRespQ                               <- mkFIFOF;


    RingbufDescriptorReadProxy#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) descReadProxy <- mkRingbufDescriptorReadProxy;
    RingbufDescriptorWriteProxy#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT) descWriteProxy <- mkRingbufDescriptorWriteProxy;
    
    rule dispatchRingbufRequestDescriptors;
        let {reqSegBuf, headDescIdx} <- descReadProxy.getWideDesc;
        RingbufRawDescriptor rawDesc = reqSegBuf[headDescIdx];
        let opcode = getOpcodeFromRingbufDescriptor(rawDesc);
        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateMrTable, CmdQueueOpcodeUpdatePGT: begin
                mrAndPgtReqQ.enq(rawDesc);
                mrAndPgtInflightReqQ.enq(rawDesc); // TODO, we can simplify this to only include 32-bit user_data field
            end
            CmdQueueOpcodeQpManagement: begin
                CmdQueueReqDescQpManagementSeg0 desc0 = unpack(reqSegBuf[0]);

                let ent = EntryCommonQPC {
                    isError:        desc0.isError,
                    qpnKeyPart:     getKeyQP(desc0.qpn), 
                    pdHandler:      desc0.pdHandler,
                    qpType:         desc0.qpType,
                    rqAccessFlags:  desc0.rqAccessFlags,
                    pmtu:           desc0.pmtu
                };

                qpcInflightReqQ.enq(rawDesc);
                qpcReqQ.enq(
                    WriteReqCommonQPC {
                        qpn: desc0.qpn,
                        ent: desc0.isValid ? tagged Valid ent : tagged Invalid
                    }
                );
            end
        endcase

    endrule

    rule gatherResponse if (descWriteProxy.canSetDesc);
        // TODO should we use a fair algorithm here?
        
        Vector#(COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT, RingbufRawDescriptor) respRawDescSeg = ?;
        

        if (mrAndPgtRespQ.notEmpty) begin
            // Note: since the update MR and update PGT req both need only one descriptor and the response only care for
            // `isSuccessOrNeedSignalCplt` field, so we use CmdQueueRespDescUpdatePGT to handle both resp.
            CmdQueueRespDescUpdatePGT respDesc = unpack(mrAndPgtInflightReqQ.first);
            respDesc.commonHeader.isSuccessOrNeedSignalCplt = mrAndPgtRespQ.first;
            mrAndPgtInflightReqQ.deq;
            mrAndPgtRespQ.deq;
            respRawDescSeg[0] = pack(respDesc);
            descWriteProxy.setWideDesc(respRawDescSeg, 0);
        end 
        else if (qpcRespQ.notEmpty) begin 
            qpcRespQ.deq;
            qpcInflightReqQ.deq;
           
            CmdQueueRespDescQpManagementSeg0 respDesc = unpack(qpcInflightReqQ.first);
            respDesc.commonHeader.isSuccessOrNeedSignalCplt = qpcRespQ.first;
            respRawDescSeg[0] = pack(respDesc);
            descWriteProxy.setWideDesc(respRawDescSeg, 0);
        end
    endrule

    interface ringbufSrv = toGPServer(descReadProxy.ringbufConnector, descWriteProxy.ringbufConnector);
    interface mrAndPgtManagerClt = toGPClient(mrAndPgtReqQ, mrAndPgtRespQ);
    interface qpcModifyClt = toGPClient(qpcReqQ, qpcRespQ);
endmodule
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import UserLogicUtils :: *;
import UserLogicTypes :: *;

interface CommandQueueController;
    interface Server#(RingbufRawDescriptor, RingbufRawDescriptor) ringbufSrv;
    interface Client#(RingbufRawDescriptor, Bool) pgtManagerClt;
endinterface

module mkCommandQueueController(CommandQueueController ifc);
    FIFOF#(RingbufRawDescriptor) ringbufReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) ringbufRespQ <- mkFIFOF;
    
    FIFOF#(RingbufRawDescriptor) pgtReqQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) pgtInflightReqQ <- mkFIFOF;
    FIFOF#(Bool) pgtRespQ <- mkFIFOF;
    
    
    rule dispatchRingbufRequestDescriptors;
        let desc = ringbufReqQ.first;
        ringbufReqQ.deq;
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
        endcase

    endrule

    rule gatherResponse;
        // TODO should we use a fair algorithm here?
        if (pgtRespQ.notEmpty) begin
            ringbufRespQ.enq(pgtInflightReqQ.first);
            pgtInflightReqQ.deq;
            pgtRespQ.deq;
        end
        
    endrule

    interface ringbufSrv = toGPServer(ringbufReqQ, ringbufRespQ);
    interface pgtManagerClt = toGPClient(pgtReqQ, pgtRespQ);
endmodule
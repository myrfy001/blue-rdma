import ClientServer :: * ;
import GetPut :: *;
import Connectable :: *;

import UserLogicSettings :: *;
import UserLogicTypes :: *;


function RingbufRawDescriptorOpcode getOpcodeFromRingbufDescriptor(RingbufRawDescriptor desc);
    return pack(desc)[valueOf(RINGBUF_DESC_OPCODE_OFFSET) + valueOf(RINGBUF_DESC_OPCODE_LENGTH) - 1 :valueOf(RINGBUF_DESC_OPCODE_OFFSET)];
endfunction

module mkFakeClient(Client#(t_req, t_resp));
    Reg#(Bool) t <- mkReg(False);
    interface Get request;
        method ActionValue#(t_req) get() if (t);
            return ?;
        endmethod
    endinterface
    interface Put response;
        method Action put(t_resp val) if (t);
        endmethod
    endinterface
endmodule

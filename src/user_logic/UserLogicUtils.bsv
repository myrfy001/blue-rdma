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


// module mkActionValue2Server#(function ActionValue #(t_resp) func (t_req x)) (Server#(t_req, t_resp) ifc)
//     provisos(Bits#(t_req, sz_req), Bits#(t_resp, sz_resp));
//     Wire#(t_resp) respWire <- mkWire;

//     interface Put request;
//         method Action put(t_req req);
//             let t <- func(req);
//             respWire <= t;
//         endmethod
//     endinterface

//     interface Get response;
//         method ActionValue#(t_resp) get();
//             return respWire;
//         endmethod
//     endinterface
// endmodule

// interface ActionValueCallClient#(type t_req, type t_resp);
//     interface Client#(t_req, t_resp) clt;
//     method ActionValue#(t_resp) call(t_req req);
// endinterface


// module mkActionValueCallClient(ActionValueCallClient#(t_req, t_resp) ifc)
//     provisos(Bits#(t_req, sz_req), Bits#(t_resp, sz_resp));

//     Wire#(t_req) reqWire <- mkWire;
//     Wire#(t_resp) respWire <- mkWire;

//     method ActionValue#(t_resp) call(t_req req);
//         reqWire <= req;
//         return respWire;
//     endmethod

//     interface Client clt;
//         interface Put response;
//             method Action put(t_resp resp);
//                 respWire <= resp; 
//             endmethod
//         endinterface

//         interface Get request;
//             method ActionValue#(t_req) get();
//                 return reqWire;
//             endmethod
//         endinterface
//     endinterface
// endmodule



// module mkFoo(Server#(Bit#(2), Bit#(2)));
//     Reg#(Bit#(2)) a <- mkReg(0);
//     function ActionValue#(Bit#(2)) fff(Bit#(2) x);
//         return (actionvalue
//             let ret = ?;
//             a <= x;
//             if (a == 2) begin
//                 ret = x + 2;
//             end else begin
//                 ret = x + 1;
//             end
//             return ret;
//         endactionvalue);
//     endfunction
//     let t <- mkActionValue2Server(fff);
//     return t;
// endmodule



// module mkBsvTop(Empty);
//     let remote <- mkFoo;
//     let local1 <- mkActionValueCallClient;
//     Reg#(Bit#(2)) r <- mkReg(0);
//     Reg#(Bit#(2)) cnt <- mkReg(0);

//     // mkConnection(remote, local1.clt);
//     (* conflict_free = "forwardReq, forwardResp" *)
//     rule forwardReq;
//         let t1 <- local1.clt.request.get;
//         remote.request.put(t1);
//     endrule
//     rule forwardResp;
//         let t2 <- remote.response.get;
//         local1.clt.response.put(t2);
//     endrule



//     rule printClk;
//         cnt <= cnt + 1;
//         $display("cnt=%d", cnt);
//     endrule

//     rule run;
//         let ret <- local1.call(r);
//         r <= ret;
//         $display("r=%d", r);
//     endrule
// endmodule
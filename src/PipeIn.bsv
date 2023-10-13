import FIFOF :: *;
import GetPut :: *;
import PAClib :: *;
import Connectable :: *;

interface PipeIn#(type t_ele);
    method Action enq(t_ele element);
    method Bool notFull;
endinterface

function PipeIn #(t_ele)  f_FIFOF_to_PipeIn  (FIFOF #(t_ele) fifof);
    return (interface PipeIn;
               method Action enq(t_ele element);
                  fifof.enq(element);
               endmethod
               method Bool notFull();
                  return fifof.notFull();
               endmethod
            endinterface);
endfunction

instance ToPut #(PipeIn #(t_ele), t_ele);
    function Put #(t_ele) toPut (PipeIn #(t_ele) pi);
       return (interface Put;
                  method Action put (t_ele data);
                     pi.enq (data);
                  endmethod
               endinterface);
    endfunction
endinstance

instance Connectable#(PipeOut#(anytype), PipeIn#(anytype));
   module mkConnection#(PipeOut#(anytype) po, PipeIn#(anytype) pi)(Empty);
      rule forward;
         pi.enq(po.first);
         po.deq;
      endrule
   endmodule
endinstance
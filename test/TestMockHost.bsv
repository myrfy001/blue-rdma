import BRAM :: *;
import MockHost :: *;

(* doc = "testcase" *)
module mkTestMockHost(Empty);
    BRAM_Configure cfg = defaultValue;
    BRAM2PortBE#(Bit#(5), Bit#(8), 1) a <- mkMockHost(cfg);
endmodule
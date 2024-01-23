import FIFOF :: *;
import ClientServer :: * ;
import GetPut :: *;
import Clocks :: * ;
import Vector :: *;
import BRAM :: *;


import Connectable :: * ;
import StmtFSM::*;
import Randomizable :: * ;


(* synthesize *)
module mkTb(Empty);


    BRAM_Configure cfg = defaultValue;
    cfg.allowWriteResponseBypass = False;
    cfg.memorySize = 1024*1024; // 64 MB, word size is 64B
    cfg.loadFormat = tagged Hex "../user_logic/test_host_memory.hex";
    

    BRAM2PortBE#(Bit#(32), Bit#(512), 64) hostMem <- mkBRAM2ServerBE(cfg);


    rule sendDmaReadReq;
        hostMem.portA.request.put(BRAMRequestBE{
            writeen: 0,
            responseOnWrite: False,
            address: 1,
            datain: ?
        });
    endrule

    rule forwardDMAReadToDMAWrite;
        Bit#(512) r <-  hostMem.portA.response.get;

        $display(fshow(r));
        $finish;
    endrule


endmodule
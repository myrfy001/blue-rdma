import BRAM :: *;
import MockHost :: *;

(* doc = "testcase" *)
module mkTestMockHost(Empty);
    BRAM_Configure cfg = defaultValue;
    cfg.memorySize = 1024*1024;
    MockHost#(Bit#(32), Bit#(512), 64, Bit#(8), Bit#(8)) mockHost <- mkMockHost(cfg);
    
    Reg#(Bit#(5)) step <- mkReg(1);

    rule step1 if (mockHost.ready && (step == 1));
        mockHost.hostMem.portA.request.put(BRAMRequestBE{
            writeen: 'hFFFFFFFFFFFFFFFF,
            responseOnWrite: False,
            address: 2,
            datain: 'h11111111111111111111111111
        });
        step <= step + 1;
    endrule

    rule step2 if (step == 2);
        mockHost.hostMem.portA.request.put(BRAMRequestBE{
            writeen: 'hFFFFFFFFFFFFFFFF,
            responseOnWrite: False,
            address: 3,
            datain: 'h222222222222222222222222
        });
        step <= step + 1;
    endrule

    rule step3 if (step == 3);
        mockHost.hostMem.portA.request.put(BRAMRequestBE{
            writeen: 'h8,
            responseOnWrite: False,
            address: 2,
            datain: 'hEE000000
        });
        step <= step + 1;
    endrule

    rule step4 if (step == 4);
        mockHost.hostMem.portA.request.put(BRAMRequestBE{
            writeen: 0,
            responseOnWrite: False,
            address: 2,
            datain: ?
        });
        step <= step + 1;
    endrule

    rule step5 if (step == 5);
        let resp <- mockHost.hostMem.portA.response.get;
        if (resp != 'h111111111111111111EE111111) begin
            $display("Error: mockHost BRAM not work as expected");
        end
        $finish;
    endrule
    
endmodule
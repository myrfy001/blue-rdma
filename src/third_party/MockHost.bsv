package MockHost ;

import BRAM :: *;
import BRAMCore ::*;
import DefaultValue ::*;
import ClientServer ::*;
import FIFOF :: *;
import DReg :: *;
import GetPut :: *;
import SpecialFIFOs :: *;

//  Export  section

// Modules for export
export mkMockHost;


// imported C function to handle shared memory
import "BDPI" function ActionValue#(Bit#(64)) c_createBRAM(Bit#(32) wordWidth, Bit#(32) addrWidth);
// import "BDPI" function ActionValue#(Bit#(sz_word)) c_readBRAM(Bit#(64) memPtr, Bit#(64) wordAddr, Bit#(32) wordWidth);
import "BDPI" function ActionValue#(t_word) c_readBRAM(Bit#(64) memPtr, Bit#(64) wordAddr, Bit#(32) wordWidth) provisos (
	Bits#(t_word, sz_word)
);
import "BDPI" function Action c_writeBRAM(Bit#(64) memPtr, Bit#(64) wordAddr, t_word d, Bit#(sz_byteen) byteEn, Bit#(32) wordWidth) provisos (
	Bits#(t_word, sz_word)
);

function ActionValue#(t_word) readBRAM(Bit#(64) memPtr, t_addr wordAddr) provisos (
	Bits#(t_addr, sz_addr),
	Bits#(t_word, sz_word),
	Add#(a__, sz_addr, 64)
);
    return c_readBRAM(memPtr, zeroExtend(pack(wordAddr)), fromInteger(valueOf(sz_word)));
endfunction

function Action writeBRAM(Bit#(64) memPtr, t_addr wordAddr, t_word d, Bit#(sz_byteen) byteEn) provisos (
	Bits#(t_addr, sz_addr),
	Bits#(t_word, sz_word),
	Add#(a__, sz_addr, 64)
);
    return c_writeBRAM(memPtr, zeroExtend(pack(wordAddr)), d, byteEn, fromInteger(valueOf(sz_word)));
endfunction


// mkBRAM2ServerBE  Dual ported
// Exported module
module mkMockHost #( BRAM_Configure cfg ) (BRAM2PortBE #(addr, data, n) ) provisos(
	Bits#(addr, addr_sz),
	Bits#(data, data_sz),
	Div#(data_sz, n, chunk_sz),
	Mul#(chunk_sz, n, data_sz),
	Add#(a__, addr_sz, 64)
);


    // mem
	Reg#(Bit#(64))  memHandle   <- mkReg(0);
	Reg#(Bool)      initDone    <- mkReg(False);

	FIFOF#(data) portRespQueueA <- mkFIFOF;
	FIFOF#(data) portRespQueueB <- mkFIFOF;

    rule doInit(!initDone);
		let ptr <- c_createBRAM(fromInteger(valueOf(data_sz)), fromInteger(cfg.memorySize));
		if(ptr == 0) begin
			$fwrite(stderr, "%0t: mkControlledBRAM2ServerBE: ERROR: fail to create memory\n", $time);
			$finish;
		end
		$display("%0t: mkControlledBRAM2ServerBE: allocate memory ptr = %h", $time, ptr);
		memHandle <= ptr;
		initDone <= True;
	endrule


	interface BRAMServer portA;
		interface Put request;
			method Action put(BRAMRequestBE#(addr, data, n) req);
				if (req.writeen == 0) begin
					let resp <- readBRAM(memHandle, req.address);
					portRespQueueA.enq(resp);
				end 
				else begin
					writeBRAM(memHandle, req.address, req.datain, req.writeen);
					if (req.responseOnWrite) begin
						portRespQueueA.enq(unpack(0));
					end
				end
			endmethod
		endinterface

		interface Get response;
			method ActionValue#(data) get;
				portRespQueueA.deq;
				return portRespQueueA.first;
			endmethod
		endinterface
	endinterface

    method Action portAClear;
	endmethod


	interface BRAMServer portB;
		interface Put request;
			method Action put(BRAMRequestBE#(addr, data, n) req);
				if (req.writeen == 0) begin
					let resp <- readBRAM(memHandle, req.address);
					portRespQueueB.enq(resp);
				end 
				else begin
					writeBRAM(memHandle, req.address, req.datain, req.writeen);
					if (req.responseOnWrite) begin
						portRespQueueB.enq(unpack(0));
					end
				end
			endmethod
		endinterface

		interface Get response;
			method ActionValue#(data) get;
				portRespQueueB.deq;
				return portRespQueueB.first;
			endmethod
		endinterface
	endinterface

    method Action portBClear;
	endmethod
endmodule

endpackage
package MockHost ;

import Clocks :: * ;
import BRAM :: *;
import BRAMCore ::*;
import DefaultValue ::*;
import ClientServer ::*;
import FIFOF :: *;
import DReg :: *;
import GetPut :: *;
import SpecialFIFOs :: *;
import Ports :: *;
import DataTypes :: *;
import PrimUtils :: *;
import SemiFifo :: *;
import UserLogicTypes :: *;
import AxiStreamTypes :: *;

//  Export  section

// Modules for export
export mkMockHost;

// Interfaces
export MockHost(..);


// imported C function to handle shared memory
import "BDPI" function ActionValue#(Bit#(64)) c_createBRAM(Bit#(32) wordWidth, Bit#(32) addrWidth);
import "BDPI" function ActionValue#(t_word) c_readBRAM(Bit#(64) memPtr, Bit#(64) wordAddr, Bit#(32) wordWidth) provisos (
	Bits#(t_word, sz_word)
);
import "BDPI" function Action c_writeBRAM(Bit#(64) memPtr, Bit#(64) wordAddr, t_word d, Bit#(sz_byteen) byteEn, Bit#(32) wordWidth) provisos (
	Bits#(t_word, sz_word)
);

import "BDPI" function ActionValue#(PcieBarAccessAction) c_getPcieBarReadReq(Bit#(64) memPtr);
import "BDPI" function Action c_putPcieBarReadResp(Bit#(64) memPtr, PcieBarAccessAction resp);
import "BDPI" function ActionValue#(PcieBarAccessAction) c_getPcieBarWriteReq(Bit#(64) memPtr);
import "BDPI" function Action c_putPcieBarWriteResp(Bit#(64) memPtr, PcieBarAccessAction resp);

import "BDPI" function ActionValue#(NetIfcAccessAction) c_netIfcGetRxData(Bit#(64) memPtr);
import "BDPI" function Action c_netIfcPutTxData(Bit#(64) memPtr, NetIfcAccessAction resp);

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


interface MockHost#(type addr, type data, numeric type n, type bar_addr_t, type bar_data_t);
	interface BRAM2PortBE #(addr, data, n) hostMem;
	interface Client#(bar_addr_t, bar_data_t) barReadClt;
	interface Client#(Tuple2#(bar_addr_t, bar_data_t), Bool) barWriteClt; 

	interface AxiStream512FifoIn   axiStreamTxUdp;
	interface Get#(AxiStream512)   axiStreamRxUdp;

	method Bool ready;
endinterface

typedef struct {
	Bit#(64) pci_tag;
	Bit#(64) valid;
	Bit#(64) addr;
	Bit#(64) value;
} PcieBarAccessAction deriving(Bits, FShow);

typedef struct {
	Bit#(8) isValid;
	Bit#(8) isLast;
	Bit#(8) isFirst;
	Bit#(8) reserved1;
	ByteEnWide byteEn;
	DATA_WIDE data;
} NetIfcAccessAction deriving(Bits, FShow); 


// Exported module
module mkMockHost #( BRAM_Configure cfg, Clock cmacRxTxClk, Reset cmacRxTxRst) (MockHost#(addr, data, n, bar_addr_t, bar_data_t)) provisos(
	Bits#(addr, addr_sz),
	Bits#(data, data_sz),
	Div#(data_sz, n, chunk_sz),
	Mul#(chunk_sz, n, data_sz),
	Add#(a__, addr_sz, 64),
	Bits#(bar_addr_t, bar_addr_sz),
	Bits#(bar_data_t, bar_data_sz),
	Add#(b__, bar_addr_sz, 64),
	Add#(c__, bar_data_sz, 64),
	FShow#(addr),
	FShow#(data)
);

	Clock srcClock <- exposeCurrentClock;
    Reset srcReset <- exposeCurrentReset;

    // mem
	Reg#(Bit#(64))  memHandle   <- mkReg(0);
	Reg#(Bool)      initDone    <- mkReg(False);
	Reg#(Bit#(64))  memHandleForCmac   <- mkSyncReg(0, srcClock, srcReset, cmacRxTxClk);
	Reg#(Bool)      initDoneForCamc    <- mkSyncReg(False, srcClock, srcReset, cmacRxTxClk);


	FIFOF#(data) portRespQueueA <- mkFIFOF;
	FIFOF#(data) portRespQueueB <- mkFIFOF;

	FIFOF#(Tuple2#(bar_addr_t, bar_data_t)) barWriteReqQ <- mkFIFOF;
	FIFOF#(Bool) barWriteRespQ <- mkFIFOF;
	FIFOF#(bar_addr_t) barReadReqQ <- mkFIFOF;
	FIFOF#(bar_data_t) barReadRespQ <- mkFIFOF;

	FIFOF#(AxiStream512) udpAxiTxQ <- mkFIFOF(clocked_by cmacRxTxClk, reset_by cmacRxTxRst);
	FIFOF#(AxiStream512) udpAxiRxQ <- mkFIFOF(clocked_by cmacRxTxClk, reset_by cmacRxTxRst);



	// Note, it assumes that the resp will keep order as the request.
	// We do not support out of order now, to support OOO, the CSR
	// handling logic must also pass the tag field all the way around.
	// since the current CSR/BAR read logic is simple and will finish
	// in one cycle, it can't be out of order, so we simply keep tag 
	// in order in this queue.
	FIFOF#(Bit#(64)) readTagKeepOrderQ <- mkFIFOF;
	FIFOF#(Bit#(64)) writeTagKeepOrderQ <- mkFIFOF;

    rule doInit(!initDone);
		let ptr <- c_createBRAM(fromInteger(valueOf(data_sz)), fromInteger(cfg.memorySize));
		if(ptr == 0) begin
			$fwrite(stderr, "%0t: mkControlledBRAM2ServerBE: ERROR: fail to create memory\n", $time);
			$finish;
		end
		$display("%0t: mkControlledBRAM2ServerBE: allocate memory ptr = %h", $time, ptr);
		memHandle <= ptr;
		initDone <= True;
		memHandleForCmac <= ptr;
		initDoneForCamc <= True;
	endrule

	rule forwardBarReadReq if (initDone);
		let rawReq <- c_getPcieBarReadReq(memHandle);
		if (rawReq.valid != 0) begin
			barReadReqQ.enq(unpack(truncate(pack(rawReq.addr))));
			readTagKeepOrderQ.enq(rawReq.pci_tag);
		end
	endrule

	rule forwardBarReadResp if (initDone);
		barReadRespQ.deq;
		readTagKeepOrderQ.deq;
		let resp = PcieBarAccessAction {
			pci_tag: readTagKeepOrderQ.first,
			valid: 1,
			addr: 0,
			value: unpack(zeroExtend(pack(barReadRespQ.first)))
		};
		c_putPcieBarReadResp(memHandle, resp);
	endrule

	rule forwardBarWriteReq if (initDone);
		let rawReq <- c_getPcieBarWriteReq(memHandle);
		if (rawReq.valid != 0) begin
			barWriteReqQ.enq(
				tuple2(
					unpack(truncate(pack(rawReq.addr))),
					unpack(truncate(pack(rawReq.value)))
				)
			);
			writeTagKeepOrderQ.enq(rawReq.pci_tag);
		end
	endrule

	rule forwardBarWriteResp if (initDone);
		barWriteRespQ.deq;
		writeTagKeepOrderQ.deq;
		let resp = PcieBarAccessAction {
			pci_tag: writeTagKeepOrderQ.first,
			valid: barWriteRespQ.first ? 1 : 0,
			addr: 0,
			value: 0
		};
		c_putPcieBarWriteResp(memHandle, resp);
	endrule

	rule forwardNetIfcTx if (initDoneForCamc);

		if (udpAxiTxQ.notEmpty) begin
			let originTxData = udpAxiTxQ.first;
			udpAxiTxQ.deq;

			let req = NetIfcAccessAction {
				isValid: 1,
				isLast: originTxData.tLast ? 1 : 0,
				isFirst: ?,
				reserved1: 0,
				byteEn: originTxData.tKeep,
				data: originTxData.tData
			};
			$display("time=%0t: ", $time, "net ifc send data=", fshow(req));
			
			c_netIfcPutTxData(memHandleForCmac, req);
		end
		else begin
			// $display("time=%0t: ", $time, "net ifc send data=NO_DATA_TO_SEND");
		end

	endrule

	rule forwardNetIfcRx if (initDoneForCamc);
		let rawReq <- c_netIfcGetRxData(memHandleForCmac);
		if (rawReq.isValid != 0) begin
			AxiStream512 req = AxiStream512{
				tLast: rawReq.isLast != 0 ? True : False,
				tKeep: rawReq.byteEn,
				tData: rawReq.data,
				tUser: 0
			};

			udpAxiRxQ.enq(req);
			$display("time=%0t: ", $time, "net ifc recv data=", fshow(req));

			// if (udpAxiRxQ.notFull) begin
			// 	udpAxiRxQ.enq(req);
			// 	$display("time=%0t: ", $time, "net ifc recv data=", fshow(req));
			// end 
			// else begin
			// 	$display("time=%0t: ", $time, "net ifc recv data BUT DISCARD SINCE QUEUE FULL");
			// end
		end
	endrule

	method Bool ready = initDone;

	interface BRAM2PortBE hostMem;
		interface BRAMServer portA;
			interface Put request;
				method Action put(BRAMRequestBE#(addr, data, n) req);
					$display(
						"time=%0t::", $time, "mock Host Mem Port A req.writeen=", fshow(req.writeen),
						" req.addr=", fshow(req.address)
					);
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
					$display(
						"time=%0t::", $time, "mock Host Mem Port A output read result=", fshow(portRespQueueA.first)
					);

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
					$display(
						"time=%0t::", $time, "mock Host Mem Port B req.writeen=", fshow(req.writeen),
						" req.addr=", fshow(req.address)
					);
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
	endinterface

	interface barWriteClt = toGPClient(barWriteReqQ, barWriteRespQ);
    interface barReadClt = toGPClient(barReadReqQ, barReadRespQ);

	interface axiStreamRxUdp 	= toGet(udpAxiRxQ);
	interface axiStreamTxUdp 	= convertFifoToFifoIn(udpAxiTxQ);

endmodule

endpackage
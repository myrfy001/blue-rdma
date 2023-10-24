import FIFOF :: *;
import Connectable :: *;
import GetPut :: *;
import StmtFSM :: *;

import BusConversion :: *;
import SemiFifo :: *;
import AxiStreamTypes :: *;
import Axi4LiteTypes :: *;
import Axi4Types :: *;



typedef 12 CONTROL_REG_ADDR_WIDTH;
typedef 4 CONTROL_REG_DATA_STRB_WIDTH;
typedef TMul#(CONTROL_REG_DATA_STRB_WIDTH, 8) CONTROL_DATA_WIDTH;
typedef 64 HOST_ADDR_WIDTH;
typedef 28 DESC_BYPASS_LENGTH_WIDTH;
typedef 16 DESC_BYPASS_CONTROL_WIDTH;
typedef 4 STREAM_FIFO_DEPTH;
typedef 256 STREAM_DATA_WIDTH;
typedef TDiv#(STREAM_DATA_WIDTH, 8) STREAM_KEEP_WIDTH;

Integer bypass_CONTROL_FLAGS = 'h01;

typedef enum {
    CtlRegAddrH2cSourceLow = 'h0000,
    CtlRegAddrH2cSourceHigh = 'h0004,
    CtlRegAddrC2hSourceLow = 'h0008,
    CtlRegAddrC2hSourceHigh = 'h00C,
    CtlRegAddTransSize = 'h0010
} ControlRegisterAddress deriving(Bits, Eq);


interface RegisterBlock#(numeric type controlAddrWidth, numeric type dataStrbWidth);
    interface RawAxi4LiteSlave#(controlAddrWidth, dataStrbWidth) axilRegBlock;
endinterface



module mkRegisterBlock(RegisterBlock#(CONTROL_REG_ADDR_WIDTH, CONTROL_REG_DATA_STRB_WIDTH));
    FIFOF#(Axi4LiteWrAddr#(CONTROL_REG_ADDR_WIDTH)) ctrlWrAddrFifo <- mkFIFOF;
    FIFOF#(Axi4LiteWrData#(CONTROL_REG_DATA_STRB_WIDTH)) ctrlWrDataFifo <- mkFIFOF;
    FIFOF#(Axi4LiteWrResp) ctrlWrRespFifo <- mkFIFOF;
    FIFOF#(Axi4LiteRdAddr#(CONTROL_REG_ADDR_WIDTH)) ctrlRdAddrFifo <- mkFIFOF;
    FIFOF#(Axi4LiteRdData#(CONTROL_REG_DATA_STRB_WIDTH)) ctrlRdDataFifo <- mkFIFOF;


    Reg#(Bit#(HOST_ADDR_WIDTH)) h2cSourceAddress <- mkRegU;
    Reg#(Bit#(HOST_ADDR_WIDTH)) c2hDestAddress <- mkRegU;
    Reg#(Bit#(DESC_BYPASS_LENGTH_WIDTH)) transSize[2] <- mkCReg(2, 0);


    let ctlAxilSlave <- mkPipeToRawAxi4LiteSlave(
        convertFifoToPipeIn(ctrlWrAddrFifo),
        convertFifoToPipeIn(ctrlWrDataFifo),
        convertFifoToPipeOut(ctrlWrRespFifo),

        convertFifoToPipeIn(ctrlRdAddrFifo),
        convertFifoToPipeOut(ctrlRdDataFifo)
    );

    rule readControlCmd if (ctrlWrAddrFifo.notEmpty && ctrlWrDataFifo.notEmpty && transSize[1] == 0);
        ctrlWrAddrFifo.deq;
        ctrlWrDataFifo.deq;
        
        let addr_to_match = unpack(truncate(pack(ctrlWrAddrFifo.first.awAddr)));
        case (addr_to_match) matches
            CtlRegAddrH2cSourceLow: h2cSourceAddress[31:0] <= ctrlWrDataFifo.first.wData;
            CtlRegAddrH2cSourceHigh: h2cSourceAddress[63:32] <= ctrlWrDataFifo.first.wData;
            CtlRegAddrC2hSourceLow: c2hDestAddress[31:0] <= ctrlWrDataFifo.first.wData;
            CtlRegAddrC2hSourceHigh: c2hDestAddress[63:32] <= ctrlWrDataFifo.first.wData;
            CtlRegAddTransSize: begin
                transSize[1] <= truncate(ctrlWrDataFifo.first.wData);
                $display("set size");
            end
            default: begin 
                $display("unknown addr");
            end
        endcase

        ctrlWrRespFifo.enq(0);
    endrule

    rule respondToControlCmdRead;
        ctrlRdAddrFifo.deq;
        ctrlRdDataFifo.enq(Axi4LiteRdData{rResp: 'h0, rData: 'hABCD4321});
    endrule
    
    interface axilRegBlock = ctlAxilSlave;

endmodule
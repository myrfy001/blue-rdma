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
typedef 256 STREAM_DATA_WIDTH;
typedef TDiv#(STREAM_DATA_WIDTH, 8) STREAM_KEEP_WIDTH;


typedef enum {
    ModifyFirstStagePgt = 0,
    ModifySecondStagePgt = 1,
    MaxGuard = 16'hFFFF // padding to make this enum use 8 bit
} RdmaCsrCmdType deriving(Bits);

typedef struct {
    RdmaCsrCmdType cmdType;
    Bit#(16) reqId;
} RdmaCsrCmdTypeAndId deriving(Bits);

typedef struct {
    Bit#(16) finishedReqId;
    Bit#(8)  errorCode;
} RdmaCsrCmdExecuteResponse deriving(Bits);

typedef struct {
    Bit#(CONTROL_REG_DATA_WIDTH) ctlRegCmdSize;
    Bit#(HOST_ADDR_WIDTH) ctlRegCmdAddr;
    RdmaCsrCmdTypeAndId ctlRegCmdTypeAndId;
} RdmaControlCmdEntry deriving(Bits);

typedef 32 CONTROL_REG_DATA_WIDTH;

typedef enum {
    CtlRegIdCmdSize = 'h0000,
    CtlRegIdCmdAddrLow = 'h0001,
    CtlRegIdCmdAddrHigh = 'h0002,
    CtlRegIdCmdTypeAndId = 'h0003,
    CtlRegIdCmdExecuteStatus = 'h0004
} ControlRegisterAddress deriving(Bits, Eq);


interface RegisterBlock#(numeric type controlAddrWidth, numeric type dataStrbWidth);
    interface RawAxi4LiteSlave#(controlAddrWidth, dataStrbWidth) axilRegBlock;
    // method ActionValue#() getPgtRequest();

endinterface



module mkRegisterBlock(RegisterBlock#(CONTROL_REG_ADDR_WIDTH, CONTROL_REG_DATA_STRB_WIDTH)) ;
    FIFOF#(Axi4LiteWrAddr#(CONTROL_REG_ADDR_WIDTH)) ctrlWrAddrFifo <- mkFIFOF;
    FIFOF#(Axi4LiteWrData#(CONTROL_REG_DATA_STRB_WIDTH)) ctrlWrDataFifo <- mkFIFOF;
    FIFOF#(Axi4LiteWrResp) ctrlWrRespFifo <- mkFIFOF;
    FIFOF#(Axi4LiteRdAddr#(CONTROL_REG_ADDR_WIDTH)) ctrlRdAddrFifo <- mkFIFOF;
    FIFOF#(Axi4LiteRdData#(CONTROL_REG_DATA_STRB_WIDTH)) ctrlRdDataFifo <- mkFIFOF;


    Reg#(Bit#(CONTROL_REG_DATA_WIDTH)) ctlRegCmdSize <- mkRegU;
    Reg#(Bit#(HOST_ADDR_WIDTH)) ctlRegCmdAddr <- mkRegU;

    FIFOF#(RdmaControlCmdEntry) pendingCmdQ <- mkFIFOF;
    FIFOF#(RdmaCsrCmdExecuteResponse) pendingCmdRespQ <- mkFIFOF;

    let ctlAxilSlave <- mkPipeToRawAxi4LiteSlave(
        convertFifoToPipeIn(ctrlWrAddrFifo),
        convertFifoToPipeIn(ctrlWrDataFifo),
        convertFifoToPipeOut(ctrlWrRespFifo),
        convertFifoToPipeIn(ctrlRdAddrFifo),
        convertFifoToPipeOut(ctrlRdDataFifo)
    );


    rule handleRegisterWrite;
        ctrlWrAddrFifo.deq;
        ctrlWrDataFifo.deq;
        let addr_to_match = unpack(truncate(pack(ctrlWrAddrFifo.first.awAddr>>2)));
        let data_to_write = ctrlWrDataFifo.first.wData;
        case (addr_to_match) matches
            CtlRegIdCmdSize: ctlRegCmdSize <= data_to_write;
            CtlRegIdCmdAddrLow: ctlRegCmdAddr[31:0] <= data_to_write;
            CtlRegIdCmdAddrHigh: ctlRegCmdAddr[63:32] <= data_to_write;
            CtlRegIdCmdTypeAndId: begin
                if (pendingCmdQ.notFull) begin
                    pendingCmdQ.enq(RdmaControlCmdEntry{
                        ctlRegCmdSize: ctlRegCmdSize,
                        ctlRegCmdAddr: ctlRegCmdAddr,
                        ctlRegCmdTypeAndId: unpack(data_to_write)
                    });
                end
            end 
            default: begin 
                $display("unknown addr");
            end
        endcase

        ctrlWrRespFifo.enq(0);
    endrule

    rule handleRegisterRead;
        ctrlRdAddrFifo.deq;
        let addr_to_match = unpack(truncate(pack(ctrlRdAddrFifo.first.arAddr>>2)));
        Bit#(CONTROL_REG_DATA_WIDTH) outData = 32'hFFFFFFFF;
        case (addr_to_match) matches
            CtlRegIdCmdExecuteStatus: begin
                Bool hasResp = False;
                if (pendingCmdRespQ.notEmpty) begin
                    pendingCmdRespQ.deq;
                    hasResp = True;
                end

                Bool cmdQFull = !pendingCmdQ.notFull;
                outData = {pack(cmdQFull), pack(hasResp), 6'h0 ,pendingCmdRespQ.first.errorCode, pendingCmdRespQ.first.finishedReqId};
            end
            default: begin 
                $display("unknown addr");
            end
        endcase

        ctrlRdDataFifo.enq(Axi4LiteRdData{rResp: 'h0, rData: outData});
    endrule
    
    interface axilRegBlock = ctlAxilSlave;

endmodule
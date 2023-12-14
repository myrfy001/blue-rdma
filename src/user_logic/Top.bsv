import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import Vector :: *;
import Clocks :: * ;
import StmtFSM::*;
import Randomizable :: * ;
import Axi4LiteTypes :: *;
import BRAM :: *;

import DataTypes :: *;
import UserLogicSettings :: *;
import XdmaWrapper :: *;
import RegisterBlock :: *;
// import ControlCmdManager :: *;
import UserLogicTypes :: *;
import AddressTranslate :: *;
import Ringbuf :: *;
import Arbitration :: *;
import UserLogicUtils :: *;
import CmdQueue :: *;

import TransportLayer :: *;


interface BsvTop#(numeric type dataSz, numeric type userSz);
    interface XdmaChannel#(dataSz, userSz) xdmaChannel;
    interface RawAxi4LiteSlave#(CSR_ADDR_WIDTH, CSR_DATA_STRB_WIDTH) axilRegBlock;
    interface Clock slowClockIfc;
endinterface


(* synthesize *)
module mkBsvTop(Clock slowClock, Reset slowReset, BsvTop#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) ifc);
    XdmaWrapper#(USER_LOGIC_XDMA_KEEP_WIDTH, USER_LOGIC_XDMA_TUSER_WIDTH) xdmaWrap <- mkXdmaWrapper(clocked_by slowClock, reset_by slowReset);
    XdmaAxiLiteBridgeWrapper#(CsrAddr, CsrData) xdmaAxiLiteWrap <- mkXdmaAxiLiteBridgeWrapper(slowClock, slowReset);
    BsvTopCore#(CsrAddr, CsrData) bsvTopCore <- mkBsvTopCore(slowClock, slowReset);
    mkConnection(xdmaAxiLiteWrap.csrWriteClt, bsvTopCore.csrWriteSrv);
    mkConnection(xdmaWrap.dmaReadSrv, bsvTopCore.dmaReadClt);
    mkConnection(xdmaWrap.dmaWriteSrv, bsvTopCore.dmaWriteClt);

    interface xdmaChannel = xdmaWrap.xdmaChannel;
    interface slowClockIfc = slowClock;
    interface axilRegBlock = xdmaAxiLiteWrap.cntrlAxil;
endmodule


interface BsvTopCore#(type t_csr_addr, type t_csr_data);
    interface UserLogicDmaReadWideClt dmaReadClt;
    interface UserLogicDmaWriteWideClt dmaWriteClt;
    interface Server#(CsrWriteRequest#(t_csr_addr, t_csr_data), CsrWriteResponse) csrWriteSrv;
    interface Server#(CsrReadRequest#(t_csr_addr), CsrReadResponse#(t_csr_data)) csrReadSrv;
endinterface


// if software and hardware try to move the same pointer, software won.
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_0.recvDmaResp_1" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp" *) 
(* preempts = "regBlock.ruleHandleWrite, ringbufPool.controller_1.recvDmaResp_1" *) 
module mkBsvTopCore(Clock slowClock, Reset slowReset, BsvTopCore#(CsrAddr, CsrData) ifc);
    // TODO, make sure which reset to use.
    BluerdmaDmaProxy bluerdmaDmaProxy <- mkBluerdmaDmaProxy;
    RingbufPool#(RINGBUF_H2C_TOTAL_COUNT, RINGBUF_C2H_TOTAL_COUNT, RingbufRawDescriptor) ringbufPool <- mkRingbufPool;

    RegisterBlock#(CsrAddr, CsrData) regBlock <- mkRegisterBlock(ringbufPool.h2cMetas, ringbufPool.c2hMetas);
    
    
    TLB tlb <- mkTLB;
    PgtManager pgtManager <- mkPgtManager(tlb);

    function Bool isH2cDmaReqFinished(UserLogicDmaH2cReq req) = True;
    function Bool isH2cDmaRespFinished(UserLogicDmaH2cResp resp) = resp.dataStream.isLast;
    function Bool isC2hDmaReqFinished(UserLogicDmaC2hReq req) = req.dataStream.isLast;
    function Bool isC2hDmaRespFinished(UserLogicDmaC2hResp resp) = True;
    
    Vector#(4, RingbufDmaH2cClt) dmaAccessH2cCltVec = newVector;
    Vector#(2, RingbufDmaC2hClt) dmaAccessC2hCltVec = newVector;

    dmaAccessH2cCltVec[0] = bluerdmaDmaProxy.userlogicSideReadClt;
    dmaAccessH2cCltVec[1] = ringbufPool.dmaAccessH2cClt;
    dmaAccessH2cCltVec[2] = pgtManager.pgtDmaReadClt;
    dmaAccessH2cCltVec[3] <- mkFakeClient;

    dmaAccessC2hCltVec[0] = bluerdmaDmaProxy.userlogicSideWriteClt;
    dmaAccessC2hCltVec[1] = ringbufPool.dmaAccessC2hClt;

    BluerdmaDmaProxy blueRdmaProxy <- mkBluerdmaDmaProxy;
    UserLogicDmaReadClt xdmaReadClt <- mkClientArbiter(dmaAccessH2cCltVec, isH2cDmaReqFinished, isH2cDmaRespFinished);
    UserLogicDmaWriteClt xdmaWriteClt <- mkClientArbiter(dmaAccessC2hCltVec, isC2hDmaReqFinished, isC2hDmaRespFinished);

    
    XdmaGearbox xdmaGearbox <- mkXdmaGearbox(slowClock, slowReset);

    mkConnection(xdmaReadClt, xdmaGearbox.h2cStreamSrv);
    mkConnection(xdmaWriteClt, xdmaGearbox.c2hStreamSrv);


    CommandQueueController cmdQController<- mkCommandQueueController;
    mkConnection(toGet(ringbufPool.h2cRings[0]), cmdQController.ringbufSrv.request);
    // TODO: use mkCOnnection for this and the line above
    rule forwardCmdQResponseToRingbuf;
        let t <- cmdQController.ringbufSrv.response.get;
        ringbufPool.c2hRings[0].enq(t);
    endrule

    mkConnection(cmdQController.pgtManagerClt, pgtManager.pgtModifySrv);

    let rdmaTransportLayer <- mkTransportLayer;

    mkConnection(cmdQController.metaDataManagerClt, rdmaTransportLayer.srvPortMetaData);
    
    interface csrWriteSrv = regBlock.csrWriteSrv;
    interface csrReadSrv = regBlock.csrReadSrv;
    interface dmaReadClt = xdmaGearbox.h2cStreamClt;
    interface dmaWriteClt = xdmaGearbox.c2hStreamClt;
endmodule


(*synthesize*)
module mkTbTop(Empty);
    ClockDividerIfc divClk <- mkClockDivider(2);
    Clock slowClock = divClk.slowClock;
    Reset slowReset <- mkInitialReset(1, clocked_by slowClock);

    FakeXdma fakeXdma <- mkFakeXdma(2, tagged Hex "src/bsv/user_logic/test_host_memory.hex", clocked_by slowClock, reset_by slowReset);

    BsvTopCore#(CsrAddr, CsrData) bsvTopCore <- mkBsvTopCore(slowClock, slowReset);
    mkConnection(fakeXdma.xdmaH2cSrv, bsvTopCore.dmaReadClt);
    mkConnection(fakeXdma.xdmaC2hSrv, bsvTopCore.dmaWriteClt);




    Reg#(UInt#(32)) i <- mkReg(0);
    Reg#(Bool) startedUser <- mkReg(False);

    Randomize#(Bit#(13)) startAddrRnd <- mkGenericRandomizer;
    Randomize#(Bit#(8)) lenRnd <- mkGenericRandomizer;

    


    FSM runTest <- mkFSM(
        (seq
        

            bsvTopCore.csrReadSrv.request.put(CsrReadRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2
            });

            action
                let t <- bsvTopCore.csrReadSrv.response.get;
                $display("tail pointer = %x", t);
            endaction


            bsvTopCore.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2,
                data: 7
            });

            action
                let t <- bsvTopCore.csrWriteSrv.response.get;
            endaction

            bsvTopCore.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h1000
            });

            action
                let t <- bsvTopCore.csrWriteSrv.response.get;
            endaction

            bsvTopCore.csrReadSrv.request.put(CsrReadRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2
            });

            action
                let t <- bsvTopCore.csrReadSrv.response.get;
                $display("tail pointer = %x", t);
            endaction

        endseq)
    );
    


    rule doTest if (!startedUser);
        startedUser <= True;
        runTest.start;
    endrule
endmodule
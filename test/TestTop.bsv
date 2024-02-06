import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import PAClib :: *;
import Clocks :: *;
import BRAM :: *;

import PipeIn :: *;
import RQ :: *;
import QPContext :: *;
import MetaData :: *;
import DataTypes :: *;
import InputPktHandle :: *;
import RdmaUtils :: *;
import PrimUtils :: *;
import StmtFSM::*;
import Axi4LiteTypes :: *;

import ExtractAndPrependPipeOut :: *;

import MemRegionAndAddressTranslate :: *;
import PayloadConAndGen :: *;
import RecvStreamMocker :: *;
import Headers :: *;

import XdmaWrapper :: *;
import UserLogicTypes :: *;
import RegisterBlock :: *;

import Top :: *;

`define TEST_QPN_IDX_PART 'h3
`define TEST_QPN_KEY_PART 'h611

`define TEST_MR_IDX_PART 'h0
`define TEST_MR_KEY_PART 'h6622
`define TEST_MR_START_VA 'h0
`define TEST_MR_LENGTH   'h4000000
`define TEST_MR_FIRST_PGT_IDX   'h200

`define TEST_PGT_FIRST_ENTRY_PN 'h000C

`define TEST_PD_HANDLER   'h7890

`define TEST_WR_ADDR `TEST_MR_START_VA + 1
`define TEST_WR_LEN  1023


(* doc = "testcase" *)
module mkTestTop(Empty);

    ClockDividerIfc divClk <- mkClockDivider(2);
    Clock slowClock = divClk.slowClock;
    Reset slowReset <- mkInitialReset(1, clocked_by slowClock);

    RecvStreamMocker rsMocker <- mkRecvStreamMocker;
    TopCoreIfc topA <- mkTopCore(slowClock, slowReset);

    FakeXdma fakeXdmaA <- mkFakeXdma(1, tagged Hex "test_host_memory.hex", clocked_by slowClock, reset_by slowReset);

    mkConnection(fakeXdmaA.xdmaH2cSrv, topA.dmaReadClt);
    mkConnection(fakeXdmaA.xdmaC2hSrv, topA.dmaWriteClt);

    // loop tx stream to rx stream
    mkConnection(toGet(topA.rdmaDataStreamPipeOut), topA.rdmaDataStreamInput);


    Reg#(Bool) stopReg <- mkReg(False);
    Reg#(UInt#(32)) idx <- mkReg(0);


    FSM runTest <- mkFSM(
        (seq
            
            // set cmd queue request ringbuf addr
            // since addr is 0, the default value, we don't need to set it.

            // set cmd queue response ringbuf addr
            topA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h1000
            });

            action
                let t1 <- topA.csrWriteSrv.response.get;
            endaction


            // set send queue ringbuf addr
            topA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 1, regIndex: CsrIdxRbBaseAddrLow})) << 2,
                data: 'h3000
            });

            action
                let t1 <- topA.csrWriteSrv.response.get;
            endaction



            // move cmd queue head to init RDMA
            topA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2,
                data: 3
            });

            action
                let t1 <- topA.csrWriteSrv.response.get;
            endaction


            // read cmd resp queue head pointer to check if all cmd executed
            for (idx <= 0; idx<30; idx<=idx+1)
            seq
                topA.csrReadSrv.request.put(CsrReadRequest{
                    addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 0, regIndex: CsrIdxRbHead})) << 2
                });
                action
                    let t <- topA.csrReadSrv.response.get;
                    $display("t=%d", t);
                endaction
                delay(10);
            endseq


            
            // move cmd queue head to send WQE
            topA.csrWriteSrv.request.put(CsrWriteRequest{
                addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: True, queueIndex: 1, regIndex: CsrIdxRbHead})) << 2,
                data: 4
            });

            action
                let t1 <- topA.csrWriteSrv.response.get;
            endaction


            // read RQ's metadata report queue's head pointer to check if we received req;
            for (idx <= 0; idx<30; idx<=idx+1)
            seq
                topA.csrReadSrv.request.put(CsrReadRequest{
                    addr: zeroExtend(pack(CsrRingbufRegsAddress{isH2c: False, queueIndex: 1, regIndex: CsrIdxRbHead})) << 2
                });
                action
                    let t <- topA.csrReadSrv.response.get;
                    $display("t=%d", t);
                endaction
                delay(10);
            endseq

            delay(20000);
            $display("pass");
            $finish;

        endseq)
    );



    rule putOneReq if (!stopReg);
        stopReg <= True;

        runTest.start;
    endrule

        


endmodule
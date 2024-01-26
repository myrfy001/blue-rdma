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


import Top :: *;

`define TEST_QPN_IDX_PART 'h1
`define TEST_QPN_KEY_PART 'h2

`define TEST_MR_IDX_PART 'h11
`define TEST_MR_KEY_PART 'h22
`define TEST_MR_START_VA 'hAABBCCDD
`define TEST_MR_LENGTH   1026
`define TEST_MR_FIRST_PGT_IDX   2048

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

    mkConnection(toGet(rsMocker.streamPipeOut), topA.rdmaDataStreamInput);


    Reg#(Bool) stopReg <- mkReg(False);


    FSM runTest <- mkFSM(
        (seq
            
            // Insert QPC
            topA.qpcWriteCommonSrv.request.put(QPCWriteReqCommon{
                    qpn: genQPN(`TEST_QPN_IDX_PART, `TEST_QPN_KEY_PART),
                    ent: tagged Valid QPCEntryCommon {
                        isValid: True,
                        isError: False,
                        qpnKeyPart: `TEST_QPN_KEY_PART,
                        pdHandler: `TEST_PD_HANDLER,
                        qpType: IBV_QPT_RC,
                        rqAccessFlags: enum2Flag(IBV_ACCESS_REMOTE_WRITE),
                        pmtu: IBV_MTU_4096
                    }
            });
            action
                let _ <- topA.qpcWriteCommonSrv.response.get;
            endaction

            // // Insert into MR table
            // topA.mrModifySrv.request.put(MrTableModifyReq{
            //     idx: `TEST_MR_IDX_PART,
            //     entry: tagged Valid MemRegionTableEntry{
            //         pgtOffset: `TEST_MR_FIRST_PGT_IDX,
            //         baseVA: `TEST_MR_START_VA,
            //         len: `TEST_MR_LENGTH,
            //         accFlags: enum2Flag(IBV_ACCESS_REMOTE_WRITE),
            //         pdHandler: `TEST_PD_HANDLER,
            //         keyPart: `TEST_MR_KEY_PART
            //     }
            // });
            // action
            //     let _ <- topA.mrModifySrv.response.get;
            // endaction


            // // Insert into PGT
            // topA.mrAndPgtModifyDescSrv.request.put(PgtModifyReq{
            //     idx: `TEST_MR_FIRST_PGT_IDX,
            //     pte: PageTableEntry{
            //         pn: `TEST_PGT_FIRST_ENTRY_PN
            //     }
            // });
            // action
            //     let _ <- topA.mrAndPgtModifyDescSrv.response.get;
            // endaction

            action
                MockHeaderStream ms = unpack(0);
                BTH bth = ?;
                bth.trans = TRANS_TYPE_RC;
                bth.opcode = RDMA_WRITE_FIRST;
                bth.dqpn = genQPN(`TEST_QPN_IDX_PART, `TEST_QPN_KEY_PART);

                RETH reth = ?;
                reth.dlen = `TEST_WR_LEN;
                reth.rkey = rkeyFromKeyAndIndexPart(`TEST_MR_IDX_PART, `TEST_MR_KEY_PART);
                reth.va = `TEST_WR_ADDR;

                ms = appendSegToMockHeaderStream(ms, bth);
                ms = appendSegToMockHeaderStream(ms, reth);
                ms = addPayloadToMockHeaderStream(ms, 16'hABCD, `TEST_WR_LEN);
                ms = fillBthOfMockedHeaderStream(ms);

                rsMocker.addDataToMock(ms);
            endaction


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
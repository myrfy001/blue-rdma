import Connectable :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFOF :: *;
import PAClib :: *;
import Clocks :: *;
import BRAM :: *;

import RQ :: *;
import QPContext :: *;
import MetaData :: *;
import DataTypes :: *;
import InputPktHandle :: *;
import RdmaUtils :: *;
import PrimUtils :: *;
import StmtFSM::*;
import Axi4LiteTypes :: *;
import SemiFifo :: *;

import ExtractAndPrependPipeOut :: *;

import MemRegionAndAddressTranslate :: *;
import PayloadCon :: *;
import RecvStreamMocker :: *;
import Headers :: *;
import Ports :: *;
import StreamHandler :: *;

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
module mkTestMockXdma(Empty);



    FakeXdma fakeXdmaA <- mkFakeXdma(1);
    Reg#(Bit#(2)) cnt <- mkReg(0);


    rule doDmaReadReq;
        $display("time=%0t:", $time, "do DMA Read Req");
        fakeXdmaA.xdmaH2cSrv.request.put(
            UserLogicDmaH2cReq{
                addr: 0,
                len: 256
            } 
        );
    endrule

    rule doDmaReadResp;
        $display("time=%0t:", $time, "do DMA Read Resp");
        let _ <- fakeXdmaA.xdmaH2cSrv.response.get;
    endrule

    rule doDmaWriteReq;
        $display("time=%0t:", $time, "do DMA Write Req");
        cnt <= cnt+ 1;
        fakeXdmaA.xdmaC2hSrv.request.put(
            UserLogicDmaC2hWideReq {
                addr: 0,
                len: 256,
                dataStream: DataStreamWide{
                    isFirst : cnt == 0,
                    isLast : cnt == 3,
                    data: ?,
                    byteEn: -1
                }
            }
        );
    endrule

    rule doDmaWriteResp;
        $display("time=%0t:", $time, "do DMA Write Resp");
        let _ <- fakeXdmaA.xdmaC2hSrv.response.get;
    endrule

endmodule
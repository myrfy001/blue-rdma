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
import UdpIpEthCmacRxTx::*;

import ExtractAndPrependPipeOut :: *;

import MemRegionAndAddressTranslate :: *;
import PayloadCon :: *;
import Headers :: *;
import Ports :: *;
import StreamHandler :: *;

import XdmaWrapper :: *;
import UserLogicTypes :: *;
import RegisterBlock :: *;
import EthernetTypes :: *;


import Top :: *;
import SendQ::*;

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

    Clock rdmaClock <- exposeCurrentClock;
    Reset rdmaReset <- exposeCurrentReset;

    Clock dmacClock <- exposeCurrentClock;
    Reset dmacReset <- exposeCurrentReset;

    Clock udpClock <- exposeCurrentClock;
    Reset udpReset <- exposeCurrentReset;

    Clock cmacRxTxClk <- mkAbsoluteClock(0, 7);  // about 322 MHz
    // Clock cmacRxTxClk <- mkAbsoluteClock(0, 10);
    Reset cmacRxTxRst <- mkSyncReset(2, udpReset, cmacRxTxClk);

    RdmaUserLogicWithoutXdmaAndCmacWrapper topA <- mkRdmaUserLogicWithoutXdmaAndCmacWrapper(udpClock, udpReset, dmacClock, dmacReset);

    FakeXdma fakeXdmaA <- mkFakeXdma(1, cmacRxTxClk, cmacRxTxRst);

    mkConnection(fakeXdmaA.xdmaH2cSrv, topA.dmaReadClt);
    mkConnection(fakeXdmaA.xdmaC2hSrv, topA.dmaWriteClt);

    SyncFIFOIfc#(CsrAddr) csrReadReqSyncFifo <- mkSyncFIFO(2, dmacClock, dmacReset, rdmaClock);
    mkConnection(fakeXdmaA.barReadClt.request, toPut(csrReadReqSyncFifo)); 

    SyncFIFOIfc#(CsrData) csrReadRespSyncFifo <- mkSyncFIFO(2, rdmaClock, rdmaReset, dmacClock);
    mkConnection(toGet(csrReadRespSyncFifo), fakeXdmaA.barReadClt.response); 

    SyncFIFOIfc#(Tuple2#(CsrAddr, CsrData)) csrWriteReqSyncFifo <- mkSyncFIFO(2, dmacClock, dmacReset, rdmaClock);
    mkConnection(fakeXdmaA.barWriteClt.request, toPut(csrWriteReqSyncFifo)); 

    SyncFIFOIfc#(Bool) csrWriteRespSyncFifo <- mkSyncFIFO(2, rdmaClock, rdmaReset, dmacClock);
    mkConnection(toGet(csrWriteRespSyncFifo), fakeXdmaA.barWriteClt.response); 


// `define __TXRX_MODEL_LOOPBACK_DELAYED
// `define __TXRX_MODEL_LOOPBACK_NONE_DELAY True
`define __TXRX_MODEL_CONNECT_TO_MOCK_HOST True

`ifdef __TXRX_MODEL_LOOPBACK_DELAYED

    // loop tx stream to rx stream with delay

    FIFOF#(AxiStream512) delayQ <- mkSizedFIFOF(8192);
    // loop tx stream to rx stream with delayed time so they won't interfere eachother.
    rule bufferTxStream;
        let d = topA.axiStreamTxOutUdp.first;
        topA.axiStreamTxOutUdp.deq;
        delayQ.enq(d);
        $display("time=%0t: ", $time, "udp send data and put to buffer ");
    endrule

    rule outputTxStream;
        let t <- $time;
        if (t > 16890) begin
            let d = delayQ.first;
            delayQ.deq;
            topA.axiStreamRxInUdp.put(d);
            $display("time=%0t: ", $time, "deq data from delayQ");
        end
    endrule

`elsif __TXRX_MODEL_LOOPBACK_NONE_DELAY

    // loop tx stream to rx stream without delay
    rule displayAndForwardWireData;
        let d = topA.axiStreamTxOutUdp.first;
        topA.axiStreamTxOutUdp.deq;
        topA.axiStreamRxInUdp.put(d);
        $display("time=%0t: ", $time, "udp send data: ", fshow(d));
    endrule

`elsif __TXRX_MODEL_CONNECT_TO_MOCK_HOST

    // connect rx and tx to MockHost

    SyncFIFOIfc#(AxiStream512) netIfcRxSyncFifo <- mkSyncFIFO(32, cmacRxTxClk, cmacRxTxRst, udpClock);
    mkConnection(fakeXdmaA.axiStreamRxUdp, toPut(netIfcRxSyncFifo), clocked_by cmacRxTxClk, reset_by cmacRxTxRst);
    mkConnection(toGet(netIfcRxSyncFifo), topA.axiStreamRxInUdp);

    SyncFIFOIfc#(AxiStream512) netIfcTxSyncFifo <- mkSyncFIFO(32, udpClock, udpReset, cmacRxTxClk);
    // mkConnection(toGet(topA.axiStreamTxOutUdp), toPut(netIfcTxSyncFifo));
    rule forwardUdpSendStream;
        let txData = topA.axiStreamTxOutUdp.first;
        topA.axiStreamTxOutUdp.deq;
        netIfcTxSyncFifo.enq(txData);
        $display("time=%0t: ", $time, "forward udp send data to sync FIFO of CMAC");
    endrule
    mkConnection(convertSyncFifoToFifoOut(netIfcTxSyncFifo), fakeXdmaA.axiStreamTxUdp, clocked_by cmacRxTxClk, reset_by cmacRxTxRst); 

`endif
    rule forwardBarReadReq;
        csrReadReqSyncFifo.deq;
        let inReq = csrReadReqSyncFifo.first;
        let outReq = CsrReadRequest{addr: inReq};
        topA.csrReadSrv.request.put(outReq);
        // $display("csr read req =", fshow(outReq));
    endrule

    rule forwardBarReadResp;
        let inResp <- topA.csrReadSrv.response.get;
        let outResp = inResp.data;
        csrReadRespSyncFifo.enq(outResp);
        // $display("csr read resp =", fshow(outResp));
    endrule

    rule forwardBarWriteReq;
        csrWriteReqSyncFifo.deq;
        let inReq = csrWriteReqSyncFifo.first;
        let outReq = CsrWriteRequest{addr: tpl_1(inReq), data: tpl_2(inReq)};
        topA.csrWriteSrv.request.put(outReq);
        // $display("csr write req = ", fshow(outReq));
    endrule

    rule forwardBarWriteResp;
        let inResp <- topA.csrWriteSrv.response.get;
        let outResp = True;
        csrWriteRespSyncFifo.enq(outResp);
    endrule

    Reg#(Bool) stopReg <- mkReg(False);
    Reg#(UInt#(32)) idx <- mkReg(0);

endmodule




(* doc = "testcase" *)
module mkTestRdmaAndUserLogicWithoutUdp(Empty);

    ClockDividerIfc divClk <- mkClockDivider(2);
    Clock slowClock = divClk.slowClock;
    Reset slowReset <- mkInitialReset(1, clocked_by slowClock);
    Clock fastClock <- exposeCurrentClock;
    Reset fastReset <- exposeCurrentReset;

    Clock cmacRxTxClk <- mkAbsoluteClock(0, 7);  // about 322 MHz
    // Clock cmacRxTxClk <- mkAbsoluteClock(0, 10);
    Reset cmacRxTxRst <- mkSyncReset(2, fastReset, cmacRxTxClk);

    RdmaUserLogicWithoutXdmaAndUdpCmacWrapper topA <- mkRdmaUserLogicWithoutXdmaAndUdpCmacWrapper(slowClock, slowReset);

    FakeXdma fakeXdmaA <- mkFakeXdma(1, cmacRxTxClk, cmacRxTxRst, clocked_by slowClock, reset_by slowReset);

    mkConnection(fakeXdmaA.xdmaH2cSrv, topA.dmaReadClt);
    mkConnection(fakeXdmaA.xdmaC2hSrv, topA.dmaWriteClt);

    SyncFIFOIfc#(CsrAddr) csrReadReqSyncFifo <- mkSyncFIFO(2, slowClock, slowReset, fastClock);
    mkConnection(fakeXdmaA.barReadClt.request, toPut(csrReadReqSyncFifo), clocked_by slowClock, reset_by slowReset); 

    SyncFIFOIfc#(CsrData) csrReadRespSyncFifo <- mkSyncFIFO(2, fastClock, fastReset, slowClock);
    mkConnection(toGet(csrReadRespSyncFifo), fakeXdmaA.barReadClt.response, clocked_by slowClock, reset_by slowReset); 

    SyncFIFOIfc#(Tuple2#(CsrAddr, CsrData)) csrWriteReqSyncFifo <- mkSyncFIFO(2, slowClock, slowReset, fastClock);
    mkConnection(fakeXdmaA.barWriteClt.request, toPut(csrWriteReqSyncFifo), clocked_by slowClock, reset_by slowReset); 

    SyncFIFOIfc#(Bool) csrWriteRespSyncFifo <- mkSyncFIFO(2, fastClock, fastReset, slowClock);
    mkConnection(toGet(csrWriteRespSyncFifo), fakeXdmaA.barWriteClt.response, clocked_by slowClock, reset_by slowReset); 


`define __LOOP_WITH_DELAY_QUEUE True;

`ifdef __LOOP_WITH_DELAY_QUEUE

        FIFOF#(RqDataStreamWithExtraInfo) delayQ <- mkSizedFIFOF(8192);
        // loop tx stream to rx stream with delayed time so they won't interfere eachother.
        rule bufferTxStream;
            
            let data = topA.sqRdmaDataStreamPipeOut.first;
            topA.sqRdmaDataStreamPipeOut.deq;
            let isRawPkt = topA.sqUdpInfoPipeOut.first.isRawPkt;
            let outData = tuple3(data, isRawPkt, 0);
            delayQ.enq(outData);

            if (data.isLast) begin
                topA.sqUdpInfoPipeOut.deq;
            end
            $display("time=%0t: ", $time,"udp put to delayQ = ", fshow(outData));
        endrule

        rule outputTxStream;
            let t <- $time;
            if (t > 17890) begin
                let d = delayQ.first;
                delayQ.deq;
                topA.rqInputDataStream.put(d);
                $display("time=%0t: ", $time, "delayQ put to rqWrapper");
            end
        endrule
`else
        // loop tx stream to rx stream
        rule displayAndForwardWireData;
            let d = topA.axiStreamTxOutUdp.first;
            topA.axiStreamTxOutUdp.deq;
            topA.axiStreamRxInUdp.put(d);
            $display("time=%0t: ", $time, "udp send data: ", fshow(d));
        endrule
`endif



    rule forwardBarReadReq;
        csrReadReqSyncFifo.deq;
        let inReq = csrReadReqSyncFifo.first;
        let outReq = CsrReadRequest{addr: inReq};
        topA.csrReadSrv.request.put(outReq);
        // $display("csr read req =", fshow(outReq));
    endrule

    rule forwardBarReadResp;
        let inResp <- topA.csrReadSrv.response.get;
        let outResp = inResp.data;
        csrReadRespSyncFifo.enq(outResp);
        // $display("csr read resp =", fshow(outResp));
    endrule

    rule forwardBarWriteReq;
        csrWriteReqSyncFifo.deq;
        let inReq = csrWriteReqSyncFifo.first;
        let outReq = CsrWriteRequest{addr: tpl_1(inReq), data: tpl_2(inReq)};
        topA.csrWriteSrv.request.put(outReq);
        // $display("csr write req = ", fshow(outReq));
    endrule

    rule forwardBarWriteResp;
        let inResp <- topA.csrWriteSrv.response.get;
        let outResp = True;
        csrWriteRespSyncFifo.enq(outResp);
    endrule

    Reg#(Bool) stopReg <- mkReg(False);
    Reg#(UInt#(32)) idx <- mkReg(0);

endmodule



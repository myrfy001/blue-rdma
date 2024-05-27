import GetPut :: *;
import ClientServer :: *;
import Vector :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import FIFOF :: *;


function Action immAssert(Bool condition, String assertName, Fmt assertFmtMsg);
    action
        let pos = printPosition(getStringPosition(assertName));
        // let pos = printPosition(getEvalPosition(condition));
        if (!condition) begin
            $error(
                "ImmAssert failed in %m @time=%0t: %s-- %s: ",
                $time, pos, assertName, assertFmtMsg
            );
            $finish(1);
        end
    endaction
endfunction



typedef struct {
    tAddr addr;
    tValue value;
    Bool isWrite;
} CsrReadWriteReq#(type tAddr, type tValue) deriving(Bits, FShow);

typedef struct {
    Maybe#(tValue) value;
} CsrReadWriteResp#(type tValue) deriving(Bits, FShow);



typedef Server#(CsrReadWriteReq#(tAddr, tValue), CsrReadWriteResp#(tValue)) CsrReadWriteSrvIfc#(type tAddr, type tValue);
typedef Client#(CsrReadWriteReq#(tAddr, tValue), CsrReadWriteResp#(tValue)) CsrReadWriteCltIfc#(type tAddr, type tValue);




interface CsrSwitch#(type tAddr, type tValue, type downStreamPortCnt);
    interface CsrReadWriteSrvIfc#(tAddr, tValue) busInputSrv;
    interface Vector#(downStreamPortCnt, CsrReadWriteCltIfc#(tAddr, tValue)) busOutputCltVecIfc;
endinterface


interface PutToGetProxy#(type tData);
    interface Put#(tData) in;
    interface Get#(tData) out;
endinterface

module mkPutToGetProxy(PutToGetProxy#(tData)) provisos (
    Bits#(tData, szData)
);

    Wire#(tData) relayWire <- mkWire;

    interface Put in;
        
        method Action put(tData value);
            relayWire <= value;
        endmethod
    endinterface

    interface Get out;
        method ActionValue#(tData) get;
            return relayWire;
        endmethod
    endinterface
endmodule



module mkCombinationalCsrSwitch(CsrSwitch#(tAddr, tValue, downStreamPortCnt)) provisos (
    Bits#(tAddr, szAddr),
    Bits#(tValue, szValue)
);

    Wire#(CsrReadWriteReq#(tAddr, tValue)) reqInputRelayWire <- mkWire;
    Vector#(downStreamPortCnt, PutToGetProxy#(CsrReadWriteReq#(tAddr, tValue))) reqRelayVec <- replicateM(mkPutToGetProxy);
    Vector#(downStreamPortCnt, PutToGetProxy#(CsrReadWriteResp#(tValue))) respRelayVec <- replicateM(mkPutToGetProxy);
    Vector#(downStreamPortCnt, CsrReadWriteCltIfc#(tAddr, tValue)) busOutputCltVec = newVector;

    for (Integer idx = 0; idx < valueOf(downStreamPortCnt); idx = idx + 1) begin
        busOutputCltVec[idx] = interface CsrReadWriteCltIfc#(tAddr, tValue)
            interface request = reqRelayVec[idx].out;
            interface response = respRelayVec[idx].in;
        endinterface;
    end

    interface CsrReadWriteSrvIfc busInputSrv;
        interface Put request;
            method Action put(CsrReadWriteReq#(tAddr, tValue) req);
                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    reqRelayVec[portIdx].in.put(req);
                end
            endmethod
        endinterface

        interface Get response;
            method ActionValue#(CsrReadWriteResp#(tValue)) get;
                Bool foundValidResp = False;
                CsrReadWriteResp#(tValue) finalResp = CsrReadWriteResp{value: tagged Invalid};

                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    let resp <- respRelayVec[portIdx].out.get;
                    if (isValid(resp.value)) begin
                        immAssert(
                            !foundValidResp,
                            "More than one CSR generate response to the same address @ mkPipelineCsrSwitch",
                            $format("port index = %x", portIdx)
                        );

                        foundValidResp = True;
                        finalResp = resp;
                    end
                end
                return finalResp;
            endmethod
        endinterface
    endinterface

    interface busOutputCltVecIfc = busOutputCltVec;
endmodule


module mkPipelineCsrSwitch(CsrSwitch#(tAddr, tValue, downStreamPortCnt)) provisos (
        Bits#(tAddr, szAddr),
        Bits#(tValue, szValue)
    );

    Vector#(downStreamPortCnt, FIFOF#(CsrReadWriteReq#(tAddr, tValue))) reqRelayVec <- replicateM(mkPipelineFIFOF);
    Vector#(downStreamPortCnt, FIFOF#(CsrReadWriteResp#(tValue))) respRelayVec <- replicateM(mkPipelineFIFOF);
    Vector#(downStreamPortCnt, CsrReadWriteCltIfc#(tAddr, tValue)) busOutputCltVec = newVector;

    for (Integer idx = 0; idx < valueOf(downStreamPortCnt); idx = idx + 1) begin
        busOutputCltVec[idx] = toGPClient(reqRelayVec[idx], respRelayVec[idx]);
    end

    interface CsrReadWriteSrvIfc busInputSrv;
        interface Put request;
            method Action put(CsrReadWriteReq#(tAddr, tValue) req);
                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    reqRelayVec[portIdx].enq(req);
                end
            endmethod
        endinterface

        interface Get response;
            method ActionValue#(CsrReadWriteResp#(tValue)) get;
                Bool foundValidResp = False;
                CsrReadWriteResp#(tValue) finalResp = CsrReadWriteResp{value: tagged Invalid};

                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    let resp = respRelayVec[portIdx].first; 
                    respRelayVec[portIdx].deq;
                    if (isValid(resp.value)) begin
                        immAssert(
                            !foundValidResp,
                            "More than one CSR generate response to the same address @ mkPipelineCsrSwitch",
                            $format("port index = %x", portIdx)
                        );

                        foundValidResp = True;
                        finalResp = resp;
                    end
                end
                return finalResp;
            endmethod

        endinterface
    endinterface

    interface busOutputCltVecIfc = busOutputCltVec;


endmodule

interface CsrLeaf#(type tAddr, type tValue);
    interface CsrReadWriteSrvIfc#(tAddr, tValue) busInputSrv;

    method Action _write (tValue value);
    method tValue _read;
    method ActionValue#(tValue) readWithSideEffecct;
endinterface

module mkCsrLeaf#(Integer myAddr) (CsrLeaf#(tAddr, tValue)) provisos (
    Bits#(tAddr, szAddr),
    Bits#(tValue, szData),
    Literal#(tAddr),
    Eq#(tAddr)
);

    Reg#(tValue) storageReg <- mkReg(unpack(0));
    RWire#(tValue) userWriteReqWire <- mkRWire;
    RWire#(tValue) busWriteReqWire <- mkRWire;

    PulseWire busReqIsReadWire <- mkPulseWire;
    PulseWire busReqOccured <- mkPulseWire;



    rule arbitUserAndBusWrite;
        if (busWriteReqWire.wget matches tagged Valid .value) begin
            storageReg <= value;
        end
        else if (userWriteReqWire.wget matches tagged Valid .value) begin
            storageReg <= value;
        end
    endrule

    method Action _write (tValue value);
        userWriteReqWire.wset(value);
    endmethod
    method ActionValue#(tValue) readWithSideEffecct;
        return storageReg;
    endmethod
    method tValue _read;
        return storageReg;
    endmethod

    interface CsrReadWriteSrvIfc busInputSrv;
        interface Put request;
            method Action put(CsrReadWriteReq#(tAddr, tValue) req);
                busReqOccured.send;

                Bool isAddrHit = req.addr == fromInteger(myAddr);

                $display("leaf node get req, addr=%x", req.addr, "my addr=%x", myAddr);

                if (isAddrHit) begin
                    if (req.isWrite) begin
                        busWriteReqWire.wset(req.value);
                    end
                    else begin
                        busReqIsReadWire.send;
                    end
                end
            endmethod
        endinterface

        interface Get response;

            method ActionValue#(CsrReadWriteResp#(tValue)) get if (busReqOccured);
                if (busReqIsReadWire) begin
                    return CsrReadWriteResp{value: tagged Valid storageReg};
                end
                else begin
                    return CsrReadWriteResp{value: tagged Invalid};
                end
            endmethod
        endinterface
    endinterface

endmodule

typedef 1 FirstLevelSwitchInstCnt;
typedef 2 FirstLevelSwitchPortCnt;

typedef 2 SecondLevelSwitchPortCnt;
typedef TMul#(FirstLevelSwitchInstCnt, FirstLevelSwitchPortCnt) SecondLevelSwitchInstCnt;  // 2

typedef 2 ThirdOneLevelSwitchPortCnt;
typedef 2 ThirdTwoLevelSwitchPortCnt;
typedef TMul#(SecondLevelSwitchInstCnt, SecondLevelSwitchPortCnt) ThirdLevelTotalSwitchInstCnt;  // 4
typedef 2 ThirdOneLevelSwitchInstCnt;
typedef TSub#(ThirdLevelTotalSwitchInstCnt, ThirdOneLevelSwitchInstCnt) ThirdTwoLevelSwitchInstCnt;  // 2
typedef TMul#(ThirdOneLevelSwitchInstCnt, ThirdOneLevelSwitchPortCnt) ThirdOneLevelTotalSwitchPortCnt;  // 4
typedef ThirdOneLevelTotalSwitchPortCnt ThirdLevelPartOneLeafCsrInstCnt;  // 4

typedef 2 FourthLevelSwitchPortCnt;
typedef TMul#(ThirdTwoLevelSwitchInstCnt, ThirdTwoLevelSwitchPortCnt) FourthLevelSwitchInstCnt;  // 4
typedef TMul#(FourthLevelSwitchInstCnt, FourthLevelSwitchPortCnt) FourthLevelTotalSwitchPortCnt;  // 8


typedef TAdd#(ThirdLevelPartOneLeafCsrInstCnt, FourthLevelTotalSwitchPortCnt) TotalLeafCsrInstCnt;  // 12

import StmtFSM :: *;

module mkTb(Empty);
    
    CsrSwitch#(Bit#(32), Bit#(32), FirstLevelSwitchPortCnt) firstLevelSwitch <- mkPipelineCsrSwitch;
    Vector#(SecondLevelSwitchInstCnt, CsrSwitch#(Bit#(32), Bit#(32), SecondLevelSwitchPortCnt)) secondLevelSwitches <- replicateM(mkPipelineCsrSwitch);
    Vector#(ThirdOneLevelSwitchInstCnt, CsrSwitch#(Bit#(32), Bit#(32), ThirdOneLevelSwitchPortCnt)) thirdOneLevelSwitches <- replicateM(mkCombinationalCsrSwitch);
    Vector#(ThirdTwoLevelSwitchInstCnt, CsrSwitch#(Bit#(32), Bit#(32), ThirdTwoLevelSwitchPortCnt)) thirdTwoLevelSwitches <- replicateM(mkPipelineCsrSwitch);
    Vector#(FourthLevelSwitchInstCnt, CsrSwitch#(Bit#(32), Bit#(32), FourthLevelSwitchPortCnt)) fourthLevelSwitches <- replicateM(mkPipelineCsrSwitch);


    Vector#(TotalLeafCsrInstCnt, CsrLeaf#(Bit#(32), Bit#(32))) csrLeaves = newVector;
    for (Integer idx = 0; idx < valueOf(TotalLeafCsrInstCnt); idx = idx + 1) begin
        csrLeaves[idx] <- mkCsrLeaf(idx);
    end

    // connect level2 to level1
    for (Integer l2Idx = 0; l2Idx < valueOf(SecondLevelSwitchInstCnt); l2Idx = l2Idx + 1) begin
        mkConnection(firstLevelSwitch.busOutputCltVecIfc[l2Idx], secondLevelSwitches[l2Idx].busInputSrv);
    end

    // connect level3 to level2
    for (Integer l3Idx = 0; l3Idx < valueOf(ThirdLevelTotalSwitchInstCnt); l3Idx = l3Idx + 1) begin
        let l2Idx = l3Idx / valueOf(SecondLevelSwitchPortCnt);
        let l2Offset = l3Idx % valueOf(SecondLevelSwitchPortCnt);
        if (l3Idx < valueOf(ThirdOneLevelSwitchInstCnt)) begin
            mkConnection(secondLevelSwitches[l2Idx].busOutputCltVecIfc[l2Offset], thirdOneLevelSwitches[l3Idx].busInputSrv);
        end
        else begin
            mkConnection(secondLevelSwitches[l2Idx].busOutputCltVecIfc[l2Offset], thirdTwoLevelSwitches[l3Idx-valueOf(ThirdOneLevelSwitchInstCnt)].busInputSrv); 
        end
    end

    // connect level4 to level3 part 2
    for (Integer l4Idx = 0; l4Idx < valueOf(FourthLevelSwitchInstCnt); l4Idx = l4Idx + 1) begin
        let l3Idx = l4Idx / valueOf(ThirdTwoLevelSwitchPortCnt);
        let l3Offset = l4Idx % valueOf(ThirdTwoLevelSwitchPortCnt);
        mkConnection(thirdTwoLevelSwitches[l3Idx].busOutputCltVecIfc[l3Offset], fourthLevelSwitches[l4Idx].busInputSrv);
     
    end

    // connect leaf to switch
    for (Integer leafIdx = 0; leafIdx < valueOf(TotalLeafCsrInstCnt); leafIdx = leafIdx + 1) begin
        if (leafIdx < valueOf(ThirdLevelPartOneLeafCsrInstCnt)) begin
            let switchIdx = leafIdx / valueOf(ThirdOneLevelSwitchPortCnt);
            let switchOffset = leafIdx % valueOf(ThirdOneLevelSwitchPortCnt);
            mkConnection(thirdOneLevelSwitches[switchIdx].busOutputCltVecIfc[switchOffset], csrLeaves[leafIdx].busInputSrv);
        end
        else begin
            let switchIdx = (leafIdx - valueOf(ThirdLevelPartOneLeafCsrInstCnt)) / valueOf(FourthLevelSwitchPortCnt);
            let switchOffset = (leafIdx - valueOf(ThirdLevelPartOneLeafCsrInstCnt)) % valueOf(FourthLevelSwitchPortCnt);
            mkConnection(fourthLevelSwitches[switchIdx].busOutputCltVecIfc[switchOffset], csrLeaves[leafIdx].busInputSrv);
        end
    end


    Reg#(Bool) testStartFlagReg <- mkReg(True);
    Reg#(Bit#(32)) idxReg <- mkReg(0);
    Stmt stmt = seq
        for (idxReg <= 0; idxReg < fromInteger(valueOf(TotalLeafCsrInstCnt)); idxReg <= idxReg + 1)
            seq
                firstLevelSwitch.busInputSrv.request.put(CsrReadWriteReq{ addr: idxReg, value: idxReg, isWrite: True });
                action
                    let resp <- firstLevelSwitch.busInputSrv.response.get;
                    $display("resp = ", fshow(resp));
                endaction
            endseq

        for (idxReg <= 0; idxReg < fromInteger(valueOf(TotalLeafCsrInstCnt)); idxReg <= idxReg + 1)
            seq
                firstLevelSwitch.busInputSrv.request.put(CsrReadWriteReq{ addr: idxReg, value: ?, isWrite: False });
                action
                    let resp <- firstLevelSwitch.busInputSrv.response.get;
                    $display("resp = ", fshow(resp));
                endaction
            endseq
    endseq;
    FSM testFsm <- mkFSM(stmt);

    rule doTest if (testStartFlagReg);
        testFsm.start;
        testStartFlagReg <= False;
    endrule




    // rule getVal;
    //     let ret <- dut.busInputSrv.response.get;
    //     $display("read_ret=%d  %d", isValid(ret.value), fromMaybe(0, ret.value));
    //     // $display("[1]=%d, [2]=%d, [3]=%d", csr1, csr2, csr3);
    // endrule
 endmodule
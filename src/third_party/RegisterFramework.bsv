import GetPut :: *;
import ClientServer :: *;
import Vector :: *;
import SpecialFIFOs :: *;
import Connectable :: *;

typedef struct {
    tAddr addr;
    tValue value;
    Bool isWrite;
} CsrReadWriteReq#(type tAddr, type tValue) deriving(Bits, FShow);

typedef struct {
    Maybe#(tValue) value;
} CsrReadWriteResp#(type tValue) deriving(Bits, FShow);


// interface GetR#(type tData);
//     method Bool ready;
//     method ActionValue#(tData) get;
// endinterface

// interface PutR#(type tData);
//     method Bool ready;
//     method Action put(tData value);
// endinterface

// interface ServerR#(type tReq, type tResp);
//     interface PutR#(tReq) request;
//     interface GetR#(tResp) response;
// endinterface

// interface ClientR#(type tReq, type tResp);
//     interface GetR#(tReq) request;
//     interface PutR#(tResp) response;
// endinterface


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
    PulseWire putCalledWire <- mkPulseWire;

    interface Put in;
        // method Bool ready = True;
        
        method Action put(tData value);
            relayWire <= value;
            putCalledWire.send;
        endmethod
    endinterface

    interface Get out;
        // method Bool ready = putCalledWire;
        
        method ActionValue#(tData) get;
            return relayWire;
        endmethod
    endinterface
endmodule



module mkCombinationalSwitch(CsrSwitch#(tAddr, tValue, downStreamPortCnt)) provisos (
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


    // function Bool isAllDownstreamPortRespReady;
    //     Bool ret = True;
    //     for (Integer idx = 0; idx < valueOf(downStreamPortCnt); idx = idx + 1) begin
    //         if (!busOutputCltVec[idx].response.ready) begin
    //             ret = False;
    //         end
    //     end
    //     return ret;
    // endfunction


    interface CsrReadWriteSrvIfc busInputSrv;
        interface Put request;
            method Action put(CsrReadWriteReq#(tAddr, tValue) req);
                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    reqRelayVec[portIdx].in.put(req);
                end
            endmethod
            // method Bool ready = True;
        endinterface

        interface Get response;

            method ActionValue#(CsrReadWriteResp#(tValue)) get;
                Bool foundValidResp = False;
                CsrReadWriteResp#(tValue) finalResp = CsrReadWriteResp{value: tagged Invalid};

                for (Integer portIdx = 0; portIdx < valueOf(downStreamPortCnt); portIdx = portIdx + 1) begin
                    let resp <- respRelayVec[portIdx].out.get;
                    if (isValid(resp.value) && !foundValidResp) begin
                        foundValidResp = True;
                        finalResp = resp;
                    end
                end
                return finalResp;
            endmethod


            // method Bool ready = isAllDownstreamPortRespReady;
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

module mkCsrLeaf#(tAddr myAddr) (CsrLeaf#(tAddr, tValue)) provisos (
    Bits#(tAddr, szAddr),
    Bits#(tValue, szData),
    Eq#(tAddr)
);

    Reg#(tValue) storageReg <- mkReg(unpack(0));
    RWire#(tValue) userWriteReqWire <- mkRWire;
    RWire#(tValue) busWriteReqWire <- mkRWire;

    RWire#(Bool) busReqIsReadWire <- mkRWire;



    rule handleWrite;
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
                Bool isAddrHit = req.addr == myAddr;

                if (isAddrHit) begin
                    if (req.isWrite) begin
                        busWriteReqWire.wset(req.value);
                    end
                    else begin
                        busReqIsReadWire.wset(True);
                    end
                end
            endmethod
            // method Bool ready = True;
        endinterface

        interface Get response;

            method ActionValue#(CsrReadWriteResp#(tValue)) get;
                if (isValid(busReqIsReadWire.wget)) begin
                    return CsrReadWriteResp{value: tagged Valid storageReg};
                end
                else begin
                    return CsrReadWriteResp{value: tagged Invalid};
                end
            endmethod
            // method Bool ready = True;
        endinterface
    endinterface

endmodule

module mkTb(Empty);
    CsrSwitch#(Bit#(32), Bit#(16), 3) dut <- mkCombinationalSwitch;
    CsrLeaf#(Bit#(32), Bit#(16)) csr1 <- mkCsrLeaf(1);
    CsrLeaf#(Bit#(32), Bit#(16)) csr2 <- mkCsrLeaf(2);
    CsrLeaf#(Bit#(32), Bit#(16)) csr3 <- mkCsrLeaf(3);

    mkConnection(csr1.busInputSrv, dut.busOutputCltVecIfc[0]);
    mkConnection(csr2.busInputSrv, dut.busOutputCltVecIfc[1]);
    mkConnection(csr3.busInputSrv, dut.busOutputCltVecIfc[2]);

    Reg#(Bit#(32)) addr <- mkReg(0);

    rule setVal;
        dut.busInputSrv.request.put(CsrReadWriteReq{ addr: addr, value: 1122, isWrite: False });
        
        if (addr == 3) begin
            addr <= 1;
        end
        else begin
            addr <= addr + 1;
        end
        csr2 <= csr2 + 1;
    endrule

    rule getVal;
        let ret <- dut.busInputSrv.response.get;
        $display("read_ret=%d  %d", isValid(ret.value), fromMaybe(0, ret.value));
        // $display("[1]=%d, [2]=%d, [3]=%d", csr1, csr2, csr3);
    endrule
 endmodule
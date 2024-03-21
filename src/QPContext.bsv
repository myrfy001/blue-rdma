import ClientServer :: *;
import BRAM :: *;
import FIFOF :: *;

import DataTypes :: *;
import RdmaUtils :: *;
import Headers :: *;

import Vector :: *;

import Settings :: *;
import MetaData :: *;
import PrimUtils :: *;



interface QPContext;
    interface Server#(ReadReqCommonQPC, Maybe#(EntryCommonQPC)) readCommonSrv;
    interface Server#(WriteReqCommonQPC, Bool) writeCommonSrv;
endinterface

(* synthesize *)
module mkQPContext(QPContext);
    BypassServer#(ReadReqCommonQPC, Maybe#(EntryCommonQPC)) readCommonSrvInst <- mkBypassServer("readCommonSrvInst");
    BypassServer#(WriteReqCommonQPC, Bool) writeCommonSrvInst <- mkBypassServer("writeCommonSrvInst");

    BRAM_Configure cfg = defaultValue;
    // Both read address and read output are registered
    cfg.latency = 2;
    // Allow full pipeline behavior
    cfg.outFIFODepth = 4;
    BRAM2Port#(IndexQP, Maybe#(EntryCommonQPC)) qpcEntryCommonStorage <- mkBRAM2Server(cfg);

    FIFOF#(KeyQP) keyPipeQ <- mkFIFOF;

    rule handleReadReq;
        let req <- readCommonSrvInst.getReq;
        IndexQP idx = getIndexQP(req.qpn);
        KeyQP key   = getKeyQP(req.qpn);
        let bramReq = BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: idx,
            datain: tagged Invalid
        };
        qpcEntryCommonStorage.portA.request.put(bramReq);
        keyPipeQ.enq(key);
        $display("read BRAM idx=", fshow(idx), "req=", fshow(req));
    endrule

    rule handleReadResp;
        let respMaybe <- qpcEntryCommonStorage.portA.response.get;
        let key = keyPipeQ.first;
        keyPipeQ.deq;

        $display("respMaybe=", fshow(respMaybe));

        if (respMaybe matches tagged Valid .resp &&& resp.qpnKeyPart == key) begin
            readCommonSrvInst.putResp(tagged Valid resp);
        end 
        else begin
            readCommonSrvInst.putResp(tagged Invalid);
        end
    endrule

    rule handleWriteReq;
        let req <- writeCommonSrvInst.getReq;
        IndexQP idx = getIndexQP(req.qpn);

        let bramReq = BRAMRequest{
            write: True,
            responseOnWrite: True,
            address: idx,
            datain: req.ent
        };
        qpcEntryCommonStorage.portB.request.put(bramReq);
        $display("write BRAM idx=", fshow(idx), "req=", fshow(req.ent));
    endrule

    rule handleWriteResp;
        let resp <- qpcEntryCommonStorage.portB.response.get;
        writeCommonSrvInst.putResp(True);
    endrule

    interface readCommonSrv = readCommonSrvInst.srv;
    interface writeCommonSrv = writeCommonSrvInst.srv;
endmodule

interface ExpectedPsnManager;
    method Action updatePsn(IndexQP qpn, PSN psn);
    method PSN getPsn(IndexQP qpn);
    method Action resetPSN(IndexQP qpn);
endinterface

module mkExpectedPsnManager(ExpectedPsnManager);
    // Expected PSN for different Queues
    Vector#(MAX_QP, Reg#(PSN)) expectedPSNRegVec <- replicateM(mkReg(0));
    
    RWire#(Tuple2#(IndexQP, PSN)) updatePsnReqWire <- mkRWire;
    RWire#(IndexQP)                resetPsnReqWire <- mkRWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonicalize;
        if (resetPsnReqWire.wget matches tagged Valid .qpnIdx) begin
            expectedPSNRegVec[qpnIdx] <= 0;
        end
        else if (updatePsnReqWire.wget matches tagged Valid .req) begin
            let {qpnIdx, psn} = req;
            expectedPSNRegVec[qpnIdx] <= psn;
        end
    endrule


    method Action updatePsn(IndexQP qpnIdx, PSN psn);
        updatePsnReqWire.wset(tuple2(qpnIdx, psn));
    endmethod

    method PSN getPsn(IndexQP qpnIdx) = expectedPSNRegVec[qpnIdx];

    method Action resetPSN(IndexQP qpnIdx);
        resetPsnReqWire.wset(qpnIdx);
    endmethod
endmodule
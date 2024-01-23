import ClientServer :: *;
import BRAM :: *;
import FIFOF :: *;

import DataTypes :: *;
import RdmaUtils :: *;

import Vector :: *;

import Settings :: *;
import MetaData :: *;
import PrimUtils :: *;


interface QPContext;
    interface Server#(QPCReadReqCommon, Maybe#(QPCEntryCommon)) readCommonSrv;
    interface Server#(QPCWriteReqCommon, Bool) writeCommonSrv;
endinterface

(* synthesize *)
module mkQPContext(QPContext);
    BypassServer#(QPCReadReqCommon, Maybe#(QPCEntryCommon)) readCommonSrvInst <- mkBypassServer;
    BypassServer#(QPCWriteReqCommon, Bool) writeCommonSrvInst <- mkBypassServer;

    BRAM_Configure cfg = defaultValue;
    // Both read address and read output are registered
    cfg.latency = 1;
    // Allow full pipeline behavior
    cfg.outFIFODepth = 4;
    BRAM2Port#(IndexQP, Maybe#(QPCEntryCommon)) qpcEntryCommonStorage <- mkBRAM2Server(cfg);

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


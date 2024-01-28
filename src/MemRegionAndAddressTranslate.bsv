import BRAM :: *;
import ClientServer :: *;
import Cntrs :: *;
import Connectable :: *;
import FIFOF :: *;
import PAClib :: *;
import Vector :: *;
import Cntrs :: * ;

import DataTypes :: *;
import Headers :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;
import UserLogicTypes :: *;
import UserLogicUtils :: *;
import MetaData :: *;



typedef Server#(addrType, dataType)                    BramRead#(type addrType, type dataType);
typedef Server#(Tuple2#(addrType, dataType), Bool)     BramWrite#(type addrType, type dataType);

// interface BramCache#(type addrType, type dataType, numeric type splitCntExp);
interface BramCache#(type addrType, type dataType);
    interface BramRead #(addrType, dataType)   read;
    interface BramWrite#(addrType, dataType)   write;
endinterface


module mkBramCache(BramCache#(addrType, dataType)) provisos(
    Bits#(addrType, addrTypeSize),
    Bits#(dataType, dataTypeSize)
    // Bits#(subAddrType, subAddrTypeSize),
    // Add#(subAddrTypeSize, splitCntExp, addrTypeSize)
);
    BRAM_Configure cfg = defaultValue;
    // Both read address and read output are registered
    cfg.latency = 2;
    // Allow full pipeline behavior
    cfg.outFIFODepth = 4;

    // Vector#(TExp#(splitCntExp), BRAM2Port#(subAddrType, dataType)) subBramVec <- replicateM(mkBRAM2Server(cfg));
    // Vector#(TExp#(splitCntExp), FIFOF#(BRAMRequest)) subReqFifoVec <- replicateM(mkFIFOF);

    // FIFOF#(subAddrType) orderKeepQ <- mkFIFOF;

    // for (Integer idx = 0; idx < valueOf(TExp#(splitCntExp)); idx = idx + 1) begin
    //     rule handleSendBramReq;
    //         let req = subReqFifoVec[idx].first;
    //         subReqFifoVec[idx].deq;
    //         bram2Port.portA.request.put(req);
    //         orderKeepQ.enq(fromInteger(idx));
    //     endrule
    // end
    BRAM2Port#(addrType, dataType) bram2Port <- mkBRAM2Server(cfg);

    FIFOF#(addrType)   bramReadReqQ <- mkFIFOF;
    FIFOF#(dataType)  bramReadRespQ <- mkFIFOF;

    FIFOF#(Tuple2#(addrType, dataType))  bramWriteReqQ  <- mkFIFOF;
    FIFOF#(Bool)                         bramWriteRespQ <- mkFIFOF;

    rule handleBramReadReq;
        let cacheAddr = bramReadReqQ.first;
        bramReadReqQ.deq;

        let req = BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: cacheAddr,
            datain: dontCareValue
        };
        // subAddrType subIdx = truncateLSB();
        bram2Port.portA.request.put(req);
    endrule

    rule handleBramReadResp;
        let readRespData <- bram2Port.portA.response.get;
        bramReadRespQ.enq(readRespData);
    endrule


    rule handleBramWriteReq;
        let {cacheAddr, writeData} = bramWriteReqQ.first;
        bramWriteReqQ.deq;
        
        let req = BRAMRequest{
            write: True,
            responseOnWrite: True,
            address: cacheAddr,
            datain: writeData
        };
        // subAddrType subIdx = truncateLSB();
        bram2Port.portB.request.put(req);
    endrule

    rule handleBramWriteResp;
        let _ <- bram2Port.portB.response.get;
        bramWriteRespQ.enq(True);
    endrule


    interface read =  toGPServer(bramReadReqQ,  bramReadRespQ);
    interface write = toGPServer(bramWriteReqQ, bramWriteRespQ);
endmodule


interface MemRegionTable;
    interface Server#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) querySrv;
    interface Server#(MrTableModifyReq, MrTableModifyResp) modifySrv;
endinterface

(* synthesize *)
module mkMemRegionTable(MemRegionTable);
    BramCache#(IndexMR, Maybe#(MemRegionTableEntry)) mrTableStorage <- mkBramCache;
    BypassServer#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) querySrvInst <- mkBypassServer;
    BypassServer#(MrTableModifyReq, MrTableModifyResp) modifySrvInst <- mkBypassServer;

    rule handleQueryReq;
        let req <- querySrvInst.getReq;
        mrTableStorage.read.request.put(req.idx);
    endrule

    rule handleQueryResp;
        let resp <- mrTableStorage.read.response.get;
        querySrvInst.putResp(resp);
    endrule

    rule handleModifyReq;
        let req <- modifySrvInst.getReq;
        mrTableStorage.write.request.put(tuple2(req.idx, req.entry));
    endrule

    rule handleModifyResp;
        let resp <- mrTableStorage.write.response.get;
        modifySrvInst.putResp(MrTableModifyResp{success: resp});
    endrule

    interface querySrv = querySrvInst.srv;
    interface modifySrv = modifySrvInst.srv;
endmodule


interface TLB;
    interface Server#(PgtAddrTranslateReq, ADDR) translateSrv;
    interface Server#(PgtModifyReq, PgtModifyResp) modifySrv;
endinterface

function PageOffset getPageOffset(ADDR addr);
    return truncate(addr);
endfunction

function ADDR restorePA(PageNumber pn, PageOffset po);
    return signExtend({ pn, po });
endfunction

function PageNumber getPageNumber(ADDR pa);
    return truncate(pa >> valueOf(PAGE_OFFSET_WIDTH));
endfunction

(* synthesize *)
module mkTLB(TLB);
    
    BramCache#(PTEIndex, PageTableEntry) pageTableStorage <- mkBramCache;

    BypassServer#(PgtAddrTranslateReq, ADDR) translateSrvInst <- mkBypassServer;
    BypassServer#(PgtModifyReq, PgtModifyResp) modifySrvInst <- mkBypassServer;

    FIFOF#(Bit#(PAGE_OFFSET_WIDTH)) offsetInputQ <- mkFIFOF;

    rule handleTranslateReq;
        let req <- translateSrvInst.getReq;
        let mr = req.mrEntry;
        let va = req.addrToTrans;

        let pageNumberOffset = getPageNumber(va) - getPageNumber(mr.baseVA);
        PTEIndex pteIdx = mr.pgtOffset + truncate(pageNumberOffset);
        pageTableStorage.read.request.put(pteIdx);

        offsetInputQ.enq(getPageOffset(va));
    endrule

    rule handleTranslateResp;
        let pageOffset = offsetInputQ.first;
        offsetInputQ.deq;

        PageTableEntry pte <- pageTableStorage.read.response.get;

        let pa = restorePA(pte.pn, pageOffset);
        translateSrvInst.putResp(pa);
        
    endrule

    rule handleModifyReq;
        let req <- modifySrvInst.getReq;
        pageTableStorage.write.request.put(tuple2(req.idx, req.pte));
    endrule

    rule handleModifyResp;
        let resp <- pageTableStorage.write.response.get;
        modifySrvInst.putResp(PgtModifyResp{success: resp});
    endrule


    interface translateSrv = translateSrvInst.srv;
    interface modifySrv = modifySrvInst.srv;
endmodule



interface MrAndPgtManager;
    interface Server#(RingbufRawDescriptor, Bool) mrAndPgtModifyDescSrv;
    interface UserLogicDmaReadClt pgtDmaReadClt;
    interface Client#(MrTableModifyReq, MrTableModifyResp) mrModifyClt;
    interface Client#(PgtModifyReq, PgtModifyResp) pgtModifyClt;
endinterface


typedef enum {
    MrAndPgtManagerFsmStateIdle,
    MrAndPgtManagerFsmStateWaitMRModifyResponse,
    MrAndPgtManagerFsmStateHandlePGTUpdate,
    MrAndPgtManagerFsmStateWaitPGTUpdateLastResp
} MrAndPgtManagerFsmState deriving(Bits, Eq);


(* synthesize *)
module mkMrAndPgtManager(MrAndPgtManager);
    FIFOF#(RingbufRawDescriptor) reqQ <- mkFIFOF;
    FIFOF#(Bool) respQ <- mkFIFOF;

    FIFOF#(UserLogicDmaH2cReq) dmaReadReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cResp) dmaReadRespQ <- mkFIFOF;

    BypassClient#(MrTableModifyReq, MrTableModifyResp) mrModifyCltInst <- mkBypassClient;
    BypassClient#(PgtModifyReq, PgtModifyResp) pgtModifyCltInst <- mkBypassClient;
    

    Reg#(MrAndPgtManagerFsmState) state <- mkReg(MrAndPgtManagerFsmStateIdle);

    Reg#(DataStream) curBeatOfDataReg <- mkReg(unpack(0));
    Reg#(PTEIndex) curSecondStagePgtWriteIdxReg <- mkRegU;
    
    Integer bytesPerPgtSecondStageEntryRequest = valueOf(PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED) / valueOf(BYTE_WIDTH);

    // we set max inflight pgt update request is 2^3 = 8;
    Count#(Bit#(3)) pgtUpdateRespCounter <- mkCount(0);

    rule updatePgtStateIdle if (state == MrAndPgtManagerFsmStateIdle);
        let descRaw = reqQ.first;
        reqQ.deq;
        // $display("PGT get modify request", fshow(descRaw));
        let opcode = getOpcodeFromRingbufDescriptor(descRaw);

        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateMrTable: begin
                state <= MrAndPgtManagerFsmStateWaitMRModifyResponse;
                CmdQueueReqDescUpdateMrTable desc = unpack(descRaw);
                let modifyReq = MrTableModifyReq {
                    idx: key2IndexMR(desc.mrKey),
                    entry: isZeroR(desc.mrLength) ?
                            tagged Invalid : 
                            tagged Valid MemRegionTableEntry {
                                pgtOffset: desc.pgtOffset,
                                baseVA: desc.mrBaseVA,
                                len: desc.mrLength,
                                accFlags: unpack(desc.accFlags),
                                pdHandler: desc.pdHandler,
                                keyPart: lkey2KeyPartMR(desc.mrKey)
                            }
                };
                mrModifyCltInst.putReq(modifyReq);
                // $display("addr translate modify first stage finished.");
            end
            CmdQueueOpcodeUpdatePGT: begin
                CmdQueueReqDescUpdatePGT desc = unpack(descRaw);
                dmaReadReqQ.enq(UserLogicDmaH2cReq{
                    addr: desc.dmaAddr,
                    len: truncate(desc.dmaReadLength)
                });
                curSecondStagePgtWriteIdxReg <= truncate(desc.startIndex);
                state <= MrAndPgtManagerFsmStateHandlePGTUpdate;
                // $display("addr translate modify second stage start.");
            end
        endcase
    endrule

    rule handleMrModifyResp if (state == MrAndPgtManagerFsmStateWaitMRModifyResponse);
        let _ <- mrModifyCltInst.getResp;
        respQ.enq(True);
        state <= MrAndPgtManagerFsmStateIdle;
    endrule


    rule updatePgtStateHandlePGTUpdate if (state == MrAndPgtManagerFsmStateHandlePGTUpdate);
        // since this is the control path, it's not fully pipelined to make it simple.
        if (curBeatOfDataReg.byteEn[0] == 0) begin
            if (curBeatOfDataReg.isLast) begin
                state <= MrAndPgtManagerFsmStateWaitPGTUpdateLastResp;
                curBeatOfDataReg <= unpack(0);
                // $display("addr translate modify second stage finished.");
            end 
            else begin
                curBeatOfDataReg <= dmaReadRespQ.first.dataStream;
                dmaReadRespQ.deq;
            end
        end 
        else begin 
            let modifyReq = PgtModifyReq{
                idx: curSecondStagePgtWriteIdxReg,
                pte: PageTableEntry {
                    pn: truncate(curBeatOfDataReg.data >> valueOf(PAGE_OFFSET_WIDTH))
                }
            };
            pgtModifyCltInst.putReq(modifyReq);
            pgtUpdateRespCounter.incr(1);
            // $display("addr translate modify second stage:", fshow(modifyReq));
            curSecondStagePgtWriteIdxReg <= curSecondStagePgtWriteIdxReg + 1;
            let t = curBeatOfDataReg;
            t.byteEn = t.byteEn >> bytesPerPgtSecondStageEntryRequest;
            t.data = t.data >> (bytesPerPgtSecondStageEntryRequest * valueOf(BYTE_WIDTH));
            curBeatOfDataReg <= t;
        end
    endrule

    rule handlePgtModifyResp;
        let _ <- pgtModifyCltInst.getResp;
        pgtUpdateRespCounter.decr(1);
    endrule

    rule handlePgtModifyLastResp if (state == MrAndPgtManagerFsmStateWaitPGTUpdateLastResp);
        if (pgtUpdateRespCounter == 0) begin
            respQ.enq(True);
            state <= MrAndPgtManagerFsmStateIdle;
        end
    endrule

    interface mrAndPgtModifyDescSrv = toGPServer(reqQ, respQ);
    interface pgtDmaReadClt = toGPClient(dmaReadReqQ, dmaReadRespQ);
    interface mrModifyClt = mrModifyCltInst.clt;
    interface pgtModifyClt = pgtModifyCltInst.clt;
endmodule


// interface DmaReqAddrTranslator;
//     interface GetPut#(DmaReadReqNew) readReqTranslator;
//     interface GetPut#(DmaWriteReqNew) writeReqTranslator;
//     interface Client#(FindReqTLB, FindRespTLB) tlbClt;
// endinterface


// typedef union tagged {
//     DmaReadReqNew AddressTranslatePendingRead;
//     DmaWriteReqNew AddressTranslatePendingWrite;
// } AddressTranslatePendingReqEntry deriving(Bits, FShow);


// (* synthesize *)
// module mkDmaReadReqAddrTranslator(DmaReqAddrTranslator);
//     FIFOF#(DmaReadReqNew) readInQ <- mkFIFOF;
//     FIFOF#(DmaReadReqNew) readOutQ <- mkFIFOF;
//     FIFOF#(DmaWriteReqNew) writeInQ <- mkFIFOF;
//     FIFOF#(DmaWriteReqNew) writeOutQ <- mkFIFOF;

//     FIFOF#(AddressTranslatePendingReqEntry) pendingReqQ <- mkSizedFIFOF(3);

//     Reg#(Bool) isNextRead <- mkReg(False);
    
//     interface readReqTranslator = tuple2(toGet(readOutQ), toPut(readInQ));
//     interface writeReqTranslator = tuple2(toGet(writeOutQ), toPut(writeInQ));
//     interface Client tlbClt;
//         interface Get request;
//             method ActionValue#(FindReqTLB) get() if (readInQ.notEmpty || writeInQ.notEmpty);
//                 Bool grantRead = False;
//                 Bool grantWrite = False;

//                 if (isNextRead) begin
//                     if (readInQ.notEmpty) begin
//                         grantRead = True;
//                     end 
//                     else if (writeInQ.notEmpty) begin
//                         grantWrite = True;
//                     end
//                 end 
//                 else begin
//                     if (writeInQ.notEmpty) begin
//                         grantWrite = True;   
//                     end 
//                     else if (readInQ.notEmpty) begin
//                         grantRead = True;
//                     end
//                 end

//                 if (grantRead) begin
//                     readInQ.deq;
//                     isNextRead <= False;
//                     pendingReqQ.enq(tagged AddressTranslatePendingRead readInQ.first);  // TODO: need not to store address and mrID anymore
//                     return tuple2(zeroExtend(pack(readInQ.first.mrID)), readInQ.first.startAddr);
//                 end 
//                 else begin
//                     writeInQ.deq;
//                     isNextRead <= True;
//                     pendingReqQ.enq(tagged AddressTranslatePendingWrite writeInQ.first);  // TODO: need not to store address and mrID anymore
//                     return tuple2(zeroExtend(pack(writeInQ.first.metaData.mrID)), writeInQ.first.metaData.startAddr);
//                 end

//             endmethod
//         endinterface
        
//         interface Put response;
//             method Action put(FindRespTLB ret);
//                 pendingReqQ.deq;
//                 if (pendingReqQ.first matches tagged AddressTranslatePendingRead .resp) begin
//                     let t = resp;
//                     t.startAddr = tpl_2(ret);
//                     readOutQ.enq(t);
//                     // $display("DMA H2C Translate, %x -> %x", resp.startAddr, tpl_2(ret));
//                 end 
//                 else if (pendingReqQ.first matches tagged AddressTranslatePendingWrite .resp) begin
//                     let t = resp;
//                     t.metaData.startAddr = tpl_2(ret);
//                     writeOutQ.enq(t);
//                     // $display("DMA C2CH Translate, %x -> %x", resp.metaData.startAddr, tpl_2(ret));
//                 end
//             endmethod
//         endinterface
//     endinterface

// endmodule
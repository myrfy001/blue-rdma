import BRAM :: *;
import ClientServer :: *;
import Cntrs :: *;
import Connectable :: *;
import FIFOF :: *;
import PAClib :: *;
import Vector :: *;

import DataTypes :: *;
import Headers :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;


typedef Server#(addrType, dataType) BramRead#(type addrType, type dataType);

interface BramCache#(type addrType, type dataType);
    interface BramRead#(addrType, dataType) read;
    method Action write(addrType cacheAddr, dataType writeData);
endinterface


module mkBramCache(BramCache#(addrType, dataType)) provisos(Bits#(addrType, addrTypeSize), Bits#(dataType, dataTypeSize));
    BRAM_Configure cfg = defaultValue;
    // Both read address and read output are registered
    cfg.latency = 2;
    // Allow full pipeline behavior
    cfg.outFIFODepth = 4;
    BRAM2Port#(addrType, dataType) bram2Port <- mkBRAM2Server(cfg);

    FIFOF#(addrType)  bramReadReqQ <- mkFIFOF;
    FIFOF#(dataType) bramReadRespQ <- mkFIFOF;

    rule handleBramReadReq;
        let cacheAddr = bramReadReqQ.first;
        bramReadReqQ.deq;

        let req = BRAMRequest{
            write: False,
            responseOnWrite: False,
            address: cacheAddr,
            datain: dontCareValue
        };
        bram2Port.portA.request.put(req);
    endrule

    rule handleBramReadResp;
        let readRespData <- bram2Port.portA.response.get;
        bramReadRespQ.enq(readRespData);
    endrule

    method Action write(addrType cacheAddr, dataType writeData);
        let req = BRAMRequest{
            write: True,
            responseOnWrite: False,
            address: cacheAddr,
            datain: writeData
        };
        bram2Port.portB.request.put(req);
    endmethod

    interface read = toGPServer(bramReadReqQ, bramReadRespQ);
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
        mrTableStorage.write(req.idx, req.entry);
        modifySrvInst.putResp(MrTableModifyResp{success: True});
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
        pageTableStorage.write(req.idx, req.pte);
        modifySrvInst.putResp(PgtModifyResp{success: True});
        $display(fshow(req));
    endrule


    interface translateSrv = translateSrvInst.srv;
    interface modifySrv = modifySrvInst.srv;
endmodule



// interface PgtManager;
//     interface Server#(RingbufRawDescriptor, Bool) pgtModifySrv;
//     interface UserLogicDmaReadClt pgtDmaReadClt;
// endinterface


// typedef enum {
//     PgtManagerFsmStateIdle,
//     PgtManagerFsmStateHandleFirstStageUpdate,
//     PgtManagerFsmStateHandleSecondStageUpdate
// } PgtManagerFsmState deriving(Bits, Eq);


// module mkPgtManager#(TLB tlb)(PgtManager);
//     FIFOF#(RingbufRawDescriptor) reqQ <- mkFIFOF;
//     FIFOF#(Bool) respQ <- mkFIFOF;

//     FIFOF#(UserLogicDmaH2cReq) dmaReadReqQ <- mkFIFOF;
//     FIFOF#(UserLogicDmaH2cResp) dmaReadRespQ <- mkFIFOF;



//     Reg#(PgtManagerFsmState) state <- mkReg(PgtManagerFsmStateIdle);

//     Reg#(DataStream) curBeatOfDataReg <- mkReg(unpack(0));
//     Reg#(PgtSecondStageIndex) curSecondStagePgtWriteIdxReg <- mkRegU;
    
//     Integer bytesPerPgtSecondStageEntryRequest = valueOf(PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED) / valueOf(BYTE_WIDTH);


//     rule updatePgtStateIdle if (state == PgtManagerFsmStateIdle);
//         let descRaw = reqQ.first;
//         reqQ.deq;
//         // $display("PGT get modify request", fshow(descRaw));
//         let opcode = getOpcodeFromRingbufDescriptor(descRaw);

//         case (unpack(truncate(opcode)))
//             CmdQueueOpcodeUpdateFirstStagePGT: begin
//                 state <= PgtManagerFsmStateIdle;
//                 CmdQueueReqDescUpdateFirstStagePGT desc = unpack(descRaw);
//                 let modifyReq = PgtModifyFirstStageReq {
//                     asid: truncate(desc.index),
//                     content: PgtFirstStagePayload{
//                         secondStageOffset: truncate(desc.pointedToSecondStageIndex),
//                         secondStageEntryCnt: truncate(desc.pointedToSecondStageCount),
//                         baseVA: desc.baseVA
//                     }
//                 };
//                 tlb.modify(tagged Req4FirstStage modifyReq);
//                 respQ.enq(True);
//                 // $display("addr translate modify first stage finished.");
//             end
//             CmdQueueOpcodeUpdateSecondStagePGT: begin
//                 CmdQueueReqDescUpdateSecondStagePGT desc = unpack(descRaw);
//                 dmaReadReqQ.enq(UserLogicDmaH2cReq{
//                     addr: desc.dmaAddr,
//                     len: truncate(desc.dmaReadLength)
//                 });
//                 curSecondStagePgtWriteIdxReg <= truncate(desc.startIndex);
//                 state <= PgtManagerFsmStateHandleSecondStageUpdate;
//                 // $display("addr translate modify second stage start.");
//             end
//         endcase
//     endrule


//     rule updatePgtStateHandleSecondStageUpdate if (state == PgtManagerFsmStateHandleSecondStageUpdate);
//         // since this is the control path, it's not fully pipelined to make it simple.
//         if (curBeatOfDataReg.byteEn[0] == 0) begin
//             if (curBeatOfDataReg.isLast) begin
//                 state <= PgtManagerFsmStateIdle;
//                 curBeatOfDataReg <= unpack(0);
//                 respQ.enq(True);
//                 // $display("addr translate modify second stage finished.");
//             end 
//             else begin
//                 curBeatOfDataReg <= dmaReadRespQ.first.dataStream;
//                 dmaReadRespQ.deq;
//             end
//         end 
//         else begin 
//             let modifyReq = PgtModifySecondStageReq{
//                 index: curSecondStagePgtWriteIdxReg,
//                 content: PgtSecondStagePayload {
//                     paPart: truncate(curBeatOfDataReg.data >> valueOf(PAGE_OFFSET_WIDTH))
//                 }
//             };
//             tlb.modify(tagged Req4SecondStage modifyReq);
//             // $display("addr translate modify second stage:", fshow(modifyReq));
//             curSecondStagePgtWriteIdxReg <= curSecondStagePgtWriteIdxReg + 1;
//             let t = curBeatOfDataReg;
//             t.byteEn = t.byteEn >> bytesPerPgtSecondStageEntryRequest;
//             t.data = t.data >> (bytesPerPgtSecondStageEntryRequest * valueOf(BYTE_WIDTH));
//             curBeatOfDataReg <= t;
//         end
//     endrule

//     interface pgtModifySrv = toGPServer(reqQ, respQ);
//     interface pgtDmaReadClt = toGPClient(dmaReadReqQ, dmaReadRespQ);
// endmodule


// interface DmaReqAddrTranslator;
//     interface GetPut#(DmaReadReq) readReqTranslator;
//     interface GetPut#(DmaWriteReq) writeReqTranslator;
//     interface Client#(FindReqTLB, FindRespTLB) tlbClt;
// endinterface


// typedef union tagged {
//     DmaReadReq AddressTranslatePendingRead;
//     DmaWriteReq AddressTranslatePendingWrite;
// } AddressTranslatePendingReqEntry deriving(Bits, FShow);


// (* synthesize *)
// module mkDmaReadReqAddrTranslator(DmaReqAddrTranslator);
//     FIFOF#(DmaReadReq) readInQ <- mkFIFOF;
//     FIFOF#(DmaReadReq) readOutQ <- mkFIFOF;
//     FIFOF#(DmaWriteReq) writeInQ <- mkFIFOF;
//     FIFOF#(DmaWriteReq) writeOutQ <- mkFIFOF;

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
import Arbitration :: *;
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
import Utils :: *;
import UserLogicSettings :: *;
import UserLogicTypes :: *;
import UserLogicUtils :: *;


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


typedef Tuple2#(ASID, ADDR) FindReqTLB;
typedef Tuple2#(Bool, ADDR) FindRespTLB;
typedef Server#(FindReqTLB, FindRespTLB) FindInTLB;

interface TLB;
    interface FindInTLB find;
    method Action modify(PgtModifyReq req);
endinterface

function Bit#(PAGE_OFFSET_WIDTH) getPageOffset(ADDR addr);
    return truncate(addr);
endfunction

function ADDR restorePA(
    Bit#(TLB_CACHE_PA_DATA_WIDTH) paData, Bit#(PAGE_OFFSET_WIDTH) pageOffset
);
    return signExtend({ paData, pageOffset });
endfunction

function Bit#(TLB_CACHE_PA_DATA_WIDTH) getData4PA(ADDR pa);
    return truncate(pa >> valueOf(PAGE_OFFSET_WIDTH));
endfunction

function ADDR getPageAlignedAddr(ADDR addr);
    Bit#(TLog#(PAGE_SIZE_CAP)) t = 0;
    addr[valueOf(TLog#(PAGE_SIZE_CAP))-1:0] = t;
    return unpack(addr);
endfunction


module mkTLB(TLB);
    BramCache#(PgtFirstStageIndex, PgtFirstStagePayload) firstStageCache <- mkBramCache;
    BramCache#(PgtSecondStageIndex, PgtSecondStagePayload) secondStageCache <- mkBramCache;


    FIFOF#(ADDR) vaInputQ <- mkFIFOF;
    FIFOF#(Maybe#(Bit#(PAGE_OFFSET_WIDTH))) offsetInputQ <- mkFIFOF;
    FIFOF#(FindReqTLB) findReqQ <- mkFIFOF;
    FIFOF#(FindRespTLB) findRespQ <- mkFIFOF;

    rule handleFindReq;
        let req = findReqQ.first;
        findReqQ.deq;
        firstStageCache.read.request.put(tpl_1(req));
        vaInputQ.enq(tpl_2(req));
    endrule

    rule handleSecondStageQuery;
        let va = vaInputQ.first;
        vaInputQ.deq;

        PgtFirstStagePayload firstStageResp <- firstStageCache.read.response.get;

        let vaOffset = va - firstStageResp.baseVA;
        let secondStageIndexOffset = truncate(vaOffset >> valueOf(PAGE_OFFSET_WIDTH));

        PgtSecondStageIndex secondStageIndex = firstStageResp.secondStageOffset + secondStageIndexOffset;
        let addrToRead = secondStageIndex;
        secondStageCache.read.request.put(addrToRead);

        let pageOffset = getPageOffset(va);

        let pteValid = firstStageResp.secondStageEntryCnt != 0;

        offsetInputQ.enq(pteValid ? tagged Valid pageOffset : tagged Invalid);
    endrule

    rule handleFindResp;
        let pageOffset = offsetInputQ.first;
        offsetInputQ.deq;

        PgtSecondStagePayload secondStageResp <- secondStageCache.read.response.get;

        if (pageOffset matches tagged Valid .offset) begin
            let pa = restorePA(secondStageResp.paPart, offset);
            findRespQ.enq(tuple2(True, pa));
        end else begin
            findRespQ.enq(tuple2(False, ?));
        end
        
        
    endrule

    method Action modify(PgtModifyReq req);
        case (req) matches
            tagged Req4FirstStage .r: begin
                firstStageCache.write(
                    r.asid,
                    r.content
                );
            end
            tagged Req4SecondStage .r: begin
                secondStageCache.write(
                    r.index,
                    r.content
                );
            end
        endcase
    endmethod

    interface find = toGPServer(findReqQ, findRespQ);
endmodule



interface PgtManager;
    interface Server#(RingbufRawDescriptor, Bool) pgtModifySrv;
    interface UserLogicDmaReadClt pgtDmaReadClt;
endinterface


typedef enum {
    PgtManagerFsmStateIdle,
    PgtManagerFsmStateHandleFirstStageUpdate,
    PgtManagerFsmStateHandleSecondStageUpdate
} PgtManagerFsmState deriving(Bits, Eq);


module mkPgtManager#(TLB tlb)(PgtManager);
    FIFOF#(RingbufRawDescriptor) reqQ <- mkFIFOF;
    FIFOF#(Bool) respQ <- mkFIFOF;

    FIFOF#(UserLogicDmaH2cReq) dmaReadReqQ <- mkFIFOF;
    FIFOF#(UserLogicDmaH2cResp) dmaReadRespQ <- mkFIFOF;



    Reg#(PgtManagerFsmState) state <- mkReg(PgtManagerFsmStateIdle);

    Reg#(DataStream) curBeatOfDataReg <- mkReg(unpack(0));
    Reg#(PgtSecondStageIndex) curSecondStagePgtWriteIdxReg <- mkRegU;
    
    Integer bytesPerPgtSecondStageEntryRequest = valueOf(PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED) / valueOf(BYTE_WIDTH);


    rule updatePgtStateIdle if (state == PgtManagerFsmStateIdle);
        let descRaw = reqQ.first;
        reqQ.deq;
        // $display("PGT get modify request", fshow(descRaw));
        let opcode = getOpcodeFromRingbufDescriptor(descRaw);

        case (unpack(truncate(opcode)))
            CmdQueueOpcodeUpdateFirstStagePGT: begin
                state <= PgtManagerFsmStateIdle;
                CmdQueueDescUpdateFirstStagePGT desc = unpack(descRaw);
                let modifyReq = PgtModifyFirstStageReq {
                    asid: truncate(desc.index),
                    content: PgtFirstStagePayload{
                        secondStageOffset: truncate(desc.pointedToSecondStageIndex),
                        secondStageEntryCnt: truncate(desc.pointedToSecondStageCount),
                        baseVA: desc.baseVA
                    }
                };
                tlb.modify(tagged Req4FirstStage modifyReq);
                respQ.enq(True);
                // $display("addr translate modify first stage finished.");
            end
            CmdQueueOpcodeUpdateSecondStagePGT: begin
                CmdQueueDescUpdateSecondStagePGT desc = unpack(descRaw);
                dmaReadReqQ.enq(UserLogicDmaH2cReq{
                    addr: desc.dmaAddr,
                    len: truncate(desc.dmaReadLength)
                });
                curSecondStagePgtWriteIdxReg <= truncate(desc.startIndex);
                state <= PgtManagerFsmStateHandleSecondStageUpdate;
                // $display("addr translate modify second stage start.");
            end
        endcase
    endrule


    rule updatePgtStateHandleSecondStageUpdate if (state == PgtManagerFsmStateHandleSecondStageUpdate);
        // since this is the control path, it's not fully pipelined to make it simple.
        if (curBeatOfDataReg.byteEn[0] == 0) begin
            if (curBeatOfDataReg.isLast) begin
                state <= PgtManagerFsmStateIdle;
                curBeatOfDataReg <= unpack(0);
                respQ.enq(True);
                // $display("addr translate modify second stage finished.");
            end else begin
                curBeatOfDataReg <= dmaReadRespQ.first.dataStream;
                dmaReadRespQ.deq;
            end
        end else begin 
            let modifyReq = PgtModifySecondStageReq{
                index: curSecondStagePgtWriteIdxReg,
                content: PgtSecondStagePayload {
                    paPart: truncate(curBeatOfDataReg.data)
                }
            };
            tlb.modify(tagged Req4SecondStage modifyReq);
            // $display("addr translate modify second stage:", fshow(modifyReq));
            curSecondStagePgtWriteIdxReg <= curSecondStagePgtWriteIdxReg + 1;
            let t = curBeatOfDataReg;
            t.byteEn = t.byteEn >> bytesPerPgtSecondStageEntryRequest;
            t.data = t.data >> (bytesPerPgtSecondStageEntryRequest * valueOf(BYTE_WIDTH));
            curBeatOfDataReg <= t;
        end
    endrule

    interface pgtModifySrv = toGPServer(reqQ, respQ);
    interface pgtDmaReadClt = toGPClient(dmaReadReqQ, dmaReadRespQ);
endmodule
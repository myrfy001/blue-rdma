import FIFOF :: *;

import PipeIn :: *;
import ClientServer :: *;
import RdmaUtils :: *;
import MetaData :: *;
import DataTypes :: *;
import Headers :: *;

import PrimUtils :: *;

typedef enum {
    RDMA_REQ_ST_NORMAL,
    RDMA_REQ_ST_INV_ACC_FLAG,
    RDMA_REQ_ST_INV_OPCODE,
    RDMA_REQ_ST_INV_MR_KEY,
    RDMA_REQ_ST_INV_MR_REGION,
    RDMA_REQ_ST_UNKNOWN
} RdmaReqStatus deriving(Bits, Eq, FShow);


interface RQ;
    interface PipeIn#(RdmaPktMetaDataAndQPCtx) pktMetaDataPipeIn;
    interface Client#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) mrTableQueryClt;
    interface Client#(PgtAddrTranslateReq, ADDR) pgtQueryClt;
    interface Client#(PayloadConReq, PayloadConResp) payloadXonsumerControlPortClt;
endinterface


(* synthesize *)
module mkRQ(RQ ifc);
    FIFOF#(RdmaPktMetaDataAndQPCtx) rdmaPktMetaDataInQ <- mkFIFOF;

    BypassClient#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) mrTableQueryCltInst  <- mkBypassClient;
    BypassClient#(PgtAddrTranslateReq, ADDR) pgtQueryCltInst                         <- mkBypassClient;
    BypassClient#(PayloadConReq, PayloadConResp) payloadConsumerControlClt           <- mkBypassClient;

    // Pipeline FIFOs
    FIFOF#(RdmaPktMetaDataAndQPCtx)                                      getMRQueryRespPipeQ <- mkFIFOF;
    FIFOF#(Tuple4#(RdmaPktMetaDataAndQPCtx, RdmaReqStatus, Bool, Bool)) getPGTQueryRespPipeQ <- mkFIFOF;
    FIFOF#(Tuple3#(RdmaPktMetaDataAndQPCtx, RdmaReqStatus, Bool))           waitDMARespPipeQ <- mkFIFOF;


    function FlagsType#(MemAccessTypeFlag) genAccessFlagFromReqType(Bool isSend, Bool isRead, Bool isWrite, Bool isAtomic);
        let accFlags = enum2Flag(IBV_ACCESS_LOCAL_WRITE);
        case ({pack(isSend), pack(isRead), pack(isWrite), pack(isAtomic)})
            4'b1000: begin  // Send
                accFlags = enum2Flag(IBV_ACCESS_LOCAL_WRITE);
            end
            4'b0100: begin  // Read
                accFlags = enum2Flag(IBV_ACCESS_REMOTE_READ);
            end
            4'b0010: begin  // Write
                accFlags = enum2Flag(IBV_ACCESS_REMOTE_WRITE);
            end
            4'b0001: begin  // Atomic
                accFlags = enum2Flag(IBV_ACCESS_REMOTE_ATOMIC);
            end
            default: begin end
        endcase
        return accFlags;
    endfunction

    rule queryMemoryRegionTable;
        let pktMetaDataAndQpc = rdmaPktMetaDataInQ.first;
        rdmaPktMetaDataInQ.deq;

        let bth  = extractBTH(pktMetaDataAndQpc.metadata.pktHeader.headerData);
        let reth = extractPriRETH(pktMetaDataAndQpc.metadata.pktHeader.headerData, bth.trans);

        let mrTableQueryReq = MrTableQueryReq{
            idx: rkey2IndexMR(reth.rkey)
        };
        mrTableQueryCltInst.putReq(mrTableQueryReq);

        getMRQueryRespPipeQ.enq(pktMetaDataAndQpc);
        
    endrule

    rule getMRQueryRespAndCheckAddrRangeAndCheckAccessFlag;
        let mrMaybe <- mrTableQueryCltInst.getResp;

        $display("mrMaybe=", fshow(mrMaybe));
        
        let pktMetaDataAndQpc = getMRQueryRespPipeQ.first;
        getMRQueryRespPipeQ.deq;

        let pktMetaData = pktMetaDataAndQpc.metadata;
        let rdmaHeader  = pktMetaData.pktHeader;
        let qpc = pktMetaDataAndQpc.qpc;
        

        let bth   = extractBTH(rdmaHeader.headerData);
        let reth  = extractPriRETH(rdmaHeader.headerData, bth.trans);

        let isSendReq            = isSendReqRdmaOpCode(bth.opcode);
        let isWriteReq           = isWriteReqRdmaOpCode(bth.opcode);
        let isWriteImmReq        = isWriteImmReqRdmaOpCode(bth.opcode);
        let isReadReq            = isReadReqRdmaOpCode(bth.opcode);
        let isAtomicReq          = isAtomicReqRdmaOpCode(bth.opcode);
        let isFirstOrOnlyPkt     = isFirstOrOnlyRdmaOpCode(bth.opcode);
        let isLastOrOnlyPkt      = isLastOrOnlyRdmaOpCode(bth.opcode);
        let isSupportedReqOpCode = isSupportedReqOpCodeRQ(qpc.qpType, bth.opcode); 

        let isRespNeedDMAWrite   = rdmaRespNeedDmaWrite(bth.opcode);
        let isReqNeedDMAWrite    = rdmaReqNeedDmaWrite(bth.opcode);

        let rdmaOpCodeNeedDMA    = isRespNeedDMAWrite || isReqNeedDMAWrite;
        

        let reqStatus        = RDMA_REQ_ST_NORMAL;

        let isAccCheckPass = False;
        case ({ pack(isSendReq || isWriteReq), pack(isReadReq), pack(isAtomicReq) })
            3'b100: begin
                isAccCheckPass = containAccessTypeFlag(qpc.rqAccessFlags, IBV_ACCESS_REMOTE_WRITE);
            end
            3'b010: begin
                isAccCheckPass = containAccessTypeFlag(qpc.rqAccessFlags, IBV_ACCESS_REMOTE_READ);
            end
            3'b001: begin
                isAccCheckPass = containAccessTypeFlag(qpc.rqAccessFlags, IBV_ACCESS_REMOTE_ATOMIC);
            end
            default: begin
                immFail(
                    "unreachible case @ mkReqHandleRQ",
                    $format(
                        "isSendReq=", fshow(isSendReq),
                        ", isWriteReq=", fshow(isWriteReq),
                        ", isReadReq=", fshow(isReadReq),
                        ", isAtomicReq=", fshow(isAtomicReq)
                    )
                );
            end
        endcase

        let isMrKeyMatch           = False;
        let isAccTypeMatch         = False;
        let isAccessRangeCheckPass = False;
        let needWaitForPGTResponse = False;
        if (mrMaybe matches tagged Valid .mr) begin
            isMrKeyMatch    = (truncate(reth.rkey) == mr.keyPart);
            let reqAccFlags = genAccessFlagFromReqType(isSendReq, isReadReq, isWriteReq, isAtomicReq);
            isAccTypeMatch  = compareAccessTypeFlags(mr.accFlags, reqAccFlags);

            isAccessRangeCheckPass = checkAddrAndLenWithinRange(
                reth.va,
                zeroExtend(pktMetaData.pktPayloadLen),
                mr.baseVA,
                mr.len
            );

            $display(
                "reth.va=", fshow(reth.va), 
                "pktMetaData.pktPayloadLen=", fshow(pktMetaData.pktPayloadLen),
                "mr.baseVA=", mr.baseVA,
                "mr.len=", mr.len);
            
            pgtQueryCltInst.putReq(PgtAddrTranslateReq{
                mrEntry: mr,
                addrToTrans: reth.va
            });
            needWaitForPGTResponse = True;
        end

        if (!isAccCheckPass) begin
            reqStatus = RDMA_REQ_ST_INV_ACC_FLAG;
        end
        else if (!isSupportedReqOpCode) begin
            reqStatus = RDMA_REQ_ST_INV_OPCODE;
        end
        else if (!isMrKeyMatch) begin
            reqStatus = RDMA_REQ_ST_INV_MR_KEY;
        end
        else if (!isAccTypeMatch) begin
            reqStatus = RDMA_REQ_ST_INV_ACC_FLAG;
        end
        else if (!isAccessRangeCheckPass) begin
            reqStatus = RDMA_REQ_ST_INV_MR_REGION;
        end
        
        getPGTQueryRespPipeQ.enq(tuple4(pktMetaDataAndQpc, reqStatus, needWaitForPGTResponse, rdmaOpCodeNeedDMA));



    endrule

    rule recvAddrTransRespAndIssueDMA;
        let { pktMetaDataAndQpc, reqStatus, needWaitForPGTResponse, rdmaOpCodeNeedDMA } = getPGTQueryRespPipeQ.first;
        getPGTQueryRespPipeQ.deq;

        let pktMetaData = pktMetaDataAndQpc.metadata;

        let phyAddr = 0;
        if (needWaitForPGTResponse) begin
            phyAddr <- pgtQueryCltInst.getResp;
        end

        Bool needIssueDMARequest = (
            needWaitForPGTResponse && 
            rdmaOpCodeNeedDMA      &&
            reqStatus == RDMA_REQ_ST_NORMAL
        );

        if (needIssueDMARequest) begin
            payloadConsumerControlClt.putReq(PayloadConReq{
                fragNum: pktMetaData.pktFragNum,
                consumeInfo: tagged SendWriteReqReadRespInfo DmaWriteMetaData {
                    startAddr: phyAddr,
                    len      : pktMetaData.pktPayloadLen
                }
            });
        end 
        else begin
            let req <- genDiscardPayloadReq(pktMetaData.pktFragNum, pktMetaData.pktPayloadLen);
            payloadConsumerControlClt.putReq(req);
        end

        waitDMARespPipeQ.enq(tuple3(pktMetaDataAndQpc, reqStatus, needIssueDMARequest));
        $display("qqqqqqqqq, reqStatus=", fshow(reqStatus));
    endrule

    rule waitDMAFinishAndWriteMetaToHost;
        let { pktMetaDataAndQpc, reqStatus, needIssueDMARequest } = waitDMARespPipeQ.first;
        waitDMARespPipeQ.deq;

        let pktMetaData = pktMetaDataAndQpc.metadata;
        let rdmaHeader  = pktMetaData.pktHeader;
        

        let bth   = extractBTH(rdmaHeader.headerData);
        let reth  = extractPriRETH(rdmaHeader.headerData, bth.trans);
        let rethSecondary  = extractSecRETH(rdmaHeader.headerData, bth.trans);



        if (needIssueDMARequest) begin
            let _ <- payloadConsumerControlClt.getResp;
        end

        C2hReportEntry rptEntry = ?;

        rptEntry.trans = bth.trans;
        rptEntry.opcode = bth.opcode;
        rptEntry.solicited = bth.solicited;
        rptEntry.dqpn = bth.dqpn;
        rptEntry.psn = bth.psn;

        rptEntry.va = reth.va;
        rptEntry.rkey = reth.rkey;
        rptEntry.dlen = reth.dlen;




    endrule



    interface pktMetaDataPipeIn = toPipeIn(rdmaPktMetaDataInQ);
    interface mrTableQueryClt   = mrTableQueryCltInst.clt;
    interface pgtQueryClt       = pgtQueryCltInst.clt;
    interface payloadXonsumerControlPortClt = payloadConsumerControlClt.clt;
    
endmodule
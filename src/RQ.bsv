import FIFOF :: *;

import PipeIn :: *;
import ClientServer :: *;
import RdmaUtils :: *;
import MetaData :: *;
import DataTypes :: *;
import Headers :: *;
import PAClib :: *;
import PipeIn :: *;
import PrimUtils :: *;

import UserLogicTypes :: *;


interface RQ;
    interface PipeIn#(RdmaPktMetaDataAndQPC) pktMetaDataPipeIn;
    interface Client#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) mrTableQueryClt;
    interface Client#(PgtAddrTranslateReq, ADDR) pgtQueryClt;
    interface Client#(PayloadConReq, PayloadConResp) payloadXonsumerControlPortClt;
    interface PipeOut#(C2hReportEntry) pktReportEntryPipeOut;
endinterface


(* synthesize *)
module mkRQ(RQ ifc);
    FIFOF#(RdmaPktMetaDataAndQPC)     rdmaPktMetaDataInQ  <- mkFIFOF;
    FIFOF#(C2hReportEntry)              pktReportEntryQ     <- mkFIFOF;

    BypassClient#(MrTableQueryReq, Maybe#(MemRegionTableEntry)) mrTableQueryCltInst  <- mkBypassClient;
    BypassClient#(PgtAddrTranslateReq, ADDR) pgtQueryCltInst                         <- mkBypassClient;
    BypassClient#(PayloadConReq, PayloadConResp) payloadConsumerControlClt           <- mkBypassClient;

    // Pipeline FIFOs
    FIFOF#(RdmaPktMetaDataAndQPC)                                      getMRQueryRespPipeQ <- mkFIFOF;
    FIFOF#(Tuple4#(RdmaPktMetaDataAndQPC, RdmaReqStatus, Bool, Bool)) getPGTQueryRespPipeQ <- mkFIFOF;
    FIFOF#(Tuple3#(RdmaPktMetaDataAndQPC, RdmaReqStatus, Bool))           waitDMARespPipeQ <- mkFIFOF;


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
                consumeInfo: tagged SendWriteReqReadRespInfo DmaWriteMetaDataNew {
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
    endrule

    rule waitDMAFinishAndWriteMetaToHost;
        let { pktMetaDataAndQpc, reqStatus, needIssueDMARequest } = waitDMARespPipeQ.first;
        waitDMARespPipeQ.deq;

        let pktMetaData = pktMetaDataAndQpc.metadata;
        let rdmaHeader  = pktMetaData.pktHeader;
        

        let bth   = extractBTH(rdmaHeader.headerData);
        let reth  = extractPriRETH(rdmaHeader.headerData, bth.trans);
        let rethSecondary  = extractSecRETH(rdmaHeader.headerData, bth.trans, bth.opcode);
        let aeth  = extractAETH(rdmaHeader.headerData);
        let nreth = extractNRETH(rdmaHeader.headerData);
        let immDT  = extractImmDt(rdmaHeader.headerData, bth.opcode, bth.trans);


        if (needIssueDMARequest) begin
            let _ <- payloadConsumerControlClt.getResp;
        end

        C2hReportEntry rptEntry = ?;

        rptEntry.reqStatus      = reqStatus;

        rptEntry.trans          = bth.trans;
        rptEntry.opcode         = bth.opcode;
        rptEntry.solicited      = bth.solicited;
        rptEntry.dqpn           = bth.dqpn;
        rptEntry.psn            = bth.psn;

        rptEntry.va             = reth.va;
        rptEntry.rkey           = reth.rkey;
        rptEntry.dlen           = reth.dlen;

        rptEntry.secondaryVa    = rethSecondary.va;
        rptEntry.secondaryRkey  = rethSecondary.rkey;

        rptEntry.code           = aeth.code;
        rptEntry.value          = aeth.value;
        rptEntry.msn            = aeth.msn;
        rptEntry.lastRetryPSN   = nreth.lastRetryPSN;

        rptEntry.immDt          = immDT.data;


        pktReportEntryQ.enq(rptEntry);
    endrule



    interface pktMetaDataPipeIn             = toPipeIn(rdmaPktMetaDataInQ);
    interface mrTableQueryClt               = mrTableQueryCltInst.clt;
    interface pgtQueryClt                   = pgtQueryCltInst.clt;
    interface payloadXonsumerControlPortClt = payloadConsumerControlClt.clt;
    interface pktReportEntryPipeOut         = toPipeOut(pktReportEntryQ);
endmodule


interface RQReportEntryToRingbufDesc;
    interface PipeIn#(C2hReportEntry) pktReportEntryPipeIn;
    interface PipeOut#(RingbufRawDescriptor) ringbufDescPipeOut;
endinterface

typedef enum {
    RQReportEntryToRingbufDescStatusOutputBasicInfo,
    RQReportEntryToRingbufDescStatusOutputExtraInfo
} RQReportEntryToRingbufDescStatus deriving(Bits, Eq);

module mkRQReportEntryToRingbufDesc(RQReportEntryToRingbufDesc);
    FIFOF#(C2hReportEntry) pktReportEntryPipeInQ <- mkFIFOF;
    FIFOF#(RingbufRawDescriptor) ringbufDescPipeOutQ <- mkFIFOF;

    Reg#(RQReportEntryToRingbufDescStatus) state <- mkReg(RQReportEntryToRingbufDescStatusOutputBasicInfo);

    rule outputDesc ;
        let reportEntry = pktReportEntryPipeInQ.first;
        RingbufRawDescriptor ent;

        let bth = PktMeatReportQueueDescFragBTH {
            trans:      reportEntry.trans,
            opcode:     reportEntry.opcode,
            dqpn:       reportEntry.dqpn,
            psn:        reportEntry.psn,
            solicited:  reportEntry.solicited,
            ackReq:     reportEntry.ackReq,
            reserved1:  unpack(0)
        };

        let reth = PktMeatReportQueueDescFragRETH {
            va: reportEntry.va,
            rkey: reportEntry.rkey,
            dlen: reportEntry.dlen
        };

        let secReth = PktMeatReportQueueDescFragSecondaryRETH {
            secondaryRkey: reportEntry.secondaryRkey,
            secondaryVa  : reportEntry.secondaryVa
        };

        let aeth = PktMeatReportQueueDescFragAETH {
            code:           reportEntry.code,
            value:          reportEntry.value,
            msn:            reportEntry.msn,
            lastRetryPSN:   reportEntry.lastRetryPSN
        };

        let immDt = PktMeatReportQueueDescFragImmDT {
            data:           reportEntry.immDt
        };

        let opcode = {pack(reportEntry.trans), pack(reportEntry.opcode)};
        if (state == RQReportEntryToRingbufDescStatusOutputBasicInfo) begin
            case (opcode)
                fromInteger(valueOf(RC_SEND_FIRST)),
                fromInteger(valueOf(RC_SEND_MIDDLE)),
                fromInteger(valueOf(RC_SEND_LAST)),
                fromInteger(valueOf(RC_SEND_ONLY)):
                begin
                    ent = pack(PktMeatReportQueueDescBth{
                        reqStatus   :   reportEntry.reqStatus,
                        bth         :   bth,
                        reserved1   :   unpack(0)
                    });
                    ringbufDescPipeOutQ.enq(pack(ent));
                    pktReportEntryPipeInQ.deq;
                end
                fromInteger(valueOf(RC_RDMA_WRITE_FIRST)),
                fromInteger(valueOf(RC_RDMA_WRITE_MIDDLE)),
                fromInteger(valueOf(RC_RDMA_WRITE_LAST)),
                fromInteger(valueOf(RC_RDMA_WRITE_LAST_WITH_IMMEDIATE)),
                fromInteger(valueOf(RC_RDMA_WRITE_ONLY)),
                fromInteger(valueOf(RC_RDMA_WRITE_ONLY_WITH_IMMEDIATE)),
                fromInteger(valueOf(RC_RDMA_READ_REQUEST)),
                fromInteger(valueOf(RC_RDMA_READ_RESPONSE_FIRST)),
                fromInteger(valueOf(RC_RDMA_READ_RESPONSE_MIDDLE)),
                fromInteger(valueOf(RC_RDMA_READ_RESPONSE_LAST)),
                fromInteger(valueOf(RC_RDMA_READ_RESPONSE_ONLY)),
                fromInteger(valueOf(XRC_RDMA_READ_RESPONSE_FIRST)),
                fromInteger(valueOf(XRC_RDMA_READ_RESPONSE_MIDDLE)),
                fromInteger(valueOf(XRC_RDMA_READ_RESPONSE_LAST)),
                fromInteger(valueOf(XRC_RDMA_READ_RESPONSE_ONLY)):
                begin
                    ent = pack(PktMeatReportQueueDescBthRethImmDT{
                        reqStatus   :   reportEntry.reqStatus,
                        bth         :   bth,
                        reth        :   reth,
                        immDt       :   immDt,
                        reserved1   :   unpack(0)
                    });
                    if (opcode == fromInteger(valueOf(RC_RDMA_READ_REQUEST))) begin
                        state <= RQReportEntryToRingbufDescStatusOutputExtraInfo;
                    end
                    else begin
                        pktReportEntryPipeInQ.deq;
                    end
                    ringbufDescPipeOutQ.enq(pack(ent));
                end
                fromInteger(valueOf(RC_ACKNOWLEDGE)),
                fromInteger(valueOf(RC_ATOMIC_ACKNOWLEDGE)),
                fromInteger(valueOf(XRC_ACKNOWLEDGE)),
                fromInteger(valueOf(XRC_ATOMIC_ACKNOWLEDGE)):
                begin
                    ent = pack(PktMeatReportQueueDescBthAeth{
                        reqStatus   :   reportEntry.reqStatus,
                        bth         :   bth,
                        aeth        :   aeth,
                        reserved1   :   unpack(0)
                    });
                    pktReportEntryPipeInQ.deq;
                    ringbufDescPipeOutQ.enq(pack(ent));
                end

                default: begin
                    pktReportEntryPipeInQ.deq;
                    $display("Warn: Received Not Supported Packet, Will not report to software.");
                end
            endcase
        end 
        else if (state == RQReportEntryToRingbufDescStatusOutputExtraInfo) begin
            case (reportEntry.opcode)
                RDMA_READ_REQUEST: begin
                    ent = pack(PktMeatReportQueueDescSecondaryReth{
                        secReth     :   secReth,
                        reserved1   :   unpack(0)
                    });
                    
                    state <= RQReportEntryToRingbufDescStatusOutputBasicInfo;
                    ringbufDescPipeOutQ.enq(pack(ent));
                    pktReportEntryPipeInQ.deq;
                end
                default: begin
                    $display("Warn: Received Not Supported Packet, Will not report to software.");
                end
            endcase
        end

        
    endrule






    interface pktReportEntryPipeIn  = toPipeIn(pktReportEntryPipeInQ);
    interface ringbufDescPipeOut    = toPipeOut(ringbufDescPipeOutQ);

endmodule
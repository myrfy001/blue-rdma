import FIFOF :: *;
import PAClib :: *;
import Vector :: *;
import ClientServer :: *;
import GetPut :: *;
import BRAM :: *;

import DataTypes :: *;
import ExtractAndPrependPipeOut :: *;
import Headers :: *;
import MetaData :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;

function Bool checkZeroFields4BTH(BTH bth);
    let bthRsvdCheck =
        isZero(pack(bth.tver))  &&
        isZero(pack(bth.fecn))  &&
        isZero(pack(bth.becn))  &&
        isZero(pack(bth.resv6)) &&
        isZero(pack(bth.resv7));
    return bthRsvdCheck;
endfunction

function Bool padCntCheckReqHeader(BTH bth);
    let zeroPadCntCheck = isZero(bth.padCnt);

    return case (bth.opcode)
        SEND_MIDDLE            : zeroPadCntCheck;
        SEND_FIRST                         ,
        SEND_LAST, SEND_ONLY               ,
        SEND_LAST_WITH_IMMEDIATE           ,
        SEND_ONLY_WITH_IMMEDIATE           ,
        SEND_LAST_WITH_INVALIDATE          ,
        SEND_ONLY_WITH_INVALIDATE          : True;

        RDMA_WRITE_MIDDLE: zeroPadCntCheck ;
        RDMA_WRITE_FIRST                   ,
        RDMA_WRITE_LAST, RDMA_WRITE_ONLY   ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE     ,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE     : True;

        RDMA_READ_REQUEST                  ,
        COMPARE_SWAP                       ,
        FETCH_ADD                          : zeroPadCntCheck;

        default                            : False;
    endcase;
endfunction

// TODO: verify that read/atomic response can only have normal AETH code
function Bool padCntCheckRespHeader(BTH bth, AETH aeth);
    let zeroPadCntCheck = isZero(bth.padCnt);

    case (bth.opcode)
        RDMA_READ_RESPONSE_MIDDLE: return zeroPadCntCheck;
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  : return aeth.code == AETH_CODE_ACK;
        RDMA_READ_RESPONSE_FIRST ,
        ATOMIC_ACKNOWLEDGE       : return aeth.code == AETH_CODE_ACK && zeroPadCntCheck;
        ACKNOWLEDGE              : case (aeth.code)
            AETH_CODE_ACK,
            AETH_CODE_RNR: return zeroPadCntCheck;
            AETH_CODE_NAK: return case (aeth.value)
                zeroExtend(pack(AETH_NAK_SEQ_ERR)),
                zeroExtend(pack(AETH_NAK_INV_REQ)),
                zeroExtend(pack(AETH_NAK_RMT_ACC)),
                zeroExtend(pack(AETH_NAK_RMT_OP)) ,
                zeroExtend(pack(AETH_NAK_INV_RD)) : zeroPadCntCheck;
                default                           : False;
            endcase;
            // AETH_CODE_RSVD
            default: return False;
        endcase
        default: return False;
    endcase
endfunction

// TODO: check XRC domain match
function Bool validateHeader(TransType transType, QKEY qkey, EntryCommonQPC qpcCommon);
    let transTypeMatch = transTypeMatchQpType(transType, qpcCommon.qpType, True);
    return transTypeMatch;
endfunction



interface HeaderAndMetaDataAndPayloadSeperateDataStreamPipeOut;
    interface HeaderDataStreamAndMetaDataPipeOut headerAndMetaData;
    interface DataStreamFragMetaPipeOut payloadStreamFragMetaPipeOut;
    interface RqDataStreamWithRawPacketFlagPipeIn rdmaPktPipeIn;
    interface Client#(DATA, InputStreamFragBufferIdx) payloadStreamFragStorageInsertClt;
endinterface

// After extract header from rdmaPktPipeIn,
// it outputs header DataStream and payload DataStream,
// and every header DataStream has corresponding payload DataStream,
// if header has no payload, then output empty payload DataStream.
// This module will not discard invalid packet.
(* synthesize *)
module mkExtractHeaderFromRdmaPktPipeOut(HeaderAndMetaDataAndPayloadSeperateDataStreamPipeOut);
    FIFOF#(RqDataStreamWithRawPacketFlag) rdmaPktPipeInQ <- mkFIFOF;

    FIFOF#(HeaderMetaData) headerMetaDataInQ <- mkFIFOF;
    FIFOF#(DataStream) dataInQ <- mkFIFOF;
    

    Vector#(2, PipeOut#(HeaderMetaData)) headerMetaDataPipeOutVec <-
        mkForkVector(toPipeOut(headerMetaDataInQ));
    let headerMetaDataPipeIn = headerMetaDataPipeOutVec[0];
    let headerMetaDataPipeOut <- mkBuffer(headerMetaDataPipeOutVec[1]);
    let dataPipeIn = toPipeOut(dataInQ);
    let headerAndPayloadPipeOut <- mkExtractHeaderFromDataStreamPipeOut(
        dataPipeIn, headerMetaDataPipeIn
    );

    rule debug;
        if (!rdmaPktPipeInQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkExtractHeaderFromRdmaPktPipeOut rdmaPktPipeInQ");
        end
        if (!headerMetaDataInQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkExtractHeaderFromRdmaPktPipeOut headerMetaDataInQ");
        end
        if (!dataInQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkExtractHeaderFromRdmaPktPipeOut dataInQ");
        end
       
    endrule

    rule extractHeader;
        let {rdmaPktDataStream, isEmptyHeader} = rdmaPktPipeInQ.first;
        rdmaPktPipeInQ.deq;
        dataInQ.enq(rdmaPktDataStream);

        if (rdmaPktDataStream.isFirst) begin
            let { transType, rdmaOpCode } =
                extractTranTypeAndRdmaOpCode(rdmaPktDataStream.data);

            let headerHasPayload = rdmaOpCodeHasPayload(rdmaOpCode);
            HeaderByteNum headerLen = fromInteger(
                calcHeaderLenByTransTypeAndRdmaOpCode(transType, rdmaOpCode)
            );
            immAssert(
                !isZero(headerLen),
                "!isZero(headerLen) assertion @ mkExtractHeaderFromRdmaPktPipeOut",
                $format(
                    "headerLen=%0d should not be zero, transType=",
                    headerLen, fshow(transType),
                    ", rdmaOpCode=", fshow(rdmaOpCode)
                )
            );

            let headerMetaData = genHeaderMetaData(headerLen, headerHasPayload, isEmptyHeader);
            headerMetaDataInQ.enq(headerMetaData);
            // $display(
            //     "time=%0t: extractHeader", $time,
            //     ", headerLen=%0d", headerLen,
            //     ", rdmaOpCode=", fshow(rdmaOpCode),
            //     ", transType=", fshow(transType),
            //     ", rdmaPktDataStream=", fshow(rdmaPktDataStream),
            //     ", headerHasPayload=", fshow(headerHasPayload),
            //     ", headerMetaData=", fshow(headerMetaData)
            // );
        end
        // $display("time=%0t: rdmaPktDataStream=", $time, fshow(rdmaPktDataStream));
    endrule

    interface headerAndMetaData = interface HeaderDataStreamAndMetaDataPipeOut;
        interface headerDataStream = headerAndPayloadPipeOut.header;
        interface headerMetaData = headerMetaDataPipeOut;
    endinterface;
    interface payloadStreamFragMetaPipeOut = headerAndPayloadPipeOut.payloadStreamFragMetaPipeOut;
    interface rdmaPktPipeIn = toPut(rdmaPktPipeInQ);
    interface payloadStreamFragStorageInsertClt = headerAndPayloadPipeOut.payloadStreamFragStorageInsertClt;
endmodule

interface InputRdmaPktBuf;
    interface RdmaPktMetaDataAndQpcAndPayloadPipeOut reqPktPipeOut;
    
    interface DataStreamPipeIn headerDataStreamPipeIn;
    interface Put#(HeaderMetaData) headerMetaDataPipeIn;
    interface DataStreamFragMetaPipeIn payloadStreamFragMetaPipeIn;

    interface Client#(ReadReqCommonQPC, Maybe#(EntryCommonQPC)) qpcReadCommonClt;
endinterface

typedef enum {
    RDMA_PKT_BUT_ST_PRE_CHECK_FRAG,
    RDMA_PKT_BUF_ST_DISCARD_FRAG
} RdmaPktBufState deriving(Bits, Eq);

typedef struct {
    QPN dqpn;
    QKEY qkeyDETH;
    Bool isLastPkt;
    Bool isFirstOrMidPkt;
    Bool isLastOrOnlyPkt;
} HeaderValidateInfo deriving(Bits);

typedef struct {
    QPN  dqpn;
    PMTU pmtu;
    Bool isValidHeader;
    Bool isLastPkt;
    Bool isFirstOrMidPkt;
    Bool isLastOrOnlyPkt;
} ValidHeaderInfo deriving(Bits);

typedef struct {
    PAD         padCnt;
    HeaderRDMA  rdmaHeader;
    PktFragNum  pktFragNum;
    PktLen      pktLen;
    PMTU        pmtu;
    Bool        pktValid;
    Bool        isFirstOrMidPkt;
    Bool        isLastOrOnlyPkt;
    Bool        isMidPkt;
} PktLenCheckInfo deriving(Bits);




// This module will discard:
// - invalid packet that header is without payload but packet has payload;
// TODO: check write requests have non-zero RETH.dlen but without payload
// TODO: check remote XRC domain and XRCETH valid?
// TODO: reset mkInputRdmaPktBufAndHeaderValidation when error or retry?
(* synthesize *)
module mkInputRdmaPktBufAndHeaderValidation(InputRdmaPktBuf);
    // Output FIFO for PipeOut
    FIFOF#(DataStreamFragMetaData)   reqPayloadFragMetaOutQ <- mkFIFOF;
    FIFOF#(RdmaPktMetaDataAndQPC)  reqPktMetaDataAndQpcOutQ <- mkFIFOF;

    // Pipeline buffers
    FIFOF#(Tuple4#(HeaderRDMA, Bool, Bool, Bool))                                             rdmaHeaderRecvQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                                       payloadFragMetaRecvQ <- mkFIFOF;

    FIFOF#(HeaderRDMA)                                                                    rdmaHeaderPreCheckQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                                   payloadFragMetaPreCheckQ <- mkFIFOF;

    FIFOF#(Tuple2#(HeaderRDMA, HeaderValidateInfo))                                     rdmaHeaderValidationQ <- mkSizedFIFOF(valueOf(QPC_QUERY_RESP_MAX_DELAY));
    FIFOF#(DataStreamFragMetaData)                                                 payloadFragMetaValidationQ <- mkSizedFIFOF(valueOf(QPC_QUERY_RESP_MAX_DELAY));

    FIFOF#(Tuple3#(HeaderRDMA, Maybe#(EntryCommonQPC), ValidHeaderInfo))                    rdmaHeaderFilterQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                                     payloadFragMetaFilterQ <- mkFIFOF;

    FIFOF#(Tuple3#(HeaderRDMA, EntryCommonQPC, ValidHeaderInfo))                       rdmaHeaderFragLenCalcQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                                payloadFragMetaFragLenCalcQ <- mkFIFOF;

    FIFOF#(Tuple3#(HeaderRDMA, EntryCommonQPC, ValidHeaderInfo))                        rdmaHeaderPktLenCalcQ <- mkFIFOF;
    FIFOF#(Tuple5#(DataStreamFragMetaData, ByteEnBitNum, ByteEnBitNum, Bool, Bool))        payloadPktLenCalcQ <- mkFIFOF;

    FIFOF#(Tuple2#(PktLenCheckInfo, EntryCommonQPC))                                rdmaHeaderPktLenPreCheckQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                             payloadFragMetaPktLenPreCheckQ <- mkFIFOF;

    FIFOF#(Tuple5#(PktLenCheckInfo, EntryCommonQPC, Bool, Bool, Bool))                 rdmaHeaderPktLenCheckQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData)                                                payloadFragMetaPktLenCheckQ <- mkFIFOF;


    Reg#(Bool)        isValidPktReg <- mkRegU;
    Reg#(PAD)          bthPadCntReg <- mkRegU;
    Reg#(PktFragNum)  pktFragNumReg <- mkRegU;
    Reg#(PktLen)          pktLenReg <- mkRegU;
    Reg#(Bool)          pktValidReg <- mkRegU;

    Reg#(RdmaPktBufState) pktBufStateReg <- mkReg(RDMA_PKT_BUT_ST_PRE_CHECK_FRAG);

    FIFOF#(DataStreamFragMetaData) payloadStreamFragMetaPipeInQ <- mkFIFOF;
    FIFOF#(DataStream) headerDataStreamQ                        <- mkFIFOF;
    let headerMetaDataQ                                         <- mkFIFOF;

    let rdmaHeaderPipeOut <- mkDataStream2Header(
        toPipeOut(headerDataStreamQ),
        toPipeOut(headerMetaDataQ)
    );

    BypassClient#(ReadReqCommonQPC, Maybe#(EntryCommonQPC)) qpcReadCommonCltInst <- mkBypassClient("qpcReadCommonCltInst");

    function Bool fifofNotEmpty(FIFOF#(anytype) fifof) = fifof.notEmpty;
    function Bool fifofNotFull(FIFOF#(anytype) fifof) = fifof.notFull;
    function Bool fifofVecAll(
        function Bool mapFunc(FIFOF#(anytype) fifof),
        Vector#(vSz, FIFOF#(anytype)) fifofVec
    ) provisos(Add#(1, anysize, vSz));
        let fifofMapVec = map(mapFunc, fifofVec);
        let result = fold(\&& , fifofMapVec);
        return result;
    endfunction


    rule debug;
        if (!rdmaHeaderRecvQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderRecvQ");
        end
        if (!payloadFragMetaRecvQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaRecvQ");
        end
        if (!rdmaHeaderPreCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderPreCheckQ");
        end
        if (!payloadFragMetaPreCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaPreCheckQ");
        end
        if (!rdmaHeaderValidationQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderValidationQ");
        end
        if (!payloadFragMetaValidationQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaValidationQ");
        end
        if (!rdmaHeaderFilterQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderFilterQ");
        end
        if (!payloadFragMetaFilterQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaFilterQ");
        end
        if (!rdmaHeaderFragLenCalcQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderFragLenCalcQ");
        end
        if (!rdmaHeaderPktLenCalcQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderPktLenCalcQ");
        end
        if (!payloadPktLenCalcQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadPktLenCalcQ");
        end
        if (!rdmaHeaderPktLenPreCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderPktLenPreCheckQ");
        end
        if (!payloadFragMetaPktLenPreCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaPktLenPreCheckQ");
        end
        if (!rdmaHeaderPktLenCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: rdmaHeaderPktLenCheckQ");
        end
        if (!payloadFragMetaPktLenCheckQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: payloadFragMetaPktLenCheckQ");
        end
    endrule






    (* conflict_free = "recvPktFrag, \
                        preCheckHeader, \
                        discardInvalidFrag, \
                        prepareValidation, \
                        checkMetaDataQP, \
                        discardInvalidHeaderPkt, \
                        calcFraglen, \
                        calcPktLen, \
                        preCheckPktLen, \
                        checkPktLen" *)
    rule recvPktFrag;
        let payloadFragMeta = payloadStreamFragMetaPipeInQ.first;
        payloadStreamFragMetaPipeInQ.deq;
        let payloadHasSingleFrag = payloadFragMeta.isFirst && payloadFragMeta.isLast;
        let fragHasNoData = isZeroByteEn(payloadFragMeta.byteEn);

        if (payloadFragMeta.isFirst) begin
            let rdmaHeader = rdmaHeaderPipeOut.first;
            let bth        = extractBTH(rdmaHeader.headerData);
            let aeth       = extractAETH(rdmaHeader.headerData);

            let bthCheckResult = checkZeroFields4BTH(bth);
            let headerCheckResult =
                padCntCheckReqHeader(bth) || padCntCheckRespHeader(bth, aeth);
            // Discard packet that should not have payload
            let nonPayloadHeaderShouldHaveNoPayload =
                rdmaHeader.headerMetaData.hasPayload ?
                    True : (payloadHasSingleFrag && fragHasNoData);

            rdmaHeaderPipeOut.deq;
            rdmaHeaderRecvQ.enq(tuple4(
                rdmaHeader, bthCheckResult, headerCheckResult, nonPayloadHeaderShouldHaveNoPayload
            ));
            // $display(
            //     "time=%0t: recvPktFrag", $time,
            //     ", bthCheckResult=", fshow(bthCheckResult),
            //     ", headerCheckResult=", fshow(headerCheckResult),
            //     ", nonPayloadHeaderShouldHaveNoPayload=",
            //     fshow(nonPayloadHeaderShouldHaveNoPayload),
            //     ", bth=", fshow(bth), ", aeth=", fshow(aeth)
            // );
        end

        payloadFragMetaRecvQ.enq(payloadFragMeta);
        $display(
            "time=%0t: 1st stage recvPktFrag", $time
            // ", bth=", fshow(bth), ", aeth=", fshow(aeth)
        );
    endrule

    rule preCheckHeader if (pktBufStateReg == RDMA_PKT_BUT_ST_PRE_CHECK_FRAG);
        let streamFragMeta = payloadFragMetaRecvQ.first;
        payloadFragMetaRecvQ.deq;

        if (streamFragMeta.isFirst) begin
            let {
                rdmaHeader, bthCheckResult, headerCheckResult, nonPayloadHeaderShouldHaveNoPayload
            } = rdmaHeaderRecvQ.first;
            rdmaHeaderRecvQ.deq;

            let bth = extractBTH(rdmaHeader.headerData);
            $display("bthCheckResult=", bthCheckResult, "headerCheckResult=", headerCheckResult, "nonPayloadHeaderShouldHaveNoPayload=", nonPayloadHeaderShouldHaveNoPayload);
            if (bthCheckResult && headerCheckResult && nonPayloadHeaderShouldHaveNoPayload) begin
                // Packet header is valid
                rdmaHeaderPreCheckQ.enq(rdmaHeader);
                payloadFragMetaPreCheckQ.enq(streamFragMeta);

                // $display(
                //     "time=%0t: bth=", $time, fshow(bth),
                //     ", headerMetaData=", fshow(rdmaHeader.headerMetaData),
                //     "\ntime=%0t: streamFragMeta=", $time, fshow(streamFragMeta)
                // );
            end
            else begin
                if (!streamFragMeta.isLast) begin
                    $warning(
                        "time=%0t: InputRdmaPktBuf preCheckHeader", $time,
                        ", discard invalid RDMA packet of multi-fragment payload"
                    );
                    pktBufStateReg <= RDMA_PKT_BUF_ST_DISCARD_FRAG;
                end
                else begin
                    $warning(
                        "time=%0t: InputRdmaPktBuf preCheckHeader", $time,
                        ", discard invalid RDMA packet of single-fragment payload"
                    );
                end
            end
        end
        else begin
            payloadFragMetaPreCheckQ.enq(streamFragMeta);
            // $display("time=%0t: streamFragMeta=", $time, fshow(streamFragMeta));
        end
        $display(
            "time=%0t: 2nd-1 stage preCheckHeader", $time
            // ", bthCheckResult=", fshow(bthCheckResult),
            // ", headerCheckResult=", fshow(headerCheckResult),
            // ", nonPayloadHeaderShouldHaveNoPayload=",
            // fshow(nonPayloadHeaderShouldHaveNoPayload),
            // ", bth=", fshow(bth)
        );
    endrule

    rule discardInvalidFrag if (pktBufStateReg == RDMA_PKT_BUF_ST_DISCARD_FRAG);
        let streamFragMeta = payloadFragMetaRecvQ.first;
        payloadFragMetaRecvQ.deq;
        if (streamFragMeta.isLast) begin
            pktBufStateReg <= RDMA_PKT_BUT_ST_PRE_CHECK_FRAG;
        end
        // $display("time=%0t: 2nd-2 stage discardInvalidFrag", $time);
    endrule

    rule prepareValidation;
        let streamFragMeta = payloadFragMetaPreCheckQ.first;
        payloadFragMetaPreCheckQ.deq;

        if (streamFragMeta.isFirst) begin
            let rdmaHeader = rdmaHeaderPreCheckQ.first;
            rdmaHeaderPreCheckQ.deq;

            let bth    = extractBTH(rdmaHeader.headerData);
            let deth   = extractDETH(rdmaHeader.headerData);
            let xrceth = extractXRCETH(rdmaHeader.headerData);

            let isLastPkt       = isLastRdmaOpCode(bth.opcode);
            let isFirstOrMidPkt = isFirstOrMiddleRdmaOpCode(bth.opcode);
            let isLastOrOnlyPkt = isLastOrOnlyRdmaOpCode(bth.opcode);
            let dqpn            = bth.dqpn;
            qpcReadCommonCltInst.putReq(ReadReqCommonQPC{qpn: dqpn});

            let headerValidateInfo = HeaderValidateInfo {
                dqpn           : dqpn,
                qkeyDETH       : deth.qkey,
                isLastPkt      : isLastPkt,
                isFirstOrMidPkt: isFirstOrMidPkt,
                isLastOrOnlyPkt: isLastOrOnlyPkt
            };
            rdmaHeaderValidationQ.enq(tuple2(rdmaHeader, headerValidateInfo));
        end

        // Notice: this fifo should be large enough to wait qpcReadCommonCltInst's response
        payloadFragMetaValidationQ.enq(streamFragMeta);
        // $display("time=%0t: 3rd stage prepareValidation", $time);
    endrule

    rule checkMetaDataQP;
        let streamFragMeta = payloadFragMetaValidationQ.first;
        payloadFragMetaValidationQ.deq;

        if (streamFragMeta.isFirst) begin
            let { rdmaHeader, headerValidateInfo } = rdmaHeaderValidationQ.first;
            rdmaHeaderValidationQ.deq;

            let bth    = extractBTH(rdmaHeader.headerData);
            let isLastPkt       = headerValidateInfo.isLastPkt;
            let isFirstOrMidPkt = headerValidateInfo.isFirstOrMidPkt;
            let isLastOrOnlyPkt = headerValidateInfo.isLastOrOnlyPkt;

            let isValidHeader = False;

            let qpcCommonMaybe <- qpcReadCommonCltInst.getResp;

            PMTU pmtu = IBV_MTU_256;
            if (qpcCommonMaybe matches tagged Valid .qpcCommon) begin

                isValidHeader = validateHeader(
                    bth.trans,
                    headerValidateInfo.qkeyDETH,
                    qpcCommon
                );
                pmtu = qpcCommon.pmtu;
            end
            // $display(
            //     "time=%0t: checkMetaDataQP", $time,
            //     ", dqpn=%h", headerValidateInfo.dqpn,
            //     ", bth.dqpn=%h", bth.dqpn,
            //     ", bth.psn=%h", bth.psn,
            //     ", bth.opcode=", fshow(bth.opcode)
            // );


            let validHeaderInfo = ValidHeaderInfo {
                dqpn           : headerValidateInfo.dqpn,
                pmtu           : pmtu,
                isValidHeader  : isValidHeader,
                isLastPkt      : isLastPkt,
                isFirstOrMidPkt: isFirstOrMidPkt,
                isLastOrOnlyPkt: isLastOrOnlyPkt
            };
            rdmaHeaderFilterQ.enq(tuple3(rdmaHeader, qpcCommonMaybe, validHeaderInfo));
        end

        payloadFragMetaFilterQ.enq(streamFragMeta);
        // $display("time=%0t: 4th stage checkMetaDataQP", $time);
    endrule

    rule discardInvalidHeaderPkt;
        let streamFragMeta = payloadFragMetaFilterQ.first;
        payloadFragMetaFilterQ.deq;

        let isValidPkt = isValidPktReg;

        if (streamFragMeta.isFirst) begin
            let { rdmaHeader, qpcCommonMaybe, validHeaderInfo } = rdmaHeaderFilterQ.first;
            rdmaHeaderFilterQ.deq;

            let bth           = extractBTH(rdmaHeader.headerData);
            let isValidHeader = validHeaderInfo.isValidHeader;

            if (isValidHeader) begin
                immAssert(
                    isValid(qpcCommonMaybe),
                    "isValid(qpcCommonMaybe) assertion @ mkInputRdmaPktBufAndHeaderValidation",
                    $format(
                        "qpcCommonMaybe=", fshow(qpcCommonMaybe), " should be valid"
                    )
                );
                rdmaHeaderFragLenCalcQ.enq(tuple3(rdmaHeader, fromMaybe(?, qpcCommonMaybe), validHeaderInfo));
            end
            else begin
                $display(
                    "time=%0t: found invalid header", $time,
                    ", isValidHeader=", fshow(isValidHeader)
                );
            end

            isValidPkt = isValidHeader;
            isValidPktReg <= isValidPkt;
        end

        // immAssert(
        //     isValidPkt,
        //     "isValidPkt assertion @ mkInputRdmaPktBufAndHeaderValidation",
        //     $format(
        //         "isValidPkt=", fshow(isValidPkt),
        //         " should be true"
        //     )
        // );

        if (isValidPkt) begin
            payloadFragMetaFragLenCalcQ.enq(streamFragMeta);
        end
        $display(
            "time=%0t: 5th stage discardInvalidHeaderPkt", $time,
            ", isValidPkt=", fshow(isValidPkt)
        );
    endrule

    rule calcFraglen;
        let streamFragMeta = payloadFragMetaFragLenCalcQ.first;
        payloadFragMetaFragLenCalcQ.deq;

        let bthPadCnt = bthPadCntReg;
        if (streamFragMeta.isFirst) begin
            let { rdmaHeader, qpcCommon, validHeaderInfo } = rdmaHeaderFragLenCalcQ.first;
            rdmaHeaderFragLenCalcQ.deq;

            let bth       = extractBTH(rdmaHeader.headerData);
            bthPadCnt     = bth.padCnt;
            bthPadCntReg <= bthPadCnt;

            rdmaHeaderPktLenCalcQ.enq(tuple3(rdmaHeader, qpcCommon, validHeaderInfo));

            // $display(
            //     "time=%0t: streamFragMeta.byteEn=%h, streamFragMeta.isFirst=",
            //     $time, streamFragMeta.byteEn, fshow(streamFragMeta.isFirst),
            //     ", streamFragMeta.isLast=", streamFragMeta.isLast, ", bth.psn=%h", bth.psn,
            //     ", bth.opcode=", fshow(bth.opcode), ", bth.padCnt=%h", bth.padCnt
            // );
        end
        
        let payloadFragLen = calcFragByteNumFromByteEn(streamFragMeta.byteEn);
        immAssert(
            isValid(payloadFragLen),
            "isValid(payloadFragLen) assertion @ mkInputRdmaPktBufAndHeaderValidation",
            $format(
                "payloadFragLen=", fshow(payloadFragLen), " should be valid"
            )
        );
        let fragLen         = unwrapMaybe(payloadFragLen);
        let isByteEnNonZero = !isZeroByteEn(streamFragMeta.byteEn);
        let isByteEnAllOne  = isAllOnesR(streamFragMeta.byteEn);
        ByteEnBitNum fragLenWithOutPad = fragLen - zeroExtend(bthPadCnt);

        payloadPktLenCalcQ.enq(tuple5(
            streamFragMeta, fragLen, fragLenWithOutPad, isByteEnNonZero, isByteEnAllOne
        ));
        // $display("time=%0t: 6th stage calcFraglen", $time);
    endrule

    rule calcPktLen;
        let {
            streamFragMeta, fragLen, fragLenWithOutPad, isByteEnNonZero, isByteEnAllOne
        } = payloadPktLenCalcQ.first;
        payloadPktLenCalcQ.deq;

        let { rdmaHeader, qpcCommon, validHeaderInfo } = rdmaHeaderPktLenCalcQ.first;

        let bth             = extractBTH(rdmaHeader.headerData);
        let pmtu            = validHeaderInfo.pmtu;

        let isLastPkt       = validHeaderInfo.isLastPkt;
        let isFirstOrMidPkt = validHeaderInfo.isFirstOrMidPkt;
        let isLastOrOnlyPkt = validHeaderInfo.isLastOrOnlyPkt;
        let isMidPkt        = isMiddleRdmaOpCode(bth.opcode);

        let pktLen = pktLenReg;
        let pktFragNum = pktFragNumReg;
        let pktValid = False;

        // PktLen fragLenExt = zeroExtend(fragLen);
        PktLen fragLenExtWithOutPad = zeroExtend(fragLenWithOutPad);
        case ({ pack(streamFragMeta.isFirst), pack(streamFragMeta.isLast) })
            2'b11: begin // streamFragMeta.isFirst && streamFragMeta.isLast
                pktLen = fragLenExtWithOutPad;
                pktFragNum = 1;
                pktValid = (isFirstOrMidPkt ? False : (isLastPkt ? isByteEnNonZero : True));
            end
            2'b10: begin // streamFragMeta.isFirst && !streamFragMeta.isLast
                pktLen = fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
                pktFragNum = 1;
                pktValid = isByteEnAllOne;
            end
            2'b01: begin // !streamFragMeta.isFirst && streamFragMeta.islast
                pktLen = pktLenAddFragLen(pktLenReg, fragLenWithOutPad);
                // pktLen = pktLenReg + fragLenExtWithOutPad;
                pktFragNum = pktFragNumReg + 1;
                pktValid = pktValidReg;
            end
            2'b00: begin // !streamFragMeta.isFirst && !streamFragMeta.islast
                pktLen = pktLenAddBusByteWidth(pktLenReg);
                // pktLen = pktLenReg + fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
                pktFragNum = pktFragNumReg + 1;
                pktValid = pktValidReg && isByteEnAllOne;
            end
        endcase

        pktLenReg     <= pktLen;
        pktValidReg   <= pktValid;
        pktFragNumReg <= pktFragNum;

        if (streamFragMeta.isLast) begin
            rdmaHeaderPktLenCalcQ.deq;

            let pktLenCheckInfo = PktLenCheckInfo {
                padCnt         : bth.padCnt,
                rdmaHeader     : rdmaHeader,
                pktFragNum     : pktFragNum,
                pktLen         : pktLen,
                pmtu           : pmtu,
                pktValid       : pktValid,
                isFirstOrMidPkt: isFirstOrMidPkt,
                isLastOrOnlyPkt: isLastOrOnlyPkt,
                isMidPkt       : isMidPkt
            };
            rdmaHeaderPktLenPreCheckQ.enq(tuple2(pktLenCheckInfo, qpcCommon));
        end
        payloadFragMetaPktLenPreCheckQ.enq(streamFragMeta);
        // $display(
        //     "time=%0t: 7th stage calcPktLen", $time,
        //     ", pktLen=%0d, pktFragNum=%0d", pktLen, pktFragNum,
        //     ", isByteEnAllOne=", fshow(isByteEnAllOne),
        //     ", pktValid=", fshow(pktValid),
        //     // ", payloadOutQ.notFull=", fshow(payloadOutQ.notFull),
        //     // ", pktMetaDataOutQ.notFull=", fshow(pktMetaDataOutQ.notFull),
        //     ", DATA_STREAM_FRAG_BUF_SIZE=%0d", valueOf(DATA_STREAM_FRAG_BUF_SIZE),
        //     ", PKT_META_DATA_BUF_SIZE=%0d", valueOf(PKT_META_DATA_BUF_SIZE),
        //     ", streamFragMeta.byteEn=%h" , streamFragMeta.byteEn,
        //     ", streamFragMeta.isFirst=", fshow(streamFragMeta.isFirst),
        //     ", streamFragMeta.isLast=", fshow(streamFragMeta.isLast),
        //     ", bth.psn=%h", bth.psn,
        //     ", bth.opcode=", fshow(bth.opcode),
        //     ", bth.padCnt=%h", bth.padCnt
        // );
    endrule

    rule preCheckPktLen;
        let streamFragMeta = payloadFragMetaPktLenPreCheckQ.first;
        payloadFragMetaPktLenPreCheckQ.deq;

        if (streamFragMeta.isLast) begin
            let { pktLenCheckInfo, qpcCommon } = rdmaHeaderPktLenPreCheckQ.first;
            rdmaHeaderPktLenPreCheckQ.deq;

            let pktLen = pktLenCheckInfo.pktLen;
            let pmtu   = pktLenCheckInfo.pmtu;

            let isZeroPayloadLen = isZeroR(pktLen);
            let isPktLenEqPMTU   = pktLenEqPMTU(pktLen, pmtu);
            let isPktLenGtPMTU   = pktLenGtPMTU(pktLen, pmtu);

            rdmaHeaderPktLenCheckQ.enq(tuple5(
                pktLenCheckInfo, qpcCommon, isZeroPayloadLen, isPktLenEqPMTU, isPktLenGtPMTU
            ));
        end

        payloadFragMetaPktLenCheckQ.enq(streamFragMeta);
        // $display("time=%0t: 8th stage preCheckPktLen", $time);
    endrule

    rule checkPktLen;
        let streamFragMeta = payloadFragMetaPktLenCheckQ.first;
        payloadFragMetaPktLenCheckQ.deq;

        if (streamFragMeta.isLast) begin
            let {
                pktLenCheckInfo, qpcCommon, isZeroPayloadLen, isPktLenEqPMTU, isPktLenGtPMTU
            } = rdmaHeaderPktLenCheckQ.first;
            rdmaHeaderPktLenCheckQ.deq;

            let rdmaHeader      = pktLenCheckInfo.rdmaHeader;
            let pktFragNum      = pktLenCheckInfo.pktFragNum;
            let pktLen          = pktLenCheckInfo.pktLen;
            let pmtu            = pktLenCheckInfo.pmtu;
            let pktValid        = pktLenCheckInfo.pktValid;
            let isFirstOrMidPkt = pktLenCheckInfo.isFirstOrMidPkt;
            let isLastOrOnlyPkt = pktLenCheckInfo.isLastOrOnlyPkt;
            let isMidPkt        = pktLenCheckInfo.isMidPkt;

            // fix byteEN to prevent dma write access touch unrelated bytes.
            streamFragMeta.byteEn = streamFragMeta.byteEn << pktLenCheckInfo.padCnt;

            if (!isZeroPayloadLen) begin
                reqPayloadFragMetaOutQ.enq(streamFragMeta);
                // $display("time=%0t: streamFragMeta=", $time, fshow(streamFragMeta));
            end
            else begin
                // Discard zero length payload no matter packet has payload or not
                $info(
                    "time=%0t: InputRdmaPktBuf checkPktLen", $time,
                    ", discard zero-length payload for RDMA packet"
                );
            end

            if (pktValid) begin
                pktValid = (isMidPkt && isPktLenEqPMTU) ||
                    (isLastOrOnlyPkt && !isPktLenGtPMTU);

                // $display(
                //     "time=%0t: checkPktLen", $time,
                //     ", bth.trans=", fshow(pktLenCheckInfo.trans),
                //     ", bth.dqpn=%h", pktLenCheckInfo.dqpn,
                //     ", bth.psn=%h", pktLenCheckInfo.psn,
                //     ", bth.opcode=", fshow(pktLenCheckInfo.opcode),
                //     ", bth.padCnt=%h", pktLenCheckInfo.padCnt,
                //     ", pktLen=%0d", pktLen,
                //     ", pmtu=", fshow(pmtu),
                //     ", isFirstOrMidPkt=", fshow(isFirstOrMidPkt),
                //     ", isPktLenEqPMTU=", fshow(isPktLenEqPMTU),
                //     ", isLastOrOnlyPkt=", fshow(isLastOrOnlyPkt),
                //     ", isPktLenGtPMTU=", fshow(isPktLenGtPMTU),
                //     ", pktValid=", fshow(pktValid)
                // );
            end

            let pktStatus = PKT_ST_VALID;
            if (!pktValid) begin
                // Invalid packet length
                pktStatus = PKT_ST_LEN_ERR;
            end
            let pktMetaDataAndQpc = DataTypes::RdmaPktMetaDataAndQPC{
                metadata: RdmaPktMetaData {
                    pktPayloadLen   : pktLen,
                    pktFragNum      : (isZeroPayloadLen ? 0 : pktFragNum),
                    isZeroPayloadLen: isZeroPayloadLen,
                    pktHeader       : rdmaHeader,
                    pktValid        : pktValid,
                    pktStatus       : pktStatus
                },
                qpc: qpcCommon
            };

            reqPktMetaDataAndQpcOutQ.enq(pktMetaDataAndQpc);
            $display(
                "time=%0t:", $time, " pktMetaDataAndQpc=", fshow(pktMetaDataAndQpc)
                // "time=%0t: bth=", $time, fshow(bth), ", pktMetaDataAndQpc=", fshow(pktMetaDataAndQpc)
            );
        end
        else begin
            reqPayloadFragMetaOutQ.enq(streamFragMeta);
            // $display("time=%0t: streamFragMeta=", $time, fshow(streamFragMeta));
        end
        // $display("time=%0t: 9th stage checkPktLen", $time);
    endrule





    return interface InputRdmaPktBuf;
        interface reqPktPipeOut = interface RdmaPktMetaDataAndQpcAndPayloadPipeOut;
            interface pktMetaData                       = toPipeOut(reqPktMetaDataAndQpcOutQ);
            interface payloadStreamFragMetaPipeOut      = toPipeOut(reqPayloadFragMetaOutQ);
        endinterface;


        interface qpcReadCommonClt = qpcReadCommonCltInst.clt;
        interface payloadStreamFragMetaPipeIn = toPut(payloadStreamFragMetaPipeInQ);
        
        interface headerDataStreamPipeIn = toPut(headerDataStreamQ);
        interface headerMetaDataPipeIn = toPut(headerMetaDataQ);
        
    endinterface;

endmodule


interface ReceivedStreamFragStorage;
    interface Server#(DATA, InputStreamFragBufferIdx) insertFragSrv;
    interface Server#(Tuple2#(InputStreamFragBufferIdx, Bool), DATA) readFragSrv;
endinterface

(* synthesize *)
module mkReceivedStreamFragStorage(ReceivedStreamFragStorage);
    BypassServer#(DATA, InputStreamFragBufferIdx) insertFragSrvInst                 <- mkBypassServer("insertFragSrvInst");
    BypassServer#(Tuple2#(InputStreamFragBufferIdx, Bool), DATA) readFragSrvInst    <- mkBypassServer("readFragSrvInst");

    BRAM_Configure cfg = defaultValue;
    BRAM2Port#(InputStreamFragBufferIdxWithoutGuard, DATA) bramBuffer <- mkBRAM2Server (cfg);
    Reg#(InputStreamFragBufferIdx) idxGenerator <- mkReg(0);
    Reg#(InputStreamFragBufferIdx) lastConsumeIdx <- mkReg(0);


    rule handleInsertReq;
        let data <- insertFragSrvInst.getReq;
        bramBuffer.portA.request.put(BRAMRequest{
            write: True,
            responseOnWrite:False,
            address: truncate(idxGenerator),
            datain: data
        });

        insertFragSrvInst.putResp(idxGenerator);
        idxGenerator <= idxGenerator + 1;

        // if the substruct result's highest bit is one, the overflow
        immAssert(
            ((idxGenerator - lastConsumeIdx) >> valueOf(INPUT_STREAM_FRAG_BUFFER_INDEX_WITHOUT_GUARD_WIDTH)) == 0,
            "receive data fragment buf overfllow @ mkReceivedStreamFragStorage",
            $format(
                "idxGenerator=", fshow(idxGenerator), " lastConsumeIdx=", fshow(lastConsumeIdx)
            )
        );

        $display(
            "time=%0t:", $time, "rx frag buffer new input packet, idxGenerator=", fshow(idxGenerator),
            " lastConsumeIdx=", fshow(lastConsumeIdx) 
        );
    endrule

    rule handleReadReq;
        let {addr, isOnlyUpadteFragBufLastConsumeIndex} <- readFragSrvInst.getReq;
        lastConsumeIdx <= addr;
        if (!isOnlyUpadteFragBufLastConsumeIndex) begin
            bramBuffer.portB.request.put(BRAMRequest{
                write: False,
                responseOnWrite:False,
                address: truncate(addr),
                datain: ?
            });
        end

        $display(
            "time=%0t:", $time, "rx frag buffer new output packet,  lastConsumeIdx=", 
            fshow(lastConsumeIdx) , " isOnlyUpadteFragBufLastConsumeIndex=", fshow(isOnlyUpadteFragBufLastConsumeIndex)
        );

    endrule

    rule outputReadResp;
        let resp <- bramBuffer.portB.response.get;
        readFragSrvInst.putResp(resp);
    endrule

    interface insertFragSrv = insertFragSrvInst.srv;
    interface readFragSrv = readFragSrvInst.srv;
endmodule
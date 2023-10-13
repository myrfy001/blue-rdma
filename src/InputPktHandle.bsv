import FIFOF :: *;
import PAClib :: *;
import Vector :: *;
import ClientServer :: *;

import DataTypes :: *;
import ExtractAndPrependPipeOut :: *;
import Headers :: *;
import MetaData :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;
import PipeIn :: *;

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
    interface DataStreamPipeOut payload;
    interface DataStreamPipeIn rdmaPktPipeIn;
endinterface

// After extract header from rdmaPktPipeIn,
// it outputs header DataStream and payload DataStream,
// and every header DataStream has corresponding payload DataStream,
// if header has no payload, then output empty payload DataStream.
// This module will not discard invalid packet.
(* synthesize *)
module mkExtractHeaderFromRdmaPktPipeOut(HeaderAndMetaDataAndPayloadSeperateDataStreamPipeOut);
    FIFOF#(HeaderMetaData) headerMetaDataInQ <- mkFIFOF;
    FIFOF#(DataStream) dataInQ <- mkFIFOF;
    FIFOF#(DataStream) rdmaPktPipeInQ <- mkFIFOF;

    Vector#(2, PipeOut#(HeaderMetaData)) headerMetaDataPipeOutVec <-
        mkForkVector(toPipeOut(headerMetaDataInQ));
    let headerMetaDataPipeIn = headerMetaDataPipeOutVec[0];
    let headerMetaDataPipeOut <- mkBuffer(headerMetaDataPipeOutVec[1]);
    let dataPipeIn = toPipeOut(dataInQ);
    let headerAndPayloadPipeOut <- mkExtractHeaderFromDataStreamPipeOut(
        dataPipeIn, headerMetaDataPipeIn
    );

    rule extractHeader;
        let rdmaPktDataStream = rdmaPktPipeInQ.first;
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

            let headerMetaData = genHeaderMetaData(headerLen, headerHasPayload);
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
    interface payload = headerAndPayloadPipeOut.payload;
    interface rdmaPktPipeIn = toPipeIn(rdmaPktPipeInQ);
endmodule

interface InputRdmaPktBuf;
    interface RdmaPktMetaDataAndQpcAndPayloadPipeOut reqPktPipeOut;
    
    interface DataStreamPipeIn headerDataStreamPipeIn;
    interface PipeIn#(HeaderMetaData) headerMetaDataPipeIn;
    interface DataStreamPipeIn payloadPipeIn;

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
module mkInputRdmaPktBufAndHeaderValidation(InputRdmaPktBuf);
    // Output FIFO for PipeOut
    FIFOF#(DataStream)           reqPayloadOutQ <- mkFIFOF;
    FIFOF#(RdmaPktMetaDataAndQPC)  reqPktMetaDataAndQpcOutQ <- mkFIFOF;

    // Pipeline buffers
    FIFOF#(Tuple4#(HeaderRDMA, Bool, Bool, Bool))                                             rdmaHeaderRecvQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                           payloadRecvQ <- mkFIFOF;
    FIFOF#(HeaderRDMA)                                                                    rdmaHeaderPreCheckQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                       payloadPreCheckQ <- mkFIFOF;
    FIFOF#(Tuple2#(HeaderRDMA, HeaderValidateInfo))                                     rdmaHeaderValidationQ <- mkSizedFIFOF(valueOf(QPC_QUERY_RESP_MAX_DELAY));
    FIFOF#(DataStream)                                                                     payloadValidationQ <- mkFIFOF;
    FIFOF#(Tuple3#(HeaderRDMA, Maybe#(EntryCommonQPC), ValidHeaderInfo))                    rdmaHeaderFilterQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                         payloadFilterQ <- mkFIFOF;
    FIFOF#(Tuple3#(HeaderRDMA, EntryCommonQPC, ValidHeaderInfo))                       rdmaHeaderFragLenCalcQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                    payloadFragLenCalcQ <- mkFIFOF;
    FIFOF#(Tuple3#(HeaderRDMA, EntryCommonQPC, ValidHeaderInfo))                        rdmaHeaderPktLenCalcQ <- mkFIFOF;
    FIFOF#(Tuple5#(DataStream, ByteEnBitNum, ByteEnBitNum, Bool, Bool))                    payloadPktLenCalcQ <- mkFIFOF;
    FIFOF#(Tuple2#(PktLenCheckInfo, EntryCommonQPC))                                rdmaHeaderPktLenPreCheckQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                 payloadPktLenPreCheckQ <- mkFIFOF;
    FIFOF#(Tuple5#(PktLenCheckInfo, EntryCommonQPC, Bool, Bool, Bool))                 rdmaHeaderPktLenCheckQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                    payloadPktLenCheckQ <- mkFIFOF;
    FIFOF#(RdmaPktMetaDataAndQPC)                                                         rdmaHeaderOutputQ <- mkFIFOF;
    FIFOF#(DataStream)                                                                         payloadOutputQ <- mkFIFOF;

    Reg#(Bool)        isValidPktReg <- mkRegU;
    Reg#(PAD)          bthPadCntReg <- mkRegU;
    Reg#(PktFragNum)  pktFragNumReg <- mkRegU;
    Reg#(PktLen)          pktLenReg <- mkRegU;
    Reg#(Bool)          pktValidReg <- mkRegU;

    Reg#(RdmaPktBufState) pktBufStateReg <- mkReg(RDMA_PKT_BUT_ST_PRE_CHECK_FRAG);

    FIFOF#(DataStream) payloadPipeInQ <- mkFIFOF;
    FIFOF#(DataStream) headerDataStreamQ <- mkFIFOF;
    let headerMetaDataQ <- mkFIFOF;

    let rdmaHeaderPipeOut <- mkDataStream2Header(
        toPipeOut(headerDataStreamQ),
        toPipeOut(headerMetaDataQ)
    );

    BypassClient#(ReadReqCommonQPC, Maybe#(EntryCommonQPC)) qpcReadCommonCltInst <- mkBypassClient;

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


    (* conflict_free = "recvPktFrag, \
                        preCheckHeader, \
                        discardInvalidFrag, \
                        prepareValidation, \
                        checkMetaDataQP, \
                        discardInvalidHeaderPkt, \
                        calcFraglen, \
                        calcPktLen, \
                        preCheckPktLen, \
                        checkPktLen, \
                        outputPayload, \
                        outputHeaderMetaData" *)
    rule recvPktFrag;
        let payloadFrag = payloadPipeInQ.first;
        payloadPipeInQ.deq;
        let payloadHasSingleFrag = payloadFrag.isFirst && payloadFrag.isLast;
        let fragHasNoData = isZeroByteEn(payloadFrag.byteEn);

        if (payloadFrag.isFirst) begin
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

        payloadRecvQ.enq(payloadFrag);
        // $display(
        //     "time=%0t: 1st stage recvPktFrag", $time
        //     // ", bth=", fshow(bth), ", aeth=", fshow(aeth)
        // );
    endrule

    rule preCheckHeader if (pktBufStateReg == RDMA_PKT_BUT_ST_PRE_CHECK_FRAG);
        let payloadFrag = payloadRecvQ.first;
        payloadRecvQ.deq;

        if (payloadFrag.isFirst) begin
            let {
                rdmaHeader, bthCheckResult, headerCheckResult, nonPayloadHeaderShouldHaveNoPayload
            } = rdmaHeaderRecvQ.first;
            rdmaHeaderRecvQ.deq;

            let bth = extractBTH(rdmaHeader.headerData);
            $display("bthCheckResult=", bthCheckResult, "headerCheckResult=", headerCheckResult, "nonPayloadHeaderShouldHaveNoPayload=", nonPayloadHeaderShouldHaveNoPayload);
            if (bthCheckResult && headerCheckResult && nonPayloadHeaderShouldHaveNoPayload) begin
                // Packet header is valid
                rdmaHeaderPreCheckQ.enq(rdmaHeader);
                payloadPreCheckQ.enq(payloadFrag);

                // $display(
                //     "time=%0t: bth=", $time, fshow(bth),
                //     ", headerMetaData=", fshow(rdmaHeader.headerMetaData),
                //     "\ntime=%0t: payloadFrag=", $time, fshow(payloadFrag)
                // );
            end
            else begin
                if (!payloadFrag.isLast) begin
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
            payloadPreCheckQ.enq(payloadFrag);
            // $display("time=%0t: payloadFrag=", $time, fshow(payloadFrag));
        end
        // $display(
        //     "time=%0t: 2nd-1 stage preCheckHeader", $time
        //     // ", bthCheckResult=", fshow(bthCheckResult),
        //     // ", headerCheckResult=", fshow(headerCheckResult),
        //     // ", nonPayloadHeaderShouldHaveNoPayload=",
        //     // fshow(nonPayloadHeaderShouldHaveNoPayload),
        //     // ", bth=", fshow(bth)
        // );
    endrule

    rule discardInvalidFrag if (pktBufStateReg == RDMA_PKT_BUF_ST_DISCARD_FRAG);
        let payload = payloadRecvQ.first;
        payloadRecvQ.deq;
        if (payload.isLast) begin
            pktBufStateReg <= RDMA_PKT_BUT_ST_PRE_CHECK_FRAG;
        end
        // $display("time=%0t: 2nd-2 stage discardInvalidFrag", $time);
    endrule

    rule prepareValidation;
        let payloadFrag = payloadPreCheckQ.first;
        payloadPreCheckQ.deq;

        if (payloadFrag.isFirst) begin
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
        payloadValidationQ.enq(payloadFrag);
        // $display("time=%0t: 3rd stage prepareValidation", $time);
    endrule

    rule checkMetaDataQP;
        let payloadFrag = payloadValidationQ.first;
        payloadValidationQ.deq;

        if (payloadFrag.isFirst) begin
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

        payloadFilterQ.enq(payloadFrag);
        // $display("time=%0t: 4th stage checkMetaDataQP", $time);
    endrule

    rule discardInvalidHeaderPkt;
        let payloadFrag = payloadFilterQ.first;
        payloadFilterQ.deq;

        let isValidPkt = isValidPktReg;

        if (payloadFrag.isFirst) begin
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
            payloadFragLenCalcQ.enq(payloadFrag);
        end
        $display(
            "time=%0t: 5th stage discardInvalidHeaderPkt", $time,
            ", isValidPkt=", fshow(isValidPkt)
        );
    endrule

    rule calcFraglen;
        let payloadFrag = payloadFragLenCalcQ.first;
        payloadFragLenCalcQ.deq;

        let bthPadCnt = bthPadCntReg;
        if (payloadFrag.isFirst) begin
            let { rdmaHeader, qpcCommon, validHeaderInfo } = rdmaHeaderFragLenCalcQ.first;
            rdmaHeaderFragLenCalcQ.deq;

            let bth       = extractBTH(rdmaHeader.headerData);
            bthPadCnt     = bth.padCnt;
            bthPadCntReg <= bthPadCnt;

            rdmaHeaderPktLenCalcQ.enq(tuple3(rdmaHeader, qpcCommon, validHeaderInfo));

            // $display(
            //     "time=%0t: payloadFrag.byteEn=%h, payloadFrag.isFirst=",
            //     $time, payloadFrag.byteEn, fshow(payloadFrag.isFirst),
            //     ", payloadFrag.isLast=", payloadFrag.isLast, ", bth.psn=%h", bth.psn,
            //     ", bth.opcode=", fshow(bth.opcode), ", bth.padCnt=%h", bth.padCnt,
            //     ", payloadFrag.data=%h", payloadFrag.data
            // );
        end
        
        let payloadFragLen = calcFragByteNumFromByteEn(payloadFrag.byteEn);
        immAssert(
            isValid(payloadFragLen),
            "isValid(payloadFragLen) assertion @ mkInputRdmaPktBufAndHeaderValidation",
            $format(
                "payloadFragLen=", fshow(payloadFragLen), " should be valid"
            )
        );
        let fragLen         = unwrapMaybe(payloadFragLen);
        let isByteEnNonZero = !isZeroByteEn(payloadFrag.byteEn);
        let isByteEnAllOne  = isAllOnesR(payloadFrag.byteEn);
        ByteEnBitNum fragLenWithOutPad = fragLen - zeroExtend(bthPadCnt);

        payloadPktLenCalcQ.enq(tuple5(
            payloadFrag, fragLen, fragLenWithOutPad, isByteEnNonZero, isByteEnAllOne
        ));
        // $display("time=%0t: 6th stage calcFraglen", $time);
    endrule

    rule calcPktLen;
        let {
            payloadFrag, fragLen, fragLenWithOutPad, isByteEnNonZero, isByteEnAllOne
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
        case ({ pack(payloadFrag.isFirst), pack(payloadFrag.isLast) })
            2'b11: begin // payloadFrag.isFirst && payloadFrag.isLast
                pktLen = fragLenExtWithOutPad;
                pktFragNum = 1;
                pktValid = (isFirstOrMidPkt ? False : (isLastPkt ? isByteEnNonZero : True));
            end
            2'b10: begin // payloadFrag.isFirst && !payloadFrag.isLast
                pktLen = fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
                pktFragNum = 1;
                pktValid = isByteEnAllOne;
            end
            2'b01: begin // !payloadFrag.isFirst && payloadFrag.islast
                pktLen = pktLenAddFragLen(pktLenReg, fragLenWithOutPad);
                // pktLen = pktLenReg + fragLenExtWithOutPad;
                pktFragNum = pktFragNumReg + 1;
                pktValid = pktValidReg;
            end
            2'b00: begin // !payloadFrag.isFirst && !payloadFrag.islast
                pktLen = pktLenAddBusByteWidth(pktLenReg);
                // pktLen = pktLenReg + fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
                pktFragNum = pktFragNumReg + 1;
                pktValid = pktValidReg && isByteEnAllOne;
            end
        endcase

        pktLenReg     <= pktLen;
        pktValidReg   <= pktValid;
        pktFragNumReg <= pktFragNum;

        if (payloadFrag.isLast) begin
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
        payloadPktLenPreCheckQ.enq(payloadFrag);
        // $display(
        //     "time=%0t: 7th stage calcPktLen", $time,
        //     ", pktLen=%0d, pktFragNum=%0d", pktLen, pktFragNum,
        //     ", isByteEnAllOne=", fshow(isByteEnAllOne),
        //     ", pktValid=", fshow(pktValid),
        //     // ", payloadOutQ.notFull=", fshow(payloadOutQ.notFull),
        //     // ", pktMetaDataOutQ.notFull=", fshow(pktMetaDataOutQ.notFull),
        //     ", DATA_STREAM_FRAG_BUF_SIZE=%0d", valueOf(DATA_STREAM_FRAG_BUF_SIZE),
        //     ", PKT_META_DATA_BUF_SIZE=%0d", valueOf(PKT_META_DATA_BUF_SIZE),
        //     ", payloadFrag.byteEn=%h" , payloadFrag.byteEn,
        //     ", payloadFrag.isFirst=", fshow(payloadFrag.isFirst),
        //     ", payloadFrag.isLast=", fshow(payloadFrag.isLast),
        //     ", bth.psn=%h", bth.psn,
        //     ", bth.opcode=", fshow(bth.opcode),
        //     ", bth.padCnt=%h", bth.padCnt
        //     // ", payloadFrag.data=%h", payloadFrag.data
        // );
    endrule

    rule preCheckPktLen;
        let payloadFrag = payloadPktLenPreCheckQ.first;
        payloadPktLenPreCheckQ.deq;

        if (payloadFrag.isLast) begin
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

        payloadPktLenCheckQ.enq(payloadFrag);
        // $display("time=%0t: 8th stage preCheckPktLen", $time);
    endrule

    rule checkPktLen;
        let payloadFrag = payloadPktLenCheckQ.first;
        payloadPktLenCheckQ.deq;

        if (payloadFrag.isLast) begin
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
            payloadFrag.byteEn = payloadFrag.byteEn << pktLenCheckInfo.padCnt;

            if (!isZeroPayloadLen) begin
                payloadOutputQ.enq(payloadFrag);
                // $display("time=%0t: payloadFrag=", $time, fshow(payloadFrag));
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

            rdmaHeaderOutputQ.enq(pktMetaDataAndQpc);
            // $display(
            //     "time=%0t:", $time, " pktMetaData=", fshow(pktMetaData)
            //     // "time=%0t: bth=", $time, fshow(bth), ", pktMetaData=", fshow(pktMetaData)
            // );
        end
        else begin
            payloadOutputQ.enq(payloadFrag);
            // $display("time=%0t: payloadFrag=", $time, fshow(payloadFrag));
        end
        // $display("time=%0t: 9th stage checkPktLen", $time);
    endrule

    rule outputPayload;
        let payloadFrag = payloadOutputQ.first;
        payloadOutputQ.deq;
        reqPayloadOutQ.enq(payloadFrag);

        // $display(
        //     "time=%0t: 10th stage outputPayload", $time,
        //     ", qpIndex=%0d, isRespPkt=", qpIndex, fshow(isRespPkt)
        // );
    endrule

    rule outputHeaderMetaData;
        let pktMetaDataAndQpc = rdmaHeaderOutputQ.first;
        rdmaHeaderOutputQ.deq;
        reqPktMetaDataAndQpcOutQ.enq(pktMetaDataAndQpc);

        // $display("time=%0t: final stage outputHeaderMetaData", $time);
    endrule


    return interface InputRdmaPktBuf;
        interface reqPktPipeOut = interface RdmaPktMetaDataAndQpcAndPayloadPipeOut;
            interface pktMetaData = toPipeOut(reqPktMetaDataAndQpcOutQ);
            interface payload     = toPipeOut(reqPayloadOutQ);
        endinterface;


        interface qpcReadCommonClt = qpcReadCommonCltInst.clt;
        interface payloadPipeIn = toPipeIn(payloadPipeInQ);
        
        interface headerDataStreamPipeIn = toPipeIn(headerDataStreamQ);
        interface headerMetaDataPipeIn = toPipeIn(headerMetaDataQ);
        
    endinterface;

endmodule

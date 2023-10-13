import BRAMFIFO :: *;
import ClientServer :: *;
import Connectable :: *;
import FIFOF :: *;
import GetPut :: *;
import PAClib :: *;
import Vector :: *;

import DataTypes :: *;
import Headers :: *;
import PrimUtils :: *;
import RdmaUtils :: *;

interface BramPipe#(type anytype);
    interface PipeOut#(anytype) pipeOut;
    method Action clear();
    method Bool notEmpty();
endinterface

module mkConnectBramQ2PipeOut#(FIFOF#(anytype) bramQ)(
    BramPipe#(anytype)
) provisos(Bits#(anytype, tSz));
    FIFOF#(anytype) postBramQ <- mkFIFOF;

    mkConnection(toPut(postBramQ), toGet(bramQ));

    interface pipeOut = toPipeOut(postBramQ);
    method Action clear();
        postBramQ.clear;
    endmethod
    method Bool notEmpty() = bramQ.notEmpty && postBramQ.notEmpty;
endmodule

module mkConnectPipeOut2BramQ#(
    PipeOut#(anytype) pipeIn, FIFOF#(anytype) bramQ
)(BramPipe#(anytype)) provisos(Bits#(anytype, tSz));
    FIFOF#(anytype) postBramQ <- mkFIFOF;

    mkConnection(toPut(bramQ), toGet(pipeIn));
    mkConnection(toPut(postBramQ), toGet(bramQ));

    interface pipeOut = toPipeOut(postBramQ);
    method Action clear();
        postBramQ.clear;
    endmethod
    method Bool notEmpty() = bramQ.notEmpty && postBramQ.notEmpty;
endmodule

function Bool isDiscardPayload(PayloadConInfo payloadConInfo);
    return case (payloadConInfo) matches
        tagged DiscardPayloadInfo .info: True;
        default                        : False;
    endcase;
endfunction

interface PayloadConsumer;
    interface Server#(PayloadConReq, PayloadConResp) controlPortSrv;
    interface DmaWriteClt dmaWriteClt;
    interface DataStreamPipeIn payloadPipeIn;
endinterface

// Flush DMA write responses when error
module mkPayloadConsumer(PayloadConsumer);

    BypassServer#(PayloadConReq, PayloadConResp) controlPortSrvInst <- mkBypassServer;
    BypassClient#(DmaWriteReqNew, DmaWriteRespNew) dmaWriteCltInst <- mkBypassClient;
    FIFOF#(DataStream) payloadInBufQ <- mkFIFOF;

    // Pipeline FIFO
    FIFOF#(Tuple3#(PayloadConReq, Bool, Bool))        countReqFragQ <- mkFIFOF;
    FIFOF#(Tuple4#(PayloadConReq, Bool, Bool, Bool)) pendingConReqQ <- mkFIFOF;
    FIFOF#(PayloadConReq)                               genConRespQ <- mkFIFOF;
    FIFOF#(Tuple2#(PayloadConReq, DataStream))       pendingDmaReqQ <- mkFIFOF;

    // TODO: check payloadOutQ buffer size is enough for DMA write delay?
    FIFOF#(DataStream) payloadBufQ <- mkSizedBRAMFIFOF(valueOf(DATA_STREAM_FRAG_BUF_SIZE));
    let pipeOut2Bram <- mkConnectPipeOut2BramQ(toPipeOut(payloadInBufQ), payloadBufQ);
    let payloadBufPipeOut = pipeOut2Bram.pipeOut;

    Reg#(PktFragNum) remainingFragNumReg <- mkRegU;
    Reg#(Bool) isRemainingFragNumZeroReg <- mkReg(False);
    Reg#(Bool)      isFirstOrOnlyFragReg <- mkReg(True);


    function Action checkIsOnlyPayloadFragment(
        DataStream payload, PayloadConInfo consumeInfo
    );
        action
            immAssert(
                payload.isFirst && payload.isLast,
                "only payload assertion @ mkPayloadConsumer",
                $format(
                    "payload.isFirst=", fshow(payload.isFirst),
                    "and payload.isLast=", fshow(payload.isLast),
                    " should be true when consumeInfo=",
                    fshow(consumeInfo)
                )
            );
        endaction
    endfunction

    rule recvReq;
        let consumeReq <- controlPortSrvInst.getReq;

        let isDiscardReq = False;
        case (consumeReq.consumeInfo) matches
            tagged DiscardPayloadInfo .discardInfo: begin
                isDiscardReq = True;

                immAssert(
                    !isZero(consumeReq.fragNum),
                    "consumeReq.fragNum assertion @ mkPayloadConsumer",
                    $format(
                        "consumeReq.fragNum=%h should not be zero when consumeInfo is DiscardPayload",
                        consumeReq.fragNum
                    )
                );
            end
            tagged SendWriteReqReadRespInfo .sendWriteReqReadRespInfo    : begin
                immAssert(
                    !isZero(consumeReq.fragNum),
                    "consumeReq.fragNum assertion @ mkPayloadConsumer",
                    $format(
                        "consumeReq.fragNum=%h should not be zero when consumeInfo is SendWriteReqReadRespInfo",
                        consumeReq.fragNum
                    )
                );
            end
            default: begin
                immFail(
                    "unreachible case @ mkPayloadConsumer",
                    $format("consumeReq.consumeInfo=", fshow(consumeReq.consumeInfo))
                );
            end
        endcase

        let isFragNumLessOrEqOne = isLessOrEqOne(consumeReq.fragNum);
        countReqFragQ.enq(tuple3(consumeReq, isFragNumLessOrEqOne, isDiscardReq));
        $display(
            "time=%0t: PayloadConsumer recvReq", $time,
            ", consumeReq=", fshow(consumeReq)
        );
    endrule

    rule countReqFrag;
        let { consumeReq, isFragNumLessOrEqOne, isDiscardReq } = countReqFragQ.first;

        let isLastReqFrag = isFragNumLessOrEqOne ? True : isRemainingFragNumZeroReg;

        if (isDiscardReq) begin
            countReqFragQ.deq;
        end
        else begin
            if (isLastReqFrag) begin
                countReqFragQ.deq;
                isFirstOrOnlyFragReg      <= True;
                isRemainingFragNumZeroReg <= False;
            end
            else begin
                if (isFirstOrOnlyFragReg) begin
                    remainingFragNumReg       <= consumeReq.fragNum - 2;
                    isRemainingFragNumZeroReg <= isTwo(consumeReq.fragNum);
                    isFirstOrOnlyFragReg      <= False;
                end
                else begin
                    remainingFragNumReg       <= remainingFragNumReg - 1;
                    isRemainingFragNumZeroReg <= isOne(remainingFragNumReg);
                end
            end
        end

        pendingConReqQ.enq(tuple4(
            consumeReq, isFragNumLessOrEqOne, isFirstOrOnlyFragReg, isLastReqFrag
        ));
        $display(
            "time=%0t: countReqFrag", $time,
            ", consumeReq.fragNum=%0d", consumeReq.fragNum,
            ", remainingFragNumReg=%0d", remainingFragNumReg,
            ", isRemainingFragNumZeroReg=", fshow(isRemainingFragNumZeroReg),
            ", isFirstOrOnlyFragReg=", fshow(isFirstOrOnlyFragReg),
            ", isLastReqFrag=", fshow(isLastReqFrag)
        );
    endrule

    rule consumePayload;
        let {
            consumeReq, isFragNumLessOrEqOne, isFirstOrOnlyFrag, isLastReqFrag
        } = pendingConReqQ.first;
        let shouldDeqConReq = True;
        // pendingConReqQ.deq;

        case (consumeReq.consumeInfo) matches
            tagged DiscardPayloadInfo .discardInfo: begin
                let payload = payloadBufPipeOut.first;
                payloadBufPipeOut.deq;

                shouldDeqConReq = payload.isLast;
            end
            tagged SendWriteReqReadRespInfo .sendWriteReqReadRespInfo: begin
                let payload = payloadBufPipeOut.first;
                payloadBufPipeOut.deq;
                if (isFragNumLessOrEqOne) begin
                    checkIsOnlyPayloadFragment(payload, consumeReq.consumeInfo);
                    $display(
                        "time=%0t: single packet response consumeReq.fragNum=%0d",
                        $time, consumeReq.fragNum
                    );
                end
                $display(
                    "time=%0t: SendWriteReqReadRespInfo", $time,
                    ", consumeReq=", fshow(consumeReq),
                    ", isFirstOrOnlyFrag=", fshow(isFirstOrOnlyFrag),
                    ", isLastReqFrag=", fshow(isLastReqFrag),
                    ", payload.isFirst=", fshow(payload.isFirst),
                    ", payload.isLast=", fshow(payload.isLast)
                );

                if (isFirstOrOnlyFrag) begin
                    immAssert(
                        payload.isFirst,
                        "first payload assertion @ mkPayloadConsumer",
                        $format(
                            "payload.isFirst=", fshow(payload.isFirst),
                            " should be true when isFirstOrOnlyFrag=", fshow(isFirstOrOnlyFrag),
                            " for consumeReq=", fshow(consumeReq)
                        )
                    );
                end
                else begin
                    immAssert(
                        !payload.isFirst,
                        "first payload assertion @ mkPayloadConsumer",
                        $format(
                            "payload.isFirst=", fshow(payload.isFirst),
                            " should be false when isFirstOrOnlyFrag=", fshow(isFirstOrOnlyFrag),
                            " for consumeReq=", fshow(consumeReq)
                        )
                    );
                end

                if (isLastReqFrag) begin
                    immAssert(
                        payload.isLast,
                        "last payload assertion @ mkPayloadConsumer",
                        $format(
                            "payload.isLast=", fshow(payload.isLast),
                            " should be true when isLastReqFrag=", fshow(isLastReqFrag),
                            " for consumeReq=", fshow(consumeReq)
                        )
                    );

                    genConRespQ.enq(consumeReq);
                end
                else begin
                    immAssert(
                        !payload.isLast,
                        "last payload assertion @ mkPayloadConsumer",
                        $format(
                            "payload.isLast=", fshow(payload.isLast),
                            " should be false when isLastReqFrag=", fshow(isLastReqFrag),
                            " for consumeReq=", fshow(consumeReq)
                        )
                    );
                end

                pendingDmaReqQ.enq(tuple2(consumeReq, payload));
            end
            default: begin
                immFail(
                    "unreachible case @ mkPayloadConsumer",
                    $format("consumeReq.consumeInfo=", fshow(consumeReq.consumeInfo))
                );
            end
        endcase

        if (shouldDeqConReq) begin
            pendingConReqQ.deq;
        end
    endrule

    rule issueDmaReq;
        let { consumeReq, payload } = pendingDmaReqQ.first;
        pendingDmaReqQ.deq;

        case (consumeReq.consumeInfo) matches
            tagged SendWriteReqReadRespInfo .sendWriteReqReadRespInfo: begin
                let dmaWriteReq = DmaWriteReqNew {
                    metaData  : sendWriteReqReadRespInfo,
                    dataStream: payload
                };
                // $display("time=%0t: dmaWriteReq=", $time, fshow(dmaWriteReq));
                dmaWriteCltInst.putReq(dmaWriteReq);
            end
            default: begin
                immAssert(
                    isDiscardPayload(consumeReq.consumeInfo),
                    "isDiscardPayload assertion @ mkPayloadConsumer",
                    $format(
                        "consumeReq.consumeInfo=", fshow(consumeReq.consumeInfo),
                        " should be DiscardPayload"
                    )
                );
            end
        endcase
    endrule

    rule genConResp;
        let dmaWriteResp <- dmaWriteCltInst.getResp;
        let consumeReq = genConRespQ.first;
        genConRespQ.deq;

        case (consumeReq.consumeInfo) matches
            tagged SendWriteReqReadRespInfo .sendWriteReqReadRespInfo: begin
                let consumeResp = PayloadConResp {
                    dmaWriteResp : DmaWriteRespNew {
                        isRespErr: dmaWriteResp.isRespErr
                    }
                };
                controlPortSrvInst.putResp(consumeResp);
            end
            default: begin
                immFail(
                    "unreachible case @ mkPayloadConsumer",
                    $format("consumeReq.consumeInfo=", fshow(consumeReq.consumeInfo))
                );
            end
        endcase
        $display(
            "time=%0t: genConResp", $time,
            ", dmaWriteResp=", fshow(dmaWriteResp),
            ", consumeReq=", fshow(consumeReq)
        );
    endrule

    interface controlPortSrv = controlPortSrvInst.srv;
    interface dmaWriteClt = dmaWriteCltInst.clt;
    interface payloadPipeIn = toPipeIn(payloadInBufQ);
endmodule

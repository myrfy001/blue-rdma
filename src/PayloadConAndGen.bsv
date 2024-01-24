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

// typedef struct {
//     ADDR   startAddr;
//     Length totalLen;
//     PMTU   pmtu;
// } AddrChunkReq deriving(Bits, FShow);

// typedef struct {
//     ADDR   chunkAddr;
//     PktLen chunkLen;
//     Bool   isFirst;
//     Bool   isLast;
// } AddrChunkResp deriving(Bits, FShow);

// // typedef Server#(AddrChunkReq, AddrChunkResp) AddrChunkSrv;
// interface AddrChunkSrv;
//     interface Server#(AddrChunkReq, AddrChunkResp) srvPort;
//     method Bool isIdle();
// endinterface

// module mkAddrChunkSrv#(Bool clearAll, Bool isSQ)(AddrChunkSrv);
//     FIFOF#(AddrChunkReq)   reqQ <- mkFIFOF;
//     FIFOF#(AddrChunkResp) respQ <- mkFIFOF;

//     Reg#(PktLen)   fullPktLenReg <- mkRegU;
//     Reg#(PmtuResidue) residueReg <- mkRegU;
//     Reg#(Bool)  isZeroResidueReg <- mkRegU;

//     Reg#(PktNum)  pktNumReg <- mkRegU;
//     Reg#(ADDR) chunkAddrReg <- mkRegU;
//     Reg#(PMTU)      pmtuReg <- mkRegU;
//     Reg#(Bool)      busyReg <- mkReg(False);
//     Reg#(Bool)   isFirstReg <- mkReg(False);

//     rule resetAndClear if (clearAll);
//         reqQ.clear;
//         respQ.clear;
//         busyReg    <= False;
//         isFirstReg <= False;
//     endrule

//     // rule debug;
//     //     $display(
//     //         "time=%0t: mkAddrChunkSrv debug", $time,
//     //         ", isSQ=", fshow(isSQ),
//     //         ", reqQ.notEmpty=", fshow(reqQ.notEmpty),
//     //         ", respQ.notFull=", fshow(respQ.notFull),
//     //         ", pktNumReg=%0d", pktNumReg,
//     //         ", fullPktLenReg=%h", fullPktLenReg,
//     //         ", busyReg=", fshow(busyReg),
//     //         ", isFirstReg=", fshow(isFirstReg),
//     //         ", clearAll=", fshow(clearAll)
//     //     );
//     // endrule

//     rule recvReq if (!clearAll && !busyReg);
//         let addrChunkReq = reqQ.first;
//         reqQ.deq;

//         immAssert(
//             !isZero(addrChunkReq.totalLen),
//             "totalLen assertion @ mkAddrChunkSrv",
//             $format(
//                 "totalLen=%0d cannot be zero", addrChunkReq.totalLen
//             )
//         );

//         let { tmpPktNum, pmtuResidue } = truncateLenByPMTU(
//             addrChunkReq.totalLen, addrChunkReq.pmtu
//         );
//         let isZeroResidue = isZeroR(pmtuResidue);
//         let totalPktNum = tmpPktNum + (isZeroResidue ? 0 : 1);
//         let pmtuLen = calcPmtuLen(addrChunkReq.pmtu);

//         fullPktLenReg    <= pmtuLen;
//         residueReg       <= pmtuResidue;
//         isZeroResidueReg <= isZeroResidue;
//         pktNumReg        <= totalPktNum;
//         chunkAddrReg     <= addrChunkReq.startAddr;
//         pmtuReg          <= addrChunkReq.pmtu;
//         busyReg          <= True;
//         isFirstReg       <= True;

//         // $display(
//         //     "time=%0t: mkAddrChunkSrv recvReq", $time,
//         //     ", isSQ=", fshow(isSQ),
//         //     ", totalPktNum=%0d", totalPktNum,
//         //     ", pmtuResidue=%h", pmtuResidue,
//         //     ", addrChunkReq=", fshow(addrChunkReq)
//         // );
//     endrule

//     rule genResp if (!clearAll && busyReg);
//         let isLast = isLessOrEqOneR(pktNumReg);
//         let oneAsPSN = 1;

//         busyReg      <= !isLast;
//         pktNumReg    <= pktNumReg - 1;
//         isFirstReg   <= False;
//         chunkAddrReg <= addrAddPsnMultiplyPMTU(chunkAddrReg, oneAsPSN, pmtuReg);

//         let addrChunkResp = AddrChunkResp {
//             chunkAddr: chunkAddrReg,
//             chunkLen : (isLast && !isZeroResidueReg) ? zeroExtend(residueReg) : fullPktLenReg,
//             isFirst  : isFirstReg,
//             isLast   : isLast
//         };
//         respQ.enq(addrChunkResp);

//         // $display(
//         //     "time=%0t: mkAddrChunkSrv genResp", $time,
//         //     ", isSQ=", fshow(isSQ),
//         //     ", pktNumReg=%0d", pktNumReg,
//         //     ", chunkAddrReg=%h", chunkAddrReg,
//         //     ", addrChunkResp=", fshow(addrChunkResp)
//         // );
//     endrule

//     interface srvPort = toGPServer(reqQ, respQ);
//     method Bool isIdle() = !busyReg && !reqQ.notEmpty && !respQ.notEmpty;
// endmodule

// typedef struct {
//     DmaReadMetaDataNew dmaReadMetaData;
//     PMTU pmtu;
// } DmaReadCntrlReq deriving(Bits, FShow);

// typedef struct {
//     DmaReadRespNew dmaReadResp;
//     Bool isOrigFirst;
//     Bool isOrigLast;
// } DmaReadCntrlResp deriving(Bits, FShow);

// typedef Server#(DmaReadCntrlReq, DmaReadCntrlResp) DmaCntrlReadSrv;

// interface DmaCntrl;
//     method Bool isIdle();
//     method Action cancel();
// endinterface

// interface DmaReadCntrl;
//     interface DmaCntrlReadSrv srvPort;
//     interface DmaCntrl dmaCntrl;
// endinterface

// module mkDmaReadCntrl#(
//     // Bool clearAll,
//     CntrlStatus cntrlStatus,
//     DmaReadSrv dmaReadSrv
// )(DmaReadCntrl);
//     FIFOF#(DmaReadCntrlReq)   reqQ <- mkFIFOF;
//     FIFOF#(DmaReadCntrlResp) respQ <- mkFIFOF;

//     FIFOF#(DmaReadCntrlReq)                pendingDmaCntrlReqQ <- mkFIFOF;
//     FIFOF#(Tuple3#(DmaReadReqNew, Bool, Bool)) pendingDmaReadReqQ <- mkFIFOF;

//     let clearAll = cntrlStatus.comm.isReset;
//     let addrChunkSrv <- mkAddrChunkSrv(clearAll, cntrlStatus.isSQ);

//     Reg#(Bool) gracefulStopReg[2] <- mkCReg(2, False);
//     Reg#(Bool)       cancelReg[2] <- mkCReg(2, False);

//     rule resetAndClear if (clearAll);
//         reqQ.clear;
//         respQ.clear;

//         pendingDmaCntrlReqQ.clear;
//         pendingDmaReadReqQ.clear;

//         cancelReg[1]       <= False;
//         gracefulStopReg[1] <= False;
//     endrule

//     // rule debugNotFull if (!(
//     //     reqQ.notFull                &&
//     //     respQ.notFull               &&
//     //     pendingDmaCntrlReqQ.notFull &&
//     //     pendingDmaReadReqQ.notFull
//     //     // firstDmaReqChunkQ.notFull   &&
//     //     // lastDmaReqChunkQ.notFull
//     // ));
//     //     $display(
//     //         "time=%0t: mkDmaReadCntrl debugNotFull", $time,
//     //         ", qpn=%h", cntrlStatus.comm.getSQPN,
//     //         ", isSQ=", fshow(cntrlStatus.isSQ),
//     //         ", reqQ.notEmpty=", fshow(reqQ.notEmpty),
//     //         ", respQ.notFull=", fshow(respQ.notFull),
//     //         ", pendingDmaCntrlReqQ.notFull=", fshow(pendingDmaCntrlReqQ.notFull),
//     //         ", pendingDmaReadReqQ.notFull=", fshow(pendingDmaReadReqQ.notFull)
//     //         // ", firstDmaReqChunkQ.notFull=", fshow(firstDmaReqChunkQ.notFull),
//     //         // ", lastDmaReqChunkQ.notFull=", fshow(lastDmaReqChunkQ.notFull)
//     //     );
//     // endrule

//     // rule debugNotEmpty if (!(
//     //     reqQ.notEmpty &&
//     //     respQ.notEmpty  &&
//     //     pendingDmaCntrlReqQ.notEmpty &&
//     //     pendingDmaReadReqQ.notEmpty
//     //     // firstDmaReqChunkQ.notEmpty &&
//     //     // lastDmaReqChunkQ.notEmpty
//     // ));
//     //     $display(
//     //         "time=%0t: mkDmaReadCntrl debugNotEmpty", $time,
//     //         ", qpn=%h", cntrlStatus.comm.getSQPN,
//     //         ", isSQ=", fshow(cntrlStatus.isSQ),
//     //         ", reqQ.notEmpty=", fshow(reqQ.notEmpty),
//     //         ", respQ.notEmpty=", fshow(respQ.notEmpty),
//     //         ", pendingDmaCntrlReqQ.notEmpty=", fshow(pendingDmaCntrlReqQ.notEmpty),
//     //         ", pendingDmaReadReqQ.notEmpty=", fshow(pendingDmaReadReqQ.notEmpty)
//     //         // ", firstDmaReqChunkQ.notEmpty=", fshow(firstDmaReqChunkQ.notEmpty),
//     //         // ", lastDmaReqChunkQ.notEmpty=", fshow(lastDmaReqChunkQ.notEmpty)
//     //     );
//     // endrule

//     rule recvReq if (!clearAll && !cancelReg[1]);
//         let dmaReadCntrlReq = reqQ.first;
//         reqQ.deq;
//         pendingDmaCntrlReqQ.enq(dmaReadCntrlReq);

//         let addrChunkReq = AddrChunkReq {
//             startAddr: dmaReadCntrlReq.dmaReadMetaData.startAddr,
//             totalLen : dmaReadCntrlReq.dmaReadMetaData.len,
//             pmtu     : dmaReadCntrlReq.pmtu
//         };
//         addrChunkSrv.srvPort.request.put(addrChunkReq);
//         // $display(
//         //     "time=%0t: mkDmaReadCntrl recvReq", $time,
//         //     ", sqpn=%h", cntrlStatus.comm.getSQPN,
//         //     ", isSQ=", fshow(cntrlStatus.isSQ),
//         //     ", dmaReadCntrlReq=", fshow(dmaReadCntrlReq),
//         //     ", addrChunkReq=", fshow(addrChunkReq)
//         // );
//     endrule

//     rule issueDmaReq if (!clearAll && !cancelReg[1]);
//         let addrChunkResp <- addrChunkSrv.srvPort.response.get;

//         let pendingDmaReadCntrlReq = pendingDmaCntrlReqQ.first;

//         // let dmaReadReq       = pendingDmaReadCntrlReq.dmaReadReq;
//         // dmaReadReq.startAddr = addrChunkResp.chunkAddr;
//         // dmaReadReq.len       = zeroExtend(addrChunkResp.chunkLen);
//         let dmaReadMetaData = pendingDmaReadCntrlReq.dmaReadMetaData;
//         let dmaReadReq = DmaReadReqNew {
//             initiator: dmaReadMetaData.initiator,
//             sqpn     : dmaReadMetaData.sqpn,
//             startAddr: addrChunkResp.chunkAddr,
//             len      : addrChunkResp.chunkLen,
//             mrID     : dmaReadMetaData.mrID
//         };

//         dmaReadSrv.request.put(dmaReadReq);

//         let isFirstDmaReqChunk = addrChunkResp.isFirst;
//         let isLastDmaReqChunk = addrChunkResp.isLast;
//         pendingDmaReadReqQ.enq(tuple3(
//             dmaReadReq, isFirstDmaReqChunk, isLastDmaReqChunk
//         ));

//         if (isLastDmaReqChunk) begin
//             pendingDmaCntrlReqQ.deq;
//         end
//         // $display(
//         //     "time=%0t: mkDmaReadCntrl issueDmaReq", $time,
//         //     ", sqpn=%h", cntrlStatus.comm.getSQPN,
//         //     ", isSQ=", fshow(cntrlStatus.isSQ),
//         //     ", pendingDmaReadCntrlReq=", fshow(pendingDmaReadCntrlReq),
//         //     ", addrChunkResp=", fshow(addrChunkResp),
//         //     ", dmaReadReq=", fshow(dmaReadReq)
//         // );
//     endrule

//     rule recvDmaResp if (!clearAll);
//         let dmaResp <- dmaReadSrv.response.get;

//         let { dmaReadReq, isFirstDmaReqChunk, isLastDmaReqChunk } = pendingDmaReadReqQ.first;

//         let isOrigFirst = dmaResp.dataStream.isFirst && isFirstDmaReqChunk;
//         let isOrigLast  = dmaResp.dataStream.isLast && isLastDmaReqChunk;

//         let dmaReadCntrlResp = DmaReadCntrlResp {
//             dmaReadResp: dmaResp,
//             isOrigFirst: isOrigFirst,
//             isOrigLast : isOrigLast
//         };
//         respQ.enq(dmaReadCntrlResp);

//         if (dmaResp.dataStream.isLast) begin
//             pendingDmaReadReqQ.deq;
//         end
//         // $display(
//         //     "time=%0t: mkDmaReadCntrl recvDmaResp", $time,
//         //     ", sqpn=%h", cntrlStatus.comm.getSQPN,
//         //     ", isSQ=", fshow(cntrlStatus.isSQ),
//         //     ", isFirst=", fshow(dmaResp.dataStream.isFirst),
//         //     ", isLast=", fshow(dmaResp.dataStream.isLast),
//         //     ", isFirstDmaReqChunk=", fshow(isFirstDmaReqChunk),
//         //     ", isLastDmaReqChunk=", fshow(isLastDmaReqChunk),
//         //     ", isOrigFirst=", fshow(isOrigFirst),
//         //     ", isOrigLast=", fshow(isOrigLast)
//         // );
//     endrule

//     rule setGracefulStop if (
//         cancelReg[1]                 &&
//         !gracefulStopReg[1]          &&
//         !respQ.notEmpty              &&
//         !pendingDmaReadReqQ.notEmpty &&
//         !clearAll
//     );
//         gracefulStopReg[1] <= True;
//         // $display(
//         //     "time=%0t: mkDmaReadCntrl cancel read done", $time,
//         //     ", sqpn=%h", cntrlStatus.comm.getSQPN,
//         //     ", isSQ=", fshow(cntrlStatus.isSQ)
//         // );
//     endrule

//     interface srvPort = toGPServer(reqQ, respQ);

//     interface dmaCntrl = interface DmaCntrl;
//         method Action cancel(); // if (cntrlStatus.comm.isNonErr || cntrlStatus.comm.isERR);
//             cancelReg[0]       <= True;
//             gracefulStopReg[0] <= False;
//         endmethod

//         method Bool isIdle() = gracefulStopReg[0];
//     endinterface;
// endmodule



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

// As for segmented payload DataStream, each PayloadGenResp is sent
// at the last fragment of the segmented payload DataStream.
module mkPayloadGenerator#(
    CntrlStatus cntrlStatus,
    DmaReadCntrl dmaReadCntrl
)(PayloadGenerator);
    FIFOF#(PayloadGenReq)   payloadGenReqQ <- mkFIFOF;
    FIFOF#(PayloadGenResp) payloadGenRespQ <- mkFIFOF;

    // Pipeline FIFO
    FIFOF#(Tuple3#(PayloadGenReq, ByteEn, PktFragNum)) pendingGenReqQ <- mkFIFOF;
    // FIFOF#(Tuple3#(Bool, Bool, PktFragNum)) pendingGenRespQ <- mkFIFOF;
    // FIFOF#(Tuple3#(DataStream, Bool, Bool)) payloadSegmentQ <- mkFIFOF;

    // TODO: check payloadOutQ buffer size is enough for DMA read delay?
    FIFOF#(DataStream) payloadBufQ <- mkSizedBRAMFIFOF(valueOf(DATA_STREAM_FRAG_BUF_SIZE));
    let bramQ2PipeOut <- mkConnectBramQ2PipeOut(payloadBufQ);
    let payloadBufPipeOut = bramQ2PipeOut.pipeOut;
    // let payloadBufPipeOut <- mkConnectBramQ2PipeOut(payloadBufQ);

    Reg#(PktFragNum) pmtuFragCntReg <- mkRegU;
    Reg#(Bool)    shouldSetFirstReg <- mkReg(False);
    Reg#(Bool)     isFragCntZeroReg <- mkReg(False);
    Reg#(Bool)     isNormalStateReg <- mkReg(True);

    rule resetAndClear if (cntrlStatus.comm.isReset);
        payloadGenReqQ.clear;
        payloadGenRespQ.clear;

        pendingGenReqQ.clear;
        // pendingGenRespQ.clear;
        // payloadSegmentQ.clear;
        payloadBufQ.clear;

        bramQ2PipeOut.clear;

        shouldSetFirstReg <= False;
        isFragCntZeroReg  <= False;
        isNormalStateReg  <= True;

        // $display(
        //     "time=%0t: reset and clear mkPayloadGenerator", $time,
        //     ", pendingGenReqQ.notEmpty=", fshow(pendingGenReqQ.notEmpty)
        // );
    endrule

    // rule debugNotFull if (!(
    //     payloadGenRespQ.notFull &&
    //     pendingGenReqQ.notFull  &&
    //     // pendingGenRespQ.notFull &&
    //     // payloadSegmentQ.notFull &&
    //     payloadBufQ.notFull
    // ));
    //     $display(
    //         "time=%0t: mkPayloadGenerator debugNotFull", $time,
    //         ", qpn=%h", cntrlStatus.comm.getSQPN,
    //         ", isSQ=", fshow(cntrlStatus.isSQ),
    //         ", payloadGenReqQ.notEmpty=", fshow(payloadGenReqQ.notEmpty),
    //         ", payloadGenRespQ.notFull=", fshow(payloadGenRespQ.notFull),
    //         ", pendingGenReqQ.notFull=", fshow(pendingGenReqQ.notFull),
    //         // ", pendingGenRespQ.notFull=", fshow(pendingGenRespQ.notFull),
    //         // ", payloadSegmentQ.notFull=", fshow(payloadSegmentQ.notFull),
    //         ", payloadBufQ.notFull=", fshow(payloadBufQ.notFull)
    //     );
    // endrule

    // rule debugNotEmpty if (!(
    //     payloadGenRespQ.notEmpty &&
    //     pendingGenReqQ.notEmpty  &&
    //     // pendingGenRespQ.notEmpty &&
    //     // payloadSegmentQ.notEmpty &&
    //     payloadBufQ.notEmpty
    // ));
    //     $display(
    //         "time=%0t: mkPayloadGenerator debugNotEmpty", $time,
    //         ", qpn=%h", cntrlStatus.comm.getSQPN,
    //         ", isSQ=", fshow(cntrlStatus.isSQ),
    //         ", payloadGenReqQ.notEmpty=", fshow(payloadGenReqQ.notEmpty),
    //         ", payloadGenRespQ.notEmpty=", fshow(payloadGenRespQ.notEmpty),
    //         ", pendingGenReqQ.notEmpty=", fshow(pendingGenReqQ.notEmpty),
    //         // ", pendingGenRespQ.notEmpty=", fshow(pendingGenRespQ.notEmpty),
    //         // ", payloadSegmentQ.notEmpty=", fshow(payloadSegmentQ.notEmpty),
    //         ", payloadBufQ.notEmpty=", fshow(payloadBufQ.notEmpty)
    //     );
    // endrule

    rule recvPayloadGenReq if (cntrlStatus.comm.isNonErr && isNormalStateReg);
        let payloadGenReq = payloadGenReqQ.first;
        payloadGenReqQ.deq;
        immAssert(
            !isZero(payloadGenReq.dmaReadMetaData.len),
            "payloadGenReq.dmaReadMetaData.len assertion @ mkPayloadGenerator",
            $format(
                "payloadGenReq.dmaReadMetaData.len=%0d should not be zero",
                payloadGenReq.dmaReadMetaData.len
            )
        );

        let totalDmaLen = payloadGenReq.dmaReadMetaData.len;
        let padCnt = calcPadCnt(totalDmaLen);
        let lastFragValidByteNum = calcLastFragValidByteNum(totalDmaLen);
        let lastFragValidByteNumWithPadding = lastFragValidByteNum + zeroExtend(padCnt);
        let lastFragByteEnWithPadding = genByteEn(lastFragValidByteNumWithPadding);
        let pktFragNum = calcFragNumByPMTU(payloadGenReq.pmtu);
        immAssert(
            !isZero(lastFragValidByteNumWithPadding),
            "lastFragValidByteNumWithPadding assertion @ mkPayloadGenerator",
            $format(
                "lastFragValidByteNumWithPadding=%0d should not be zero",
                lastFragValidByteNumWithPadding,
                ", totalDmaLen=%0d, lastFragValidByteNum=%0d, padCnt=%0d",
                totalDmaLen, lastFragValidByteNum, padCnt
            )
        );

        pendingGenReqQ.enq(tuple3(payloadGenReq, lastFragByteEnWithPadding, pktFragNum));

        let dmaReadCntrlReq = DmaReadCntrlReq {
            dmaReadMetaData: payloadGenReq.dmaReadMetaData,
            pmtu           : payloadGenReq.pmtu
        };
        dmaReadCntrl.srvPort.request.put(dmaReadCntrlReq);
        // $display(
        //     "time=%0t: PayloadGenerator recvPayloadGenReq", $time,
        //     ", qpn=%h", cntrlStatus.comm.getSQPN,
        //     ", isSQ=", fshow(cntrlStatus.isSQ),
        //     ", payloadGenReq=", fshow(payloadGenReq),
        //     ", dmaReadCntrlReq=", fshow(dmaReadCntrlReq)
        // );
    endrule

    rule lastFragAddPadding if (cntrlStatus.comm.isNonErr || cntrlStatus.comm.isERR);
        let dmaReadCntrlResp <- dmaReadCntrl.srvPort.response.get;
        let { payloadGenReq, lastFragByteEnWithPadding, pktFragNum } = pendingGenReqQ.first;

        let curData = dmaReadCntrlResp.dmaReadResp.dataStream;
        let isOrigFirstFrag = dmaReadCntrlResp.isOrigFirst;
        let isOrigLastFrag = dmaReadCntrlResp.isOrigLast;

        if (isOrigLastFrag) begin
            pendingGenReqQ.deq;

            if (payloadGenReq.addPadding) begin
                curData.byteEn = lastFragByteEnWithPadding;
            end
        end

        let hasDmaRespErr = dmaReadCntrlResp.dmaReadResp.isRespErr;
        isNormalStateReg <= !hasDmaRespErr;

        // Every segmented payload has a payloadGenResp
        // Must send payloadGenResp when curData.isLast,
        // so as to make sure no DMA response error
        if (curData.isLast || hasDmaRespErr) begin
            let payloadGenResp = PayloadGenResp {
                // initiator  : payloadGenReq.initiator,
                // segment    : payloadGenReq.segment,
                addPadding : payloadGenReq.addPadding,
                isRespErr  : hasDmaRespErr
            };

            payloadGenRespQ.enq(payloadGenResp);
            // $display(
            //     "time=%0t: lastFragAddPadding", $time,
            //     ", payloadGenResp=", fshow(payloadGenResp)
            // );
        end

        payloadBufQ.enq(curData);
        // $display(
        //     "time=%0t: PayloadGenerator lastFragAddPadding", $time,
        //     ", qpn=%h", cntrlStatus.comm.getSQPN,
        //     ", isSQ=", fshow(cntrlStatus.isSQ),
        //     ", payloadGenReq.addPadding=", fshow(payloadGenReq.addPadding),
        //     ", isOrigFirstFrag=", fshow(isOrigFirstFrag),
        //     ", isOrigLastFrag=", fshow(isOrigLastFrag),
        //     ", hasDmaRespErr=", fshow(hasDmaRespErr),
        //     ", dmaReadCntrlResp=", fshow(dmaReadCntrlResp)
        // );
    endrule

    interface srvPort = toGPServer(payloadGenReqQ, payloadGenRespQ);
    interface payloadDataStreamPipeOut = payloadBufPipeOut;
    method Bool payloadNotEmpty() = bramQ2PipeOut.notEmpty;
endmodule

typedef Server#(PayloadConReq, PayloadConResp) PayloadConsumer;

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

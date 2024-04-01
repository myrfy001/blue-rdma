import FIFOF :: *;
import GetPut :: *;
import PAClib :: *;
import Vector :: *;
import ClientServer :: *;

import DataTypes :: *;
import Headers :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;

interface HeaderDataStreamAndMetaDataPipeOut;
    interface DataStreamPipeOut headerDataStream;
    interface PipeOut#(HeaderMetaData) headerMetaData;
endinterface

// If header is empty, then only output headerMetaData,
// and no headerDataStream
module mkHeader2DataStream#(
    Bool clearAll,
    PipeOut#(HeaderRDMA) headerPipeIn
)(HeaderDataStreamAndMetaDataPipeOut);
    FIFOF#(DataStream)   headerDataStreamOutQ <- mkFIFOF;
    FIFOF#(HeaderMetaData) headerMetaDataOutQ <- mkFIFOF;

    Reg#(HeaderRDMA) rdmaHeaderReg <- mkRegU;
    Reg#(Bool)      headerValidReg <- mkReg(False);


    // rule debug;
    //     if (!headerDataStreamOutQ.notFull) begin
    //         $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkHeader2DataStream headerDataStreamOutQ");
    //     end
    //     if (!headerMetaDataOutQ.notFull) begin
    //         $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkHeader2DataStream headerMetaDataOutQ");
    //     end
    // endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule resetAndClear if (clearAll);
        headerDataStreamOutQ.clear;
        headerMetaDataOutQ.clear;

        headerValidReg <= False;
    endrule

    // rule debug; if (!(
    //     headerDataStreamOutQ.notFull && headerMetaDataOutQ.notFull
    // ));
    //     $display(
    //         "time=%0t: mkHeader2DataStream debug", $time,
    //         ", headerDataStreamOutQ.notFull=", fshow(headerDataStreamOutQ.notFull),
    //         ", headerMetaDataOutQ.notFull=", fshow(headerMetaDataOutQ.notFull)
    //     );
    // endrule

    rule outputHeader if (!clearAll);
        let curHeader = headerValidReg ? rdmaHeaderReg : headerPipeIn.first;
        if (!headerValidReg) begin
            headerMetaDataOutQ.enq(curHeader.headerMetaData);
        end
        let remainingHeaderLen =
            curHeader.headerMetaData.headerLen - fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
        let remainingHeaderFragNum = curHeader.headerMetaData.headerFragNum - 1;

        HeaderData leftShiftHeaderData = truncate(curHeader.headerData << valueOf(DATA_BUS_WIDTH));
        HeaderByteEn leftShiftHeaderByteEn =
            truncate(curHeader.headerByteEn << valueOf(DATA_BUS_BYTE_WIDTH));

        let nextHeaderRDMA = HeaderRDMA {
            headerData    : leftShiftHeaderData,
            headerByteEn  : leftShiftHeaderByteEn,
            headerMetaData: HeaderMetaData {
                headerLen           : remainingHeaderLen,
                headerFragNum       : remainingHeaderFragNum,
                lastFragValidByteNum: curHeader.headerMetaData.lastFragValidByteNum,
                hasPayload          : curHeader.headerMetaData.hasPayload,
                isEmptyHeader       : curHeader.headerMetaData.isEmptyHeader
            }
        };

        Bool isFirst = !headerValidReg;
        Bool isLast  = curHeader.headerMetaData.isEmptyHeader || isZero(remainingHeaderFragNum);
        headerValidReg <= !isLast;
        if (isLast) begin
            headerPipeIn.deq;
        end
        else begin
            rdmaHeaderReg <= nextHeaderRDMA;
        end

        let dataStream = DataStream {
            data   : truncateLSB(curHeader.headerData),
            byteEn : truncateLSB(curHeader.headerByteEn),
            isFirst: isFirst,
            isLast : isLast
        };
        // $display(
        //     "time=%0t: dataStream.data=%h, dataStream.byteEn=%h, leftShiftHeaderData=%h, leftShiftHeaderByteEn=%h",
        //     $time, dataStream.data, dataStream.byteEn, leftShiftHeaderData, leftShiftHeaderByteEn
        // );

        // if it is an empty header, also output a beat of header stream to make downstream pipeline easy without blocking.
        // headerDataStreamOutQ.enq(dataStream);

        if (!curHeader.headerMetaData.isEmptyHeader) begin
            headerDataStreamOutQ.enq(dataStream);
        end

        // let bth = extractBTH(curHeader.headerData);
        // $display(
        //     "time=%0t: mkHeader2DataStream outputHeader", $time,
        //     ", start output packet, isEmptyHeader=", fshow(curHeader.headerMetaData.isEmptyHeader),
        //     ", bth.dqpn=%h", bth.dqpn,
        //     ", bth.opcode=", fshow(bth.opcode),
        //     ", bth.psn=%h", bth.psn
        // );
    endrule

    interface headerDataStream = toPipeOut(headerDataStreamOutQ);
    interface headerMetaData   = toPipeOut(headerMetaDataOutQ);
endmodule

// dataPipeIn must have multi-fragment data no more than HeaderByteNum
module mkDataStream2Header#(
    DataStreamPipeOut dataPipeIn, PipeOut#(HeaderMetaData) headerMetaDataPipeIn
)(PipeOut#(HeaderRDMA));
    FIFOF#(HeaderRDMA)                   headerOutQ <- mkFIFOF;
    Reg#(HeaderRDMA)                  rdmaHeaderReg <- mkRegU;
    Reg#(HeaderMetaData)          headerMetaDataReg <- mkRegU;
    Reg#(HeaderByteNum) headerInvalidFragByteNumReg <- mkRegU;
    Reg#(Bool)                              busyReg <- mkReg(False);

    // rule debug if (!(
    //     dataPipeIn.notEmpty           &&
    //     headerMetaDataPipeIn.notEmpty &&
    //     headerOutQ.notFull
    // ));
    //     $display(
    //         "time=%0t: mkDataStream2Header debug", $time,
    //         ", dataPipeIn.notEmpty=", fshow(dataPipeIn.notEmpty),
    //         ", headerMetaDataPipeIn.notEmpty=", fshow(headerMetaDataPipeIn.notEmpty),
    //         ", headerOutQ.notFull=", fshow(headerOutQ.notFull)
    //     );
    // endrule

    rule popHeaderMetaData if (!busyReg);
        busyReg <= True;
        let headerMetaData = headerMetaDataPipeIn.first;
        headerMetaDataPipeIn.deq;
        headerMetaDataReg <= headerMetaData;
        // $display("time=%0t: headerMetaData=", $time, fshow(headerMetaData));

        immAssert(
            !isZero(headerMetaData.headerLen),
            "headerMetaData.headerLen non-zero assertion @ mkDataStream2Header",
            $format(
                "headerMetaData.headerLen=%h should not be zero",
                headerMetaData.headerLen
            )
        );

        let { headerInvalidFragByteNum, headerInvalidFragBitNum } =
            calcHeaderInvalidFragByteAndBitNum(headerMetaData.headerFragNum);
        headerInvalidFragByteNumReg <= headerInvalidFragByteNum;
    endrule

    rule accumulate if (busyReg);
        let curDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;
        // $display(
        //     "time=%0t: curDataStreamFrag.data=%h, curDataStreamFrag.byteEn=%h, headerMetaDataReg=",
        //     $time, curDataStreamFrag.data, curDataStreamFrag.byteEn, fshow(headerMetaDataReg)
        // );

        let rdmaHeader = rdmaHeaderReg;
        let headerFragNum = rdmaHeaderReg.headerMetaData.headerFragNum;
        if (curDataStreamFrag.isFirst) begin
            rdmaHeader.headerData     = zeroExtend(curDataStreamFrag.data);
            rdmaHeader.headerByteEn   = zeroExtend(curDataStreamFrag.byteEn);
            rdmaHeader.headerMetaData = headerMetaDataReg;
            rdmaHeader.headerMetaData.headerFragNum = 1;
        end
        else begin
            rdmaHeader.headerData   = truncate({ rdmaHeader.headerData, curDataStreamFrag.data });
            rdmaHeader.headerByteEn = truncate({ rdmaHeader.headerByteEn, curDataStreamFrag.byteEn });
            rdmaHeader.headerMetaData.headerFragNum = rdmaHeaderReg.headerMetaData.headerFragNum + 1;
        end

        if (curDataStreamFrag.isLast) begin
            rdmaHeader.headerData    = rdmaHeader.headerData << getFragEnBitNumByByteEnNum(truncate(headerInvalidFragByteNumReg));
            rdmaHeader.headerByteEn  = rdmaHeader.headerByteEn << headerInvalidFragByteNumReg;
            let headerLastFragByteEn = genByteEn(rdmaHeader.headerMetaData.lastFragValidByteNum);
            headerOutQ.enq(rdmaHeader);
            busyReg <= False;

            // let { transType, rdmaOpCode } =
            //     extractTranTypeAndRdmaOpCode(rdmaHeader.headerData);
            // $display(
            //     "time=%0t: mkDataStream2Header", $time,
            //     ", rdmaOpCode=", fshow(rdmaOpCode),
            //     ", transType=", fshow(transType),
            //     ", rdmaHeader=", fshow(rdmaHeader)
            // );

            immAssert(
                headerLastFragByteEn == curDataStreamFrag.byteEn,
                "headerLastFragByteEn assertion @ mkDataStream2Header",
                $format(
                    "headerLastFragByteEn=%h should == curDataStreamFrag.byteEn=%h, headerLen=%0d",
                    headerLastFragByteEn, curDataStreamFrag.byteEn, rdmaHeader.headerMetaData.headerLen
                )
            );
            immAssert(
                rdmaHeader.headerMetaData.headerFragNum == headerMetaDataReg.headerFragNum,
                "headerMetaData.headerFragNum assertion @ mkDataStream2Header",
                $format(
                    "rdmaHeader.headerMetaData.headerFragNum=%h should == headerMetaDataReg.headerFragNum=%h when curDataStreamFrag.isLast=%b",
                    rdmaHeader.headerMetaData.headerFragNum, headerMetaDataReg.headerFragNum, curDataStreamFrag.isLast
                )
            );
        end
        else begin
            immAssert(
                isAllOnesR(curDataStreamFrag.byteEn),
                "curDataStreamFrag.byteEn assertion @ mkDataStream2Header",
                $format("curDataStreamFrag.byteEn=%h should be all ones", curDataStreamFrag.byteEn)
            );
        end

        rdmaHeaderReg <= rdmaHeader;
    endrule

    return toPipeOut(headerOutQ);
endmodule

typedef enum {
    STREAM_OUTPUT_IDLE,
    STREAM_OUTPUT_HEADER_OUTPUT,
    STREAM_OUTPUT_DATA_OUTPUT,
    STREAM_OUTPUT_HEADER_DATA_MIX_OUTPUT,
    STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT
} ExtractOrPrependHeaderStage deriving(Bits, Eq, FShow);

// headerPipeIn can not be empty, otherwise deadlock, i.e.:
//   * if isEmptyHeader is true, then must be a dummy beat in headerPipeIn
//   * if headerHasPayload is False, then dataPipeIn should not have beat for this packet
module mkPrependHeader2PipeOut#(
    Bool clearAll,
    DataStreamPipeOut headerPipeIn,
    PipeOut#(HeaderMetaData) headerMetaDataPipeIn,
    DataStreamPipeOut dataPipeIn,
    PipeOut#(PayloadMetaData) payloadMetaDataPipeIn
)(DataStreamPipeOut);
    FIFOF#(DataStream) dataStreamOutQ <- mkFIFOF;


    FIFOF#(Tuple4#(ByteEnBitNum, ByteEnBitNum, Bool, Bool)) calculatedMetasQ <- mkFIFOF;

    FIFOF#(Tuple4#(ByteEnBitNum, ByteEnBitNum, Bool, Bool)) calculatedMetasAfterHeaderRightShiftQ <- mkFIFOF;
    FIFOF#(DataStream) rightShiftedHeaderStreamQ <- mkFIFOF;

    FIFOF#(DataStream) dataStreamQ <- mkFIFOF;
    FIFOF#(Bool) needExtraBeatQ <- mkFIFOF;


    Reg#(Bool) headerStreamFrowardFinishedReg <- mkReg(False);
    Reg#(Bool) dataStreamFrowardFinishedReg <- mkReg(False);


    // preDataStreamReg is right aligned
    Reg#(DataStream)                  preDataStreamReg <- mkRegU;
    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(STREAM_OUTPUT_IDLE);

    Reg#(ByteEnBitNum) headerLastFragValidByteNumReg <- mkRegU;
    Reg#(ByteEnBitNum) headerLastFragInvalidByteNumReg <- mkRegU;
    Reg#(Bool) headerHasPayloadReg <- mkRegU;
    Reg#(Bool) isEmptyHeaderReg <- mkRegU;
    Reg#(Bool) isMixOutputHeaderOutputStageReg <- mkReg(True);

    rule debug;
        if (!dataStreamOutQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkPrependHeader2PipeOut dataStreamOutQ");
        end
        // if (!dataPipeIn.notEmpty) begin
        //     $display("time=%0t: ", $time, "EMPTY_QUEUE_DETECTED: mkPrependHeader2PipeOut dataPipeIn");
        // end
    endrule


    // rule debug if (!dataStreamOutQ.notFull);
    //     $display(
    //         "time=%0t: mkPrependHeader2PipeOut debug", $time,
    //         ", dataStreamOutQ.notFull=", fshow(dataStreamOutQ.notFull)
    //     );
    // endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule resetAndClear if (clearAll);
        dataStreamOutQ.clear;
        stageReg <= STREAM_OUTPUT_IDLE;
        isMixOutputHeaderOutputStageReg <= True;
        headerStreamFrowardFinishedReg <= False;
        dataStreamFrowardFinishedReg <= False;
        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut, resetAndClear", $time,
        //     ", sqpn=%h", cntrlStatus.comm.getSQPN
        // );
    endrule

    rule preCalculateHeaderMetaData if (!clearAll);
        let headerMetaData = headerMetaDataPipeIn.first;

        let headerFragNum = headerMetaData.headerFragNum;
        let headerLastFragValidByteNum = headerMetaData.lastFragValidByteNum;
        let { headerLastFragValidBitNum, headerLastFragInvalidByteNum, headerLastFragInvalidBitNum } =
            calcFragBitNumAndByteNum(headerLastFragValidByteNum);
        if (headerMetaData.isEmptyHeader) begin
            immAssert(
                isZero(headerLastFragValidBitNum) && isZero(headerLastFragValidByteNum),
                "empty header assertion @ mkPrependHeader2PipeOut",
                $format(
                    "headerLastFragValidBitNum=%0d", headerLastFragValidBitNum,
                    " and headerLastFragValidByteNum=%0d", headerLastFragValidByteNum,
                    " should be zero when isEmptyHeader=",
                    fshow(headerMetaData.isEmptyHeader)
                )
            );
        end
        else begin
            immAssert(
                !isZero(headerMetaData.headerLen),
                "headerMetaData.headerLen non-zero assertion @ mkPrependHeader2PipeOut",
                $format(
                    "headerLen=%0d", headerMetaData.headerLen,
                    " should not be zero when isEmptyHeader=",
                    fshow(headerMetaData.isEmptyHeader)
                )
            );
        end

        let isEmptyHeader       = headerMetaData.isEmptyHeader;
        let headerHasPayload    = headerMetaData.hasPayload;
        



        let headerStreamFrowardFinished = headerStreamFrowardFinishedReg;
        let dataStreamFrowardFinished = dataStreamFrowardFinishedReg;

        // For the first beat, pass the metadata to downstream.
        if (!headerStreamFrowardFinished) begin
            if (!isEmptyHeader) begin
                let curHeaderDataStreamFrag = headerPipeIn.first;
                if (curHeaderDataStreamFrag.isFirst) begin
                    calculatedMetasAfterHeaderRightShiftQ.enq(tuple4(headerLastFragValidByteNum,
                        headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader));
                    
                    $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                        "pass header at first beat");
                end
            end
            else begin
                calculatedMetasAfterHeaderRightShiftQ.enq(tuple4(headerLastFragValidByteNum,
                    headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader));
            
                $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                    "pass header at first beat");
            end
        end

        if (!isEmptyHeader) begin
            if (!headerStreamFrowardFinished) begin
                let curHeaderDataStreamFrag = headerPipeIn.first;
                headerPipeIn.deq;

                let rightShiftHeaderLastFragData = curHeaderDataStreamFrag.data >> getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNum));
                let rightShiftHeaderLastFragByteEn = curHeaderDataStreamFrag.byteEn >> headerLastFragInvalidByteNum;
                let outputDataStream = curHeaderDataStreamFrag;

                if (curHeaderDataStreamFrag.isLast) begin
                    headerStreamFrowardFinished = True;
                    if (headerHasPayload) begin
                        outputDataStream.data = rightShiftHeaderLastFragData;
                        outputDataStream.byteEn = rightShiftHeaderLastFragByteEn;
                    end   
                end
                rightShiftedHeaderStreamQ.enq(outputDataStream);
                $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                     ", enqueue header to rightShiftedHeaderStreamQ, data=", fshow(outputDataStream));
            end
        end
        else begin
            headerStreamFrowardFinished = True;
        end

        $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                     ", headerHasPayload=", fshow(headerHasPayload),
                     ", dataStreamFrowardFinished=", fshow(dataStreamFrowardFinished));

        if (headerHasPayload) begin
            if (!dataStreamFrowardFinished) begin
                let dataFrag = dataPipeIn.first;
                dataPipeIn.deq;
                if (dataFrag.isLast) begin
                    dataStreamFrowardFinished = True;
                    ByteEn lastFragByteEn = truncate(dataFrag.byteEn << headerLastFragInvalidByteNum);
                    let noExtraLastFrag = isZeroByteEn(lastFragByteEn);
                    needExtraBeatQ.enq(noExtraLastFrag);
                    $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                     ", enqueue needExtraBeatQ to needExtraBeatQ, data=", fshow(noExtraLastFrag));
                end
                dataStreamQ.enq(dataFrag);
                $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                     ", enqueue header to dataStreamQ, data=", fshow(dataFrag));
            end
        end
        else begin
            dataStreamFrowardFinished = True;
        end



        $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
        ", headerStreamFrowardFinished=", fshow(headerStreamFrowardFinished),
        ", dataStreamFrowardFinished=", fshow(dataStreamFrowardFinished),
        ", headerStreamFrowardFinishedReg=", fshow(headerStreamFrowardFinishedReg),
        ", dataStreamFrowardFinishedReg=", fshow(dataStreamFrowardFinishedReg)
        );

        if (headerStreamFrowardFinished && dataStreamFrowardFinished) begin
            headerStreamFrowardFinishedReg <= False;
            dataStreamFrowardFinishedReg <= False;
            headerMetaDataPipeIn.deq;
            $display("time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
                     ", reset headerStreamFrowardFinishedReg and dataStreamFrowardFinishedReg");
        end
        else begin
            headerStreamFrowardFinishedReg <= headerStreamFrowardFinished;
            dataStreamFrowardFinishedReg <= dataStreamFrowardFinished;
        end

        $display(
            "time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
            ", headerMetaData=", fshow(headerMetaData)
        );

    endrule

    function Action dispatchNextRule();
        action
            let nextState = ?;
            if (calculatedMetasAfterHeaderRightShiftQ.notEmpty) begin
                let {headerLastFragValidByteNum,
                    headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasAfterHeaderRightShiftQ.first;
                calculatedMetasAfterHeaderRightShiftQ.deq;

                headerLastFragValidByteNumReg <= headerLastFragValidByteNum;
                headerLastFragInvalidByteNumReg <= headerLastFragInvalidByteNum;
                headerHasPayloadReg <= headerHasPayload;
                isEmptyHeaderReg <= isEmptyHeader;
                case ({pack(headerHasPayload), pack(isEmptyHeader)}) matches
                    'b00: begin
                        nextState = STREAM_OUTPUT_HEADER_OUTPUT;
                    end
                    'b01: begin
                        immAssert(False, "header and payload both empty @ mkPrependHeader2PipeOut",$format(""));
                    end
                    'b10: begin
                        nextState = STREAM_OUTPUT_HEADER_DATA_MIX_OUTPUT;
                    end
                    'b11: begin
                        nextState = STREAM_OUTPUT_DATA_OUTPUT;
                    end
                endcase
            end
            else begin
                nextState = STREAM_OUTPUT_IDLE;
            end

            stageReg <= nextState;
            if (stageReg != nextState) begin
                $display("time=%0t: mkPrependHeader2PipeOut dispatchNextRule", $time,
                        ", nextState=", fshow(nextState),
                        ", curState=", fshow(stageReg)
                );
            end
        endaction
    endfunction


    rule dispatchHandleRuleByMeta if (!clearAll && stageReg == STREAM_OUTPUT_IDLE);
        dispatchNextRule;
    endrule


    rule outputHeader if (!clearAll && stageReg == STREAM_OUTPUT_HEADER_OUTPUT);
        let headerFrag = rightShiftedHeaderStreamQ.first;
        rightShiftedHeaderStreamQ.deq;
        dataStreamOutQ.enq(headerFrag);
        if (headerFrag.isLast) begin
            dispatchNextRule;
        end

        $display("time=%0t: mkPrependHeader2PipeOut outputHeader", $time,
                     ", output stream=", fshow(headerFrag));
    endrule


    rule outputData if (!clearAll && stageReg == STREAM_OUTPUT_DATA_OUTPUT);
        let dataFrag = dataStreamQ.first;
        dataStreamQ.deq;
        dataStreamOutQ.enq(dataFrag);
        if (dataFrag.isLast) begin
            dispatchNextRule;
        end
        $display("time=%0t: mkPrependHeader2PipeOut outputData", $time,
                     ", output stream=", fshow(dataFrag));
    endrule


    rule outputMixed if (!clearAll && stageReg == STREAM_OUTPUT_HEADER_DATA_MIX_OUTPUT);

        let firstDataStreamFrag = dataStreamQ.first;
        
        preDataStreamReg <= firstDataStreamFrag;
        let fragToOutput = ?;
        if (isMixOutputHeaderOutputStageReg) begin
            let headerFrag = rightShiftedHeaderStreamQ.first;
            rightShiftedHeaderStreamQ.deq;
            if (headerFrag.isLast) begin
                let noExtraLastFrag = needExtraBeatQ.first;
                needExtraBeatQ.deq;
                if (!noExtraLastFrag) begin
                    if (firstDataStreamFrag.isLast) begin
                        stageReg <= STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT;
                    end
                    else begin
                        isMixOutputHeaderOutputStageReg <= False;
                    end
                end
                else begin
                    dispatchNextRule;
                end

                let tmpData = { headerFrag.data, firstDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNumReg));
                let tmpByteEn = { headerFrag.byteEn, firstDataStreamFrag.byteEn } >> headerLastFragValidByteNumReg;

                fragToOutput = DataStream {
                    data: truncate(tmpData),
                    byteEn: truncate(tmpByteEn),
                    isFirst: headerFrag.isFirst,
                    isLast: noExtraLastFrag
                };
                dataStreamQ.deq;
            end
            else begin
                fragToOutput = headerFrag;
            end
        end
        else begin
            let tmpData = { preDataStreamReg.data, firstDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNumReg));
            let tmpByteEn = { preDataStreamReg.byteEn, firstDataStreamFrag.byteEn } >> headerLastFragValidByteNumReg;

            

            fragToOutput = DataStream {
                data: truncate(tmpData),
                byteEn: truncate(tmpByteEn),
                isFirst: False,
                isLast: False
            };
            dataStreamQ.deq;

            if (firstDataStreamFrag.isLast) begin
                isMixOutputHeaderOutputStageReg <= True;
                let noExtraLastFrag = needExtraBeatQ.first;
                needExtraBeatQ.deq;
                fragToOutput.isLast = noExtraLastFrag;
                if (!noExtraLastFrag) begin
                    dispatchNextRule;
                end
                else begin
                    stageReg <= STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT;
                end
            end
        end

        dataStreamOutQ.enq(fragToOutput);
        $display("time=%0t: mkPrependHeader2PipeOut outputMixed", $time,
                     ", output stream=", fshow(fragToOutput));
    endrule

    rule extraLastFrag if (!clearAll && stageReg == STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT);
        
        DATA leftShiftData = preDataStreamReg.data << getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNumReg));
        ByteEn leftShiftByteEn = preDataStreamReg.byteEn << headerLastFragInvalidByteNumReg;
        let extraLastDataStream = DataStream {
            data   : leftShiftData,
            byteEn : leftShiftByteEn,
            isFirst: False,
            isLast : True
        };
        dataStreamOutQ.enq(extraLastDataStream);
        dispatchNextRule;
        $display("time=%0t: mkPrependHeader2PipeOut outputMixed", $time,
                     ", output stream=", fshow(extraLastDataStream));
        
    endrule


    return toPipeOut(dataStreamOutQ);
endmodule

interface HeaderAndPayloadSeperateDataStreamPipeOut;
    interface DataStreamPipeOut header;
    interface DataStreamFragMetaPipeOut payloadStreamFragMetaPipeOut;
    interface Client#(DATA, InputStreamFragBufferIdx) payloadStreamFragStorageInsertClt;
endinterface

// Neither dataPipeIn nor headerMetaDataPipeIn can be empty, headerLen cannot be zero
// dataPipeIn could have data less than requested length from headerMetaDataPipeIn.
module mkExtractHeaderFromDataStreamPipeOut#(
    DataStreamPipeOut dataPipeIn, PipeOut#(HeaderMetaData) headerMetaDataPipeIn
)(HeaderAndPayloadSeperateDataStreamPipeOut);

    BypassClient#(DATA, InputStreamFragBufferIdx) payloadStreamFragStorageInsertCltInst <- mkBypassClient("payloadStreamFragStorageInsertCltInst");
    
    FIFOF#(DataStream) headerDataStreamOutQ                     <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData) payloadDataStreamFragPreOutQ <- mkSizedFIFOF(5);
    FIFOF#(DataStreamFragMetaData) payloadDataStreamFragOutQ    <- mkFIFOF;

    FIFOF#(Tuple4#(ByteEnBitNum, ByteEnBitNum, HeaderMetaData, ByteEn)) calculatedMetasQ <- mkFIFOF;

    Reg#(DataStream)                  preDataStreamReg <- mkRegU;
    Reg#(DataStream)                  curDataStreamReg <- mkRegU;
    Reg#(Bool)                      isFirstDataFragReg <- mkRegU;
    Reg#(HeaderFragNum)           headerFragCounterReg <- mkRegU;

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(STREAM_OUTPUT_HEADER_OUTPUT);
/*
    rule debug if (!(
        headerDataStreamOutQ.notFull && payloadDataStreamFragPreOutQ.notFull
    ));
        $display(
            "time=%0t: mkExtractHeaderFromDataStreamPipeOut debug", $time,
            ", headerDataStreamOutQ.notFull=", fshow(headerDataStreamOutQ.notFull),
            ", payloadDataStreamFragPreOutQ.notFull=", fshow(payloadDataStreamFragPreOutQ.notFull)
        );
    endrule
*/
    rule preCalculateHeaderMetaData;
        let headerMetaData = headerMetaDataPipeIn.first;
        headerMetaDataPipeIn.deq;
        immAssert(
            !isZero(headerMetaData.headerLen),
            "headerMetaData.headerLen non-zero assertion @ mkExtractHeaderFromDataStreamPipeOut",
            $format(
                "headerMetaData.headerLen=%h should not be zero, headerMetaData=",
                headerMetaData.headerLen, fshow(headerMetaData)
            )
        );

        let headerFragNum = headerMetaData.headerFragNum;
        let headerLastFragValidByteNum = headerMetaData.lastFragValidByteNum;
        let { headerLastFragValidBitNum, headerLastFragInvalidByteNum, headerLastFragInvalidBitNum } =
            calcFragBitNumAndByteNum(headerLastFragValidByteNum);
        let headerLastFragByteEn = genByteEn(headerLastFragValidByteNum);


        calculatedMetasQ.enq(tuple4(
            headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerMetaData, headerLastFragByteEn));

        // $display(
        //     "time=%0t:", $time, " headerMetaData=", fshow(headerMetaData),
        //     ", headerLastFragByteEn=%h", reverseBits(headerLastFragByteEn),
        //     ", headerLastFragValidByteNum=%0d", headerLastFragValidByteNum,
        //     ", headerLastFragInvalidByteNum=%0d", headerLastFragInvalidByteNum,
        //     ", stageReg=", fshow(stageReg)
        // );
    endrule

    
    
    // this rule is a big one since it has to handle some different case to achieve fully-pipeline
    // Note: we don't need to care isEmptyHeader, since there is a module already inserted a fake stream beta into the 
    //       original input data stream, which makes the rawPacket stream looks like a WriteOnlyWithImmediate packet.
    //       so we can handle it as a normal packet.
    // 1. if header is not last beat, then simply output Header, and decrease header counter
    // 2. if header is last beat, and no Data, then output Header with right byteEn, output a fake Data beat with
    //    byteEn=0, and move to handle next packet.
    // 3. if header is last beat, and Data also is last beat, then output Header with right byteEn,
    //    and output data as well. then move on to handle next packet.
    // 4. if header is last beat, but Data has extra beat, then only output Header and jump to STREAM_OUTPUT_DATA_OUTPUT
    rule outputHeader if (stageReg == STREAM_OUTPUT_HEADER_OUTPUT);

        let {headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerMetaData, headerLastFragByteEn} = calculatedMetasQ.first;

        let inDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;

        let curHeaderFragCounter = headerFragCounterReg;
        if (inDataStreamFrag.isFirst) begin
            headerFragCounterReg <= headerMetaData.headerFragNum-1;
            curHeaderFragCounter = headerMetaData.headerFragNum;
        end
        else begin
            headerFragCounterReg <= curHeaderFragCounter - 1;
        end

        let byteEnAdjustedFragForLastHeader = inDataStreamFrag;
        byteEnAdjustedFragForLastHeader.byteEn = headerLastFragByteEn;
        byteEnAdjustedFragForLastHeader.isLast = True;

        // This is a tricky one. Since the Header at most will be 2, so the LSB can be used to distinguish 1 or 2.
        // The logic level here is high enough, if we compare the two bits, EDA tool will give warning.
        // But this rule is hard to split into two rules.
        let isHeaderLastBeat = curHeaderFragCounter[0] == 1;

        let leftShiftedPayloadData = inDataStreamFrag.data << getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNum));
        let leftShiftedPayloadByteEn = inDataStreamFrag.byteEn << headerLastFragValidByteNum;

        if (!isHeaderLastBeat) begin // case 1
            immAssert(
                !inDataStreamFrag.isLast,
                "last header beat assertion @ mkExtractHeaderFromDataStreamPipeOut",
                $format("should not be last beat", fshow(inDataStreamFrag))
            );
            headerDataStreamOutQ.enq(inDataStreamFrag);
        end
        else begin  // isHeaderLastBeat=True, include case 2,3,4

            // both case 3,4,5 need output header with adjusted byteEN.
            headerDataStreamOutQ.enq(byteEnAdjustedFragForLastHeader);

            if (!headerMetaData.hasPayload) begin  // case 2
                immAssert(
                    inDataStreamFrag.isLast,
                    "last header beat assertion @ mkExtractHeaderFromDataStreamPipeOut",
                    $format("should be last beat", fshow(inDataStreamFrag))
                );

                // inject zero-sized payload stream, to make following pipeline not deadlock.
                let outDataStreamFragMeta = DataStreamFragMetaData {
                    bufIdx : ?,
                    byteEn : 0,
                    isFirst: True,
                    isLast : True
                };
                payloadDataStreamFragPreOutQ.enq(outDataStreamFragMeta);
                payloadStreamFragStorageInsertCltInst.putReq(leftShiftedPayloadData);

                calculatedMetasQ.deq;  // move on to next packet

            end
            else if (inDataStreamFrag.isLast) begin // case 3, has payload, but all payload is included in this beat
                immAssert(
                    !isZeroR(leftShiftedPayloadByteEn),
                    "last header beat assertion @ mkExtractHeaderFromDataStreamPipeOut",
                    $format("should be last beat", fshow(inDataStreamFrag))
                );
                let outDataStreamFragMeta = DataStreamFragMetaData {
                    bufIdx : ?,
                    byteEn : leftShiftedPayloadByteEn,
                    isFirst: True,
                    isLast : True
                };
                payloadDataStreamFragPreOutQ.enq(outDataStreamFragMeta);
                payloadStreamFragStorageInsertCltInst.putReq(leftShiftedPayloadData);
                calculatedMetasQ.deq;  // move on to next packet
            end
            else begin
                stageReg <= STREAM_OUTPUT_DATA_OUTPUT;
                preDataStreamReg <= inDataStreamFrag;
                isFirstDataFragReg <= True;
            end
        end
    endrule

    rule outputData if (stageReg == STREAM_OUTPUT_DATA_OUTPUT);

        let {headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerMetaData, headerLastFragByteEn} = calculatedMetasQ.first;

        let curDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;
        preDataStreamReg   <= curDataStreamFrag;

        isFirstDataFragReg <= False;
        let shiftedCurDataFragByteEn = curDataStreamFrag.byteEn << headerLastFragValidByteNum;
        let noExtraLastFrag = isZeroByteEn(shiftedCurDataFragByteEn);
        

        let outData   = { preDataStreamReg.data, curDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNum));
        let outByteEn = { preDataStreamReg.byteEn, curDataStreamFrag.byteEn } >> headerLastFragInvalidByteNum;
        let isLast = curDataStreamFrag.isLast && noExtraLastFrag;
        let outDataStream = DataStreamFragMetaData {
            bufIdx : ?,
            byteEn : truncate(outByteEn),
            isFirst: isFirstDataFragReg,
            isLast : isLast
        };
        // $display(
        //     "time=%0t:", $time,
        //     " extract headerLastFragValidByteNumReg=%0d", headerLastFragValidByteNumReg,
        //     ", headerLastFragInvalidByteNumReg=%0d", headerLastFragInvalidByteNumReg,
        //     ", noExtraLastFrag=", fshow(noExtraLastFrag),
        //     ", preDataStreamReg=", fshow(preDataStreamReg),
        //     ", curDataStreamFrag=", fshow(curDataStreamFrag),
        //     ", outDataStream=", fshow(outDataStream),
        //     ", stageReg=", fshow(stageReg)
        // );
        
        payloadDataStreamFragPreOutQ.enq(outDataStream);
        payloadStreamFragStorageInsertCltInst.putReq(truncate(outData));

        if (curDataStreamFrag.isLast) begin
            if (noExtraLastFrag) begin
                stageReg <= STREAM_OUTPUT_HEADER_OUTPUT;
                calculatedMetasQ.deq;  // move on to next packet
            end
            else begin
                stageReg <= STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT;
            end
        end
    endrule

    rule extraLastFrag if (stageReg == STREAM_OUTPUT_EXTRA_LAST_FRAG_OUTPUT);
        let {headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerMetaData, headerLastFragByteEn} = calculatedMetasQ.first;

        DATA leftShiftData      = truncate(preDataStreamReg.data << getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNum)));
        ByteEn leftShiftByteEn  = truncate(preDataStreamReg.byteEn << headerLastFragValidByteNum);
        let extraLastDataStream = DataStreamFragMetaData {
            bufIdx : ?,
            byteEn: leftShiftByteEn,
            isFirst: False,
            isLast: True
        };

        // $display("time=%0t: extraLastDataStream=", $time, fshow(extraLastDataStream));
        payloadDataStreamFragPreOutQ.enq(extraLastDataStream);
        payloadStreamFragStorageInsertCltInst.putReq(leftShiftData);

        stageReg <= STREAM_OUTPUT_HEADER_OUTPUT;
        calculatedMetasQ.deq;  // move on to next packet
    endrule

    rule outputPayloadStreamFragMeta;
        // the goal of this rule is assign bufIdx to fragMeta

        let fragMeta = payloadDataStreamFragPreOutQ.first;
        payloadDataStreamFragPreOutQ.deq;

        let bufIdx <- payloadStreamFragStorageInsertCltInst.getResp;
        fragMeta.bufIdx = bufIdx;
        payloadDataStreamFragOutQ.enq(fragMeta);
    endrule

    interface header = toPipeOut(headerDataStreamOutQ);
    interface payloadStreamFragMetaPipeOut = toPipeOut(payloadDataStreamFragOutQ);
    interface payloadStreamFragStorageInsertClt = payloadStreamFragStorageInsertCltInst.clt;
endmodule


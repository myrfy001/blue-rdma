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
    Reg#(HeaderBitNum)   headerInvalidFragBitNumReg <- mkRegU;
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
        headerInvalidFragBitNumReg  <= headerInvalidFragBitNum;
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
            rdmaHeader.headerData    = rdmaHeader.headerData << headerInvalidFragBitNumReg;
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
    IDLE_WAITING_DECIDE_NEXT,
    ONLY_HEADER_OUTPUT,
    HEADER_MIX_DATA_OUTPUT,
    ONLY_DATA_OUTPUT,
    EXTRA_LAST_FRAG_OUTPUT
} ExtractOrPrependHeaderStage deriving(Bits, Eq, FShow);

// Neither headerPipeIn nor dataPipeIn can be empty, otherwise deadlock.
// headerLen cannot be zero, but dataPipeIn can have empty DataStream.
// If header has no payload, then it will not dequeue dataPipeIn.
module mkPrependHeader2PipeOut#(
    Bool clearAll,
    DataStreamPipeOut headerPipeIn,
    PipeOut#(HeaderMetaData) headerMetaDataPipeIn,
    DataStreamPipeOut dataPipeIn
)(DataStreamPipeOut);
    FIFOF#(DataStream) dataStreamOutQ <- mkFIFOF;


    FIFOF#(Tuple4#(ByteEnBitNum, ByteEnBitNum, Bool, Bool)) calculatedMetasQ <- mkFIFOF;

    FIFOF#(Tuple4#(ByteEnBitNum, ByteEnBitNum, Bool, Bool)) calculatedMetasAfterHeaderRightShiftQ <- mkFIFOF;
    FIFOF#(DataStream) rightShiftedHeaderStreamQ <- mkFIFOF;

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(IDLE_WAITING_DECIDE_NEXT);

    rule debug;
        if (!dataStreamOutQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkPrependHeader2PipeOut dataStreamOutQ");
        end
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
        stageReg <= IDLE_WAITING_DECIDE_NEXT;

        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut, resetAndClear", $time,
        //     ", sqpn=%h", cntrlStatus.comm.getSQPN
        // );
    endrule

    rule preCalculateHeaderMetaData if (!clearAll);
        let headerMetaData = headerMetaDataPipeIn.first;
        headerMetaDataPipeIn.deq;
        

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


        let headerFragCnt       = headerMetaData.isEmptyHeader ? 0 : (headerFragNum - 1);
        let isEmptyHeader       = headerMetaData.isEmptyHeader;
        let headerHasPayload    = headerMetaData.hasPayload;
        calculatedMetasQ.enq(tuple4(
            headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader));

        $display(
            "time=%0t: mkPrependHeader2PipeOut preCalculateHeaderMetaData", $time,
            ", headerMetaData=", fshow(headerMetaData), "headerMetaData.isEmptyHeader=", fshow(headerMetaData.isEmptyHeader)
        );

    endrule


    rule preShiftLastHeaderBeat if (!clearAll);

        let {headerLastFragValidByteNum, headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasQ.first;
        
        $display(
            "time=%0t: mkPrependHeader2PipeOut preShiftLastHeaderBeat", $time,
            ", calculatedMetasQ.first=", fshow(calculatedMetasQ.first)
        );

        if (!isEmptyHeader && headerPipeIn.notEmpty) begin
            let curHeaderDataStreamFrag = headerPipeIn.first;
            headerPipeIn.deq;


            let rightShiftHeaderLastFragData = curHeaderDataStreamFrag.data >> getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNum));
            let rightShiftHeaderLastFragByteEn = curHeaderDataStreamFrag.byteEn >> headerLastFragInvalidByteNum;
            let outputDataStream = curHeaderDataStreamFrag;

            // For the first beat, pass the metadata to downstream.
            if (curHeaderDataStreamFrag.isFirst) begin
                calculatedMetasAfterHeaderRightShiftQ.enq(tuple4(headerLastFragValidByteNum,
                    headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader));
            end

            if (curHeaderDataStreamFrag.isLast && headerHasPayload) begin
                outputDataStream.data = rightShiftHeaderLastFragData;
                outputDataStream.byteEn = rightShiftHeaderLastFragByteEn;
                calculatedMetasQ.deq;
            end
            rightShiftedHeaderStreamQ.enq(outputDataStream);
        end
        else begin
            // No header datestreams, but to make the pipeline flow, we need to enq something.
            calculatedMetasAfterHeaderRightShiftQ.enq(tuple4(headerLastFragValidByteNum,
                headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader));
            calculatedMetasQ.deq;
        end

    endrule


    Reg#(DataStream) prevHeaderBeatReg <- mkRegU;
    Reg#(DataStream) prevDataBeatReg <- mkRegU;

    Reg#(ByteEnBitNum) headerLastFragValidByteNumReg <- mkRegU;
    Reg#(ByteEnBitNum) headerLastFragInvalidByteNumReg <- mkRegU;
    Reg#(Bool) headerHasPayloadReg <- mkRegU;
    Reg#(Bool) isEmptyHeaderReg <- mkRegU;



    

    function ActionValue#(DataStream) moveDataFragToRegAndCheckBuble(String debugName);
        actionvalue
            if (dataPipeIn.notEmpty) begin
                let dataStreamFirstBeat = dataPipeIn.first;
                dataPipeIn.deq;
                prevDataBeatReg <= dataStreamFirstBeat;
                $display(
                    "time=%0t: mkPrependHeader2PipeOut moveDataFragToRegAndCheckBuble", $time,
                    ", debugName=", fshow(debugName),
                    ", nextBeatNewData=", fshow(dataStreamFirstBeat)
                );
                return dataStreamFirstBeat;
            end
            else begin
                immAssert(
                    False,
                    "Should not reach here, buble in pipeline! @ mkTestCrcAxiStream",
                    $format("[%s] Should not reach here, buble in pipeline!", debugName)
                );
                return ?;
            end
        endactionvalue
    endfunction
    

    function Action prepareAndDecideNextStateForNewPacket();
        action
            let {headerLastFragValidByteNum,
                 headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasAfterHeaderRightShiftQ.first;
            calculatedMetasAfterHeaderRightShiftQ.deq;

            headerLastFragValidByteNumReg <= headerLastFragValidByteNum;
            headerLastFragInvalidByteNumReg <= headerLastFragInvalidByteNum;
            headerHasPayloadReg <= headerHasPayload;
            isEmptyHeaderReg <= isEmptyHeader;


            if (isEmptyHeader && dataPipeIn.notEmpty) begin 
                let _ <- moveDataFragToRegAndCheckBuble("1");
                stageReg <= ONLY_DATA_OUTPUT;
                $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_DATA_OUTPUT", $time);
            end
            else if (rightShiftedHeaderStreamQ.notEmpty) begin
                let headerStreamFirstBeat = rightShiftedHeaderStreamQ.first;
                rightShiftedHeaderStreamQ.deq;
                prevHeaderBeatReg <= headerStreamFirstBeat;
                if (!headerStreamFirstBeat.isLast) begin
                    stageReg <= ONLY_HEADER_OUTPUT;
                    $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_HEADER_OUTPUT", $time);
                end
                else if (headerHasPayload) begin
                    if (headerLastFragInvalidByteNum == 0) begin
                        stageReg <= ONLY_HEADER_OUTPUT;
                        $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_HEADER_OUTPUT", $time);
                    end 
                    else begin
                        stageReg <= HEADER_MIX_DATA_OUTPUT;
                        $display("time=%0t: mkPrependHeader2PipeOut nextState=HEADER_MIX_DATA_OUTPUT", $time);
                        let _ <- moveDataFragToRegAndCheckBuble("2");
                    end
                end
                else begin
                    stageReg <= ONLY_HEADER_OUTPUT;
                    $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_HEADER_OUTPUT", $time);
                end
            end
        endaction 
    endfunction


    function Action decideNextNewPacketOrGoToIdle;
        action
            if (calculatedMetasAfterHeaderRightShiftQ.notEmpty) begin
                prepareAndDecideNextStateForNewPacket;
            end
            else begin
                stageReg <= IDLE_WAITING_DECIDE_NEXT;
                $display("time=%0t: mkPrependHeader2PipeOut nextState=IDLE_WAITING_DECIDE_NEXT", $time);
            end
        endaction
    endfunction

    rule decideNextStepForNewPacketWhenIdle if (!clearAll && stageReg == IDLE_WAITING_DECIDE_NEXT);
        prepareAndDecideNextStateForNewPacket;
        $display(
            "time=%0t: mkPrependHeader2PipeOut decideNextStepForNewPacketWhenIdle", $time
        );
    endrule


    rule outputOnlyHeaderFrag if (!clearAll && stageReg == ONLY_HEADER_OUTPUT);
        let prevHeaderBeat = prevHeaderBeatReg;
        if (prevHeaderBeat.isLast) begin
            if (!headerHasPayloadReg) begin
                decideNextNewPacketOrGoToIdle;
            end
            else begin
                // since need to concat payload after it, mark it not the last frag.
                prevHeaderBeat.isLast = False;
                stageReg <= ONLY_DATA_OUTPUT;
                $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_DATA_OUTPUT", $time);
                let _ <- moveDataFragToRegAndCheckBuble("3");
            end
        end
        else begin
            if (rightShiftedHeaderStreamQ.notEmpty) begin
                let headerStreamCurBeat = rightShiftedHeaderStreamQ.first;
                rightShiftedHeaderStreamQ.deq;

                prevHeaderBeatReg <= headerStreamCurBeat;

                if (headerLastFragInvalidByteNumReg == 0) begin
                    stageReg <= ONLY_DATA_OUTPUT;
                    $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_DATA_OUTPUT", $time);
                    let _ <- moveDataFragToRegAndCheckBuble("4");
                end
                else begin
                    stageReg <= HEADER_MIX_DATA_OUTPUT;
                    $display("time=%0t: mkPrependHeader2PipeOut nextState=HEADER_MIX_DATA_OUTPUT", $time);
                    let _ <- moveDataFragToRegAndCheckBuble("5");
                end
            end
            else begin
                immAssert(
                    False,
                    "Should not reach here, buble in pipeline! @ outputOnlyHeaderFrag ",
                    $format("Should not reach here, buble in pipeline!")
                );
            end
        end

        dataStreamOutQ.enq(prevHeaderBeat);
        $display(
            "time=%0t: mkPrependHeader2PipeOut outputOnlyHeaderFrag ", $time,
            "prevHeaderBeat=", fshow(prevHeaderBeat)
        );
    endrule

    rule outputHeaderMixDataFrag if (!clearAll && stageReg == HEADER_MIX_DATA_OUTPUT);

        let prevDataBeat = prevDataBeatReg;

        let tmpData = { prevHeaderBeatReg.data, prevDataBeat.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNumReg));
        let tmpByteEn = { prevHeaderBeatReg.byteEn, prevDataBeat.byteEn } >> headerLastFragValidByteNumReg;

        ByteEn lastFragByteEn = prevDataBeat.byteEn << headerLastFragInvalidByteNumReg;
        let noExtraLastFrag = isZeroByteEn(lastFragByteEn);

        if (noExtraLastFrag) begin
            decideNextNewPacketOrGoToIdle;
        end
        else begin
            if (prevDataBeat.isLast) begin
                stageReg <= EXTRA_LAST_FRAG_OUTPUT;
                $display("time=%0t: mkPrependHeader2PipeOut nextState=EXTRA_LAST_FRAG_OUTPUT", $time);
            end
            else begin
                stageReg <= ONLY_DATA_OUTPUT;
                $display("time=%0t: mkPrependHeader2PipeOut nextState=ONLY_DATA_OUTPUT", $time);
            end
        end

        let headerLastFragDataStreamMergedWithPayload = DataStream {
            data: truncate(tmpData),
            byteEn: truncate(tmpByteEn),
            isFirst: prevHeaderBeatReg.isFirst,
            isLast: noExtraLastFrag
        };
        dataStreamOutQ.enq(headerLastFragDataStreamMergedWithPayload);
        $display(
            "time=%0t: mkPrependHeader2PipeOut outputHeaderMixDataFrag ", $time,
            "headerLastFragDataStreamMergedWithPayload=", fshow(headerLastFragDataStreamMergedWithPayload)
        );
    endrule

    rule outputOnlyDataFrag if (!clearAll && stageReg == ONLY_DATA_OUTPUT);

        let prevDataBeat = prevDataBeatReg;

        // TODO: Can we try to set EmptyHeader's metadata's headerLastFragValidByteNum to 32?
        // If its ok, then we can merge the following if branch into one ?
        if (isEmptyHeaderReg) begin
            if (prevDataBeat.isLast) begin
                decideNextNewPacketOrGoToIdle;
            end
            else begin
                let _ <- moveDataFragToRegAndCheckBuble("7");
            end
            dataStreamOutQ.enq(prevDataBeat);
            $display(
                "time=%0t: mkPrependHeader2PipeOut outputOnlyDataFrag ", $time,
                "prevDataBeat=", fshow(prevDataBeat)
            );
        end
        else begin

            let curDataStreamFrag = dataPipeIn.first;

            let tmpData = { prevDataBeat.data, curDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNumReg));
            let tmpByteEn = { prevDataBeat.byteEn, curDataStreamFrag.byteEn } >> headerLastFragValidByteNumReg;

            ByteEn lastFragByteEn = prevDataBeat.byteEn << headerLastFragInvalidByteNumReg;
            let noExtraLastFrag = isZeroByteEn(lastFragByteEn);

            if (curDataStreamFrag.isLast) begin
                if (noExtraLastFrag) begin
                    decideNextNewPacketOrGoToIdle;
                end
                else begin
                    stageReg <= EXTRA_LAST_FRAG_OUTPUT;
                    
                    prevDataBeatReg <= curDataStreamFrag;
                    dataPipeIn.deq;
                    
                    $display(
                        "time=%0t: mkPrependHeader2PipeOut nextState=EXTRA_LAST_FRAG_OUTPUT ", $time, 
                        ", prevDataBeat.data=", fshow(prevDataBeat.data),
                        ", prevDataBeat.byteEn=", fshow(prevDataBeat.byteEn),
                        ", curDataStreamFrag.data=", fshow(curDataStreamFrag.data),
                        ", curDataStreamFrag.byteEn=", fshow(curDataStreamFrag.byteEn)
                    );
                end
            end
            else begin
                let _ <- moveDataFragToRegAndCheckBuble("8");
            end

            let dataStreamFrag = DataStream {
                data: truncate(tmpData),
                byteEn: truncate(tmpByteEn),
                isFirst: False,
                isLast: noExtraLastFrag
            };
            dataStreamOutQ.enq(dataStreamFrag);

            $display(
                "time=%0t: mkPrependHeader2PipeOut outputOnlyDataFrag ", $time,
                "dataStreamFrag=", fshow(dataStreamFrag)
            );
        end
    endrule

    rule outputExtraLastDataFrag if (!clearAll && stageReg == EXTRA_LAST_FRAG_OUTPUT);
        DATA leftShiftData = prevDataBeatReg.data << getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNumReg));
        ByteEn leftShiftByteEn = prevDataBeatReg.byteEn << headerLastFragInvalidByteNumReg;
        let extraLastDataStream = DataStream {
            data   : leftShiftData,
            byteEn : leftShiftByteEn,
            isFirst: False,
            isLast : True
        };

        decideNextNewPacketOrGoToIdle;
        dataStreamOutQ.enq(extraLastDataStream);

        $display(
            "time=%0t: mkPrependHeader2PipeOut outputExtraLastDataFrag ", $time,
            "extraLastDataStream=", fshow(extraLastDataStream)
        );
        
    endrule






    // this rule is a big one since it has to handle 4 different case to achieve fully-pipeline
    // 1. isEmptyHeader = True, in this case, it should skip header output and directly output first beat of datastream,
    //    and decide whether goto ONLY_DATA_OUTPUT stage.
    // 2. isEmptyHeader = False, and header.isLast=False, in this case, simply output the current head fragement,
    //    no merge is required, and keep in current stage.
    // 3. isEmptyHeader = False, header.isLast=True, and has no payload, in this case, simply output the current head fragement,
    //    no merge is required, and keep in  current stage.
    // 4. isEmptyHeader = False, header.isLast=True, and has payload, in this case, we should merge head and data,
    //    and decide whether goto ONLY_DATA_OUTPUT stage.

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

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(ONLY_HEADER_OUTPUT);
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
    // 4. if header is last beat, but Data has extra beat, then only output Header and jump to ONLY_DATA_OUTPUT
    rule outputHeader if (stageReg == ONLY_HEADER_OUTPUT);

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
                stageReg <= ONLY_DATA_OUTPUT;
                preDataStreamReg <= inDataStreamFrag;
                isFirstDataFragReg <= True;
            end
        end
    endrule

    rule outputData if (stageReg == ONLY_DATA_OUTPUT);

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
                stageReg <= ONLY_HEADER_OUTPUT;
                calculatedMetasQ.deq;  // move on to next packet
            end
            else begin
                stageReg <= EXTRA_LAST_FRAG_OUTPUT;
            end
        end
    endrule

    rule extraLastFrag if (stageReg == EXTRA_LAST_FRAG_OUTPUT);
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

        stageReg <= ONLY_HEADER_OUTPUT;
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


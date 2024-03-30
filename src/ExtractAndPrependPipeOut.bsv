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
        headerDataStreamOutQ.enq(dataStream);

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
    HEADER_OUTPUT,
    DATA_OUTPUT,
    EXTRA_LAST_FRAG_OUTPUT
} ExtractOrPrependHeaderStage deriving(Bits, Eq, FShow);

// headerPipeIn can not be empty, otherwise deadlock, i.e.:
//   * if isEmptyHeader is true, then must be a dummy beat in headerPipeIn
//   * if headerHasPayload is False, then dataPipeIn should not have beat for this packet
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

    // preDataStreamReg is right aligned
    Reg#(DataStream)                  preDataStreamReg <- mkRegU;
    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(HEADER_OUTPUT);

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
        stageReg <= HEADER_OUTPUT;

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

        // $display("time=%0t:", $time, " before shift header=", fshow(curHeaderDataStreamFrag));

        if (curHeaderDataStreamFrag.isLast) begin
            calculatedMetasQ.deq;
            if (headerHasPayload) begin
                outputDataStream.data = rightShiftHeaderLastFragData;
                outputDataStream.byteEn = rightShiftHeaderLastFragByteEn;
            end     
        end
        // $display("time=%0t:", $time, " after shift header=", fshow(curHeaderDataStreamFrag));
        rightShiftedHeaderStreamQ.enq(outputDataStream);
    endrule

    // this rule is a big one since it has to handle 4 different case to achieve fully-pipeline
    // 1. isEmptyHeader = True, in this case, it should skip header output and directly output first beat of datastream,
    //    and decide whether goto DATA_OUTPUT stage.
    // 2. isEmptyHeader = False, and header.isLast=False, in this case, simply output the current head fragement,
    //    no merge is required, and keep in current stage.
    // 3. isEmptyHeader = False, header.isLast=True, and has no payload, in this case, simply output the current head fragement,
    //    no merge is required, and keep in  current stage.
    // 4. isEmptyHeader = False, header.isLast=True, and has payload, in this case, we should merge head and data,
    //    and decide whether goto DATA_OUTPUT stage.
    rule outputHeader if (!clearAll && stageReg == HEADER_OUTPUT);
        let {headerLastFragValidByteNum,
             headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasAfterHeaderRightShiftQ.first;
        let curHeaderDataStreamFrag = rightShiftedHeaderStreamQ.first;
        rightShiftedHeaderStreamQ.deq;

        $display("time=%0t:", $time, " headerDataStream=", fshow(curHeaderDataStreamFrag));
        // let bth = extractBTH(zeroExtendLSB(curHeaderDataStreamFrag.data));
        // if (bth.opcode == ACKNOWLEDGE) begin
        //     $display(
        //         "time=%0t: mkPrependHeader2PipeOut outputHeader", $time,
        //         ", bth.psn=%h, bth.opcode=", bth.psn, fshow(bth.opcode)
        //     );
        // end


        if (isEmptyHeader) begin  // case 1
            let dataStreamFirstBeat = dataPipeIn.first;
            dataPipeIn.deq;

            dataStreamOutQ.enq(dataStreamFirstBeat);
            $display("time=%0t: ", $time, "mkPrependHeader2PipeOut case1==", fshow(dataStreamFirstBeat));

            if (dataStreamFirstBeat.isLast) begin
                calculatedMetasAfterHeaderRightShiftQ.deq;
            end
            else begin
                stageReg <= DATA_OUTPUT;
            end
        end
        else if (!curHeaderDataStreamFrag.isLast || (curHeaderDataStreamFrag.isLast && !headerHasPayload)) begin // case 2 & 3
            dataStreamOutQ.enq(curHeaderDataStreamFrag);
            $display("time=%0t: ", $time, "mkPrependHeader2PipeOut case2,3==", fshow(curHeaderDataStreamFrag));
        end 
        else begin  // case 4
           
            // $display(
            //     "time=%0t: mkPrependHeader2PipeOut outputHeader", $time,
            //     ", headerHasPayload=", fshow(headerHasPayload),
            //     ", headerLastFragValidByteNum=%0d", headerLastFragValidByteNum,
            //     ", headerLastFragInvalidByteNum=%0d", headerLastFragInvalidByteNum,
            //     ", headerLastFragDataStream=", fshow(headerLastFragDataStream)
            // );
            
            let firstDataStreamFrag = dataPipeIn.first;
            dataPipeIn.deq;

            ByteEn lastFragByteEn = truncate(firstDataStreamFrag.byteEn << headerLastFragInvalidByteNum);
            let noExtraLastFrag = isZeroByteEn(lastFragByteEn);

            let tmpData = { curHeaderDataStreamFrag.data, firstDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNum));
            let tmpByteEn = { curHeaderDataStreamFrag.byteEn, firstDataStreamFrag.byteEn } >> headerLastFragValidByteNum;

            preDataStreamReg <= firstDataStreamFrag;

            let headerLastFragDataStreamMergedWithPayload = DataStream {
                data: truncate(tmpData),
                byteEn: truncate(tmpByteEn),
                isFirst: curHeaderDataStreamFrag.isFirst,
                isLast: noExtraLastFrag
            };
            dataStreamOutQ.enq(headerLastFragDataStreamMergedWithPayload);
            $display("time=%0t: ", $time, "mkPrependHeader2PipeOut case 4==", fshow(headerLastFragDataStreamMergedWithPayload));

            if (firstDataStreamFrag.isLast) begin
                if (!noExtraLastFrag) begin
                    stageReg <= EXTRA_LAST_FRAG_OUTPUT;
                end
                else begin
                    calculatedMetasAfterHeaderRightShiftQ.deq;
                end
            end 
            else begin
                stageReg <= DATA_OUTPUT;
            end
        end
    endrule

    rule outputData if (!clearAll && stageReg == DATA_OUTPUT);
        let {headerLastFragValidByteNum,
             headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasAfterHeaderRightShiftQ.first;
        
        let curDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;

        preDataStreamReg <= curDataStreamFrag;


        // Check the last data fragment has less than headerLastFragInvalidByteNum valid bytes,
        // If no, then extra last fragment, otherwise none.
        ByteEn lastFragByteEn = truncate(curDataStreamFrag.byteEn << headerLastFragInvalidByteNum);
        let noExtraLastFrag = isZeroByteEn(lastFragByteEn);
        // let noExtraLastFrag = isZero(lastFragByteEn); // If 256-bit bus, this is 32-bit and reduction

        let tmpData = { preDataStreamReg.data, curDataStreamFrag.data } >> getFragEnBitNumByByteEnNum(truncate(headerLastFragValidByteNum));
        let tmpByteEn = { preDataStreamReg.byteEn, curDataStreamFrag.byteEn } >> headerLastFragValidByteNum;

        let outDataStream = DataStream {
            data: truncate(tmpData),
            byteEn: truncate(tmpByteEn),
            isFirst: False,
            isLast: curDataStreamFrag.isLast && noExtraLastFrag
        };
        dataStreamOutQ.enq(outDataStream);
        $display("time=%0t: ", $time, "mkPrependHeader2PipeOut data ==", fshow(outDataStream));

        if (curDataStreamFrag.isLast) begin
            if (noExtraLastFrag) begin
                stageReg <= HEADER_OUTPUT;
                calculatedMetasAfterHeaderRightShiftQ.deq;
            end
            else begin
                stageReg <= EXTRA_LAST_FRAG_OUTPUT;
            end
        end
        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut outputData", $time,
        //     ", headerLastFragInvalidByteNum=%0d, noExtraLastFrag=",
        //     headerLastFragInvalidByteNum, fshow(noExtraLastFrag),
        //     ", preDataStreamReg=", fshow(preDataStreamReg),
        //     ", curDataStreamFrag=", fshow(curDataStreamFrag),
        //     ", outDataStream=", fshow(outDataStream)
        // );
    endrule

    rule extraLastFrag if (!clearAll && stageReg == EXTRA_LAST_FRAG_OUTPUT);
        let {headerLastFragValidByteNum,
             headerLastFragInvalidByteNum, headerHasPayload, isEmptyHeader} = calculatedMetasAfterHeaderRightShiftQ.first;
        
        DATA leftShiftData = truncate(preDataStreamReg.data << getFragEnBitNumByByteEnNum(truncate(headerLastFragInvalidByteNum)));
        ByteEn leftShiftByteEn = truncate(preDataStreamReg.byteEn << headerLastFragInvalidByteNum);
        let extraLastDataStream = DataStream {
            data   : leftShiftData,
            byteEn : leftShiftByteEn,
            isFirst: False,
            isLast : True
        };

        dataStreamOutQ.enq(extraLastDataStream);
        $display("time=%0t: ", $time, "mkPrependHeader2PipeOut data extra ==", fshow(extraLastDataStream));
        stageReg <= HEADER_OUTPUT;
        calculatedMetasAfterHeaderRightShiftQ.deq;
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

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(HEADER_OUTPUT);
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
    // 4. if header is last beat, but Data has extra beat, then only output Header and jump to DATA_OUTPUT
    rule outputHeader if (stageReg == HEADER_OUTPUT);

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
                stageReg <= DATA_OUTPUT;
                preDataStreamReg <= inDataStreamFrag;
                isFirstDataFragReg <= True;
            end
        end
    endrule

    rule outputData if (stageReg == DATA_OUTPUT);

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
                stageReg <= HEADER_OUTPUT;
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

        stageReg <= HEADER_OUTPUT;
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


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
    HEADER_META_DATA_POP,
    HEADER_OUTPUT,
    DATA_OUTPUT,
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

    // preDataStreamReg is right aligned
    Reg#(DataStream)                  preDataStreamReg <- mkRegU;
    Reg#(HeaderFragNum)               headerFragCntReg <- mkRegU;
    Reg#(BusBitNum)     headerLastFragInvalidBitNumReg <- mkRegU;
    Reg#(ByteEnBitNum) headerLastFragInvalidByteNumReg <- mkRegU;
    Reg#(BusBitNum)       headerLastFragValidBitNumReg <- mkRegU;
    Reg#(ByteEnBitNum)   headerLastFragValidByteNumReg <- mkRegU;

    Reg#(Bool) headerHasPayloadReg <- mkRegU;
    Reg#(Bool)          isFirstReg <- mkRegU;

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(HEADER_META_DATA_POP);

    // rule debug;
    //     if (!dataStreamOutQ.notFull) begin
    //         $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkPrependHeader2PipeOut dataStreamOutQ");
    //     end
    // endrule


    // rule debug if (!dataStreamOutQ.notFull);
    //     $display(
    //         "time=%0t: mkPrependHeader2PipeOut debug", $time,
    //         ", dataStreamOutQ.notFull=", fshow(dataStreamOutQ.notFull)
    //     );
    // endrule

    (* no_implicit_conditions, fire_when_enabled *)
    rule resetAndClear if (clearAll);
        dataStreamOutQ.clear;
        stageReg <= HEADER_META_DATA_POP;

        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut, resetAndClear", $time,
        //     ", sqpn=%h", cntrlStatus.comm.getSQPN
        // );
    endrule

    rule popHeaderMetaData if (!clearAll && stageReg == HEADER_META_DATA_POP);
        let headerMetaData = headerMetaDataPipeIn.first;
        headerMetaDataPipeIn.deq;
        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut popHeaderMetaData", $time,
        //     ", headerMetaData=", fshow(headerMetaData)
        // );

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

        headerLastFragValidByteNumReg   <= headerLastFragValidByteNum;
        headerLastFragValidBitNumReg    <= headerLastFragValidBitNum;
        headerLastFragInvalidByteNumReg <= headerLastFragInvalidByteNum;
        headerLastFragInvalidBitNumReg  <= headerLastFragInvalidBitNum;

        headerFragCntReg    <= headerMetaData.isEmptyHeader ? 0 : (headerFragNum - 1);
        headerHasPayloadReg <= headerMetaData.hasPayload;
        stageReg   <= headerMetaData.isEmptyHeader ? DATA_OUTPUT : HEADER_OUTPUT;
        isFirstReg <= True;
    endrule

    rule outputHeader if (!clearAll && stageReg == HEADER_OUTPUT);
        let curHeaderDataStreamFrag = headerPipeIn.first;
        headerPipeIn.deq;
        // $display("time=%0t:", $time, " headerDataStream=", fshow(curHeaderDataStreamFrag));
        // let bth = extractBTH(zeroExtendLSB(curHeaderDataStreamFrag.data));
        // if (bth.opcode == ACKNOWLEDGE) begin
        //     $display(
        //         "time=%0t: mkPrependHeader2PipeOut outputHeader", $time,
        //         ", bth.psn=%h, bth.opcode=", bth.psn, fshow(bth.opcode)
        //     );
        // end

        // One cycle delay when output the last fragment of header
        if (curHeaderDataStreamFrag.isLast) begin
            immAssert(
                isZero(headerFragCntReg),
                "headerFragCntReg zero assertion @ mkPrependHeader2PipeOut",
                $format(
                    "headerFragCntReg=%h should be zero when curHeaderDataStreamFrag.isLast=%b",
                    headerFragCntReg, curHeaderDataStreamFrag.isLast
                )
            );

            let rightShiftHeaderLastFragData = curHeaderDataStreamFrag.data >> headerLastFragInvalidBitNumReg;
            let rightShiftHeaderLastFragByteEn = curHeaderDataStreamFrag.byteEn >> headerLastFragInvalidByteNumReg;
            let headerLastFragDataStream = DataStream {
                data: rightShiftHeaderLastFragData,
                byteEn: rightShiftHeaderLastFragByteEn,
                isFirst: curHeaderDataStreamFrag.isFirst,
                isLast: !headerHasPayloadReg
            };
            preDataStreamReg <= headerLastFragDataStream;
            // $display(
            //     "time=%0t: mkPrependHeader2PipeOut outputHeader", $time,
            //     ", headerHasPayloadReg=", fshow(headerHasPayloadReg),
            //     ", headerLastFragValidByteNum=%0d", headerLastFragValidByteNumReg,
            //     ", headerLastFragValidBitNum=%0d", headerLastFragValidBitNumReg,
            //     ", headerLastFragInvalidByteNum=%0d", headerLastFragInvalidByteNumReg,
            //     ", headerLastFragInvalidBitNum=%0d", headerLastFragInvalidBitNumReg,
            //     ", headerLastFragDataStream=", fshow(headerLastFragDataStream)
            // );

            stageReg <= headerHasPayloadReg ? DATA_OUTPUT : HEADER_META_DATA_POP;
            if (!headerHasPayloadReg) begin
                // Output headerLastFragDataStream if header has no payload
                dataStreamOutQ.enq(curHeaderDataStreamFrag);
            end
        end
        else begin
            isFirstReg <= False;
            headerFragCntReg <= headerFragCntReg - 1;
            dataStreamOutQ.enq(curHeaderDataStreamFrag);
        end
    endrule

    rule outputData if (!clearAll && stageReg == DATA_OUTPUT);
        let curDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;

        preDataStreamReg <= curDataStreamFrag;
        isFirstReg <= False;

        // Check the last data fragment has less than headerLastFragInvalidByteNumReg valid bytes,
        // If no, then extra last fragment, otherwise none.
        ByteEn lastFragByteEn = truncate(curDataStreamFrag.byteEn << headerLastFragInvalidByteNumReg);
        let noExtraLastFrag = isZeroByteEn(lastFragByteEn);
        // let noExtraLastFrag = isZero(lastFragByteEn); // If 256-bit bus, this is 32-bit and reduction

        let tmpData = { preDataStreamReg.data, curDataStreamFrag.data } >> headerLastFragValidBitNumReg;
        let tmpByteEn = { preDataStreamReg.byteEn, curDataStreamFrag.byteEn } >> headerLastFragValidByteNumReg;

        let outDataStream = DataStream {
            data: truncate(tmpData),
            byteEn: truncate(tmpByteEn),
            isFirst: isFirstReg,
            isLast: curDataStreamFrag.isLast && noExtraLastFrag
        };
        dataStreamOutQ.enq(outDataStream);

        if (curDataStreamFrag.isLast) begin
            if (noExtraLastFrag) begin
                stageReg <= HEADER_META_DATA_POP;
            end
            else begin
                stageReg <= EXTRA_LAST_FRAG_OUTPUT;
            end
        end
        // $display(
        //     "time=%0t: mkPrependHeader2PipeOut outputData", $time,
        //     ", headerLastFragInvalidByteNumReg=%0d, noExtraLastFrag=",
        //     headerLastFragInvalidByteNumReg, fshow(noExtraLastFrag),
        //     ", preDataStreamReg=", fshow(preDataStreamReg),
        //     ", curDataStreamFrag=", fshow(curDataStreamFrag),
        //     ", outDataStream=", fshow(outDataStream)
        // );
    endrule

    rule extraLastFrag if (!clearAll && stageReg == EXTRA_LAST_FRAG_OUTPUT);
        DATA leftShiftData = truncate(preDataStreamReg.data << headerLastFragInvalidBitNumReg);
        ByteEn leftShiftByteEn = truncate(preDataStreamReg.byteEn << headerLastFragInvalidByteNumReg);
        let extraLastDataStream = DataStream {
            data   : leftShiftData,
            byteEn : leftShiftByteEn,
            isFirst: False,
            isLast : True
        };

        dataStreamOutQ.enq(extraLastDataStream);
        stageReg <= HEADER_META_DATA_POP;
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
    FIFOF#(DataStreamFragMetaData) payloadDataStreamFragPreOutQ <- mkFIFOF;
    FIFOF#(DataStreamFragMetaData) payloadDataStreamFragOutQ    <- mkFIFOF;

    Reg#(DataStream)                  preDataStreamReg <- mkRegU;
    Reg#(DataStream)                  curDataStreamReg <- mkRegU;
    Reg#(HeaderMetaData)             headerMetaDataReg <- mkRegU;
    Reg#(ByteEn)               headerLastFragByteEnReg <- mkRegU;
    Reg#(BusBitNum)     headerLastFragInvalidBitNumReg <- mkRegU;
    Reg#(ByteEnBitNum) headerLastFragInvalidByteNumReg <- mkRegU;
    Reg#(BusBitNum)       headerLastFragValidBitNumReg <- mkRegU;
    Reg#(ByteEnBitNum)   headerLastFragValidByteNumReg <- mkRegU;
    Reg#(Bool)                      isFirstDataFragReg <- mkRegU;
    Reg#(Bool)                     isHeaderLastFragReg <- mkRegU;
    Reg#(ByteEn)           shiftedCurDataFragByteEnReg <- mkRegU;

    Reg#(ExtractOrPrependHeaderStage) stageReg <- mkReg(HEADER_META_DATA_POP);
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
    rule popHeaderMetaData if (stageReg == HEADER_META_DATA_POP);
        let headerMetaData = headerMetaDataPipeIn.first;
        headerMetaDataPipeIn.deq;
        // $display("time=%0t: headerMetaData=", $time, fshow(headerMetaData));
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

        headerLastFragValidByteNumReg   <= headerLastFragValidByteNum;
        headerLastFragValidBitNumReg    <= headerLastFragValidBitNum;
        headerLastFragInvalidByteNumReg <= headerLastFragInvalidByteNum;
        headerLastFragInvalidBitNumReg  <= headerLastFragInvalidBitNum;

        let headerLastFragByteEn = genByteEn(headerLastFragValidByteNum);
        headerLastFragByteEnReg <= headerLastFragByteEn;
        headerMetaData.headerFragNum = headerFragNum - 1;
        headerMetaDataReg   <= headerMetaData;
        isHeaderLastFragReg <= isOne(headerFragNum);

        let firstDataStreamFrag = dataPipeIn.first;
        dataPipeIn.deq;

        curDataStreamReg <= firstDataStreamFrag;
        ByteEn tmpByteEn  = truncate(firstDataStreamFrag.byteEn << headerLastFragValidByteNum);
        shiftedCurDataFragByteEnReg <= tmpByteEn;
        // $display(
        //     "time=%0t:", $time, " headerMetaData=", fshow(headerMetaData),
        //     ", headerLastFragByteEn=%h", reverseBits(headerLastFragByteEn),
        //     ", headerLastFragValidByteNum=%0d", headerLastFragValidByteNum,
        //     ", headerLastFragValidBitNum=%0d", headerLastFragValidBitNum,
        //     ", headerLastFragInvalidByteNum=%0d", headerLastFragInvalidByteNum,
        //     ", headerLastFragInvalidBitNum=%0d", headerLastFragInvalidBitNum,
        //     ", curDataStreamReg=", fshow(curDataStreamReg),
        //     ", stageReg=", fshow(stageReg)
        // );
        stageReg <= HEADER_OUTPUT;
    endrule

    rule outputHeader if (stageReg == HEADER_OUTPUT);
        let curDataStreamFrag = curDataStreamReg;
        preDataStreamReg <= curDataStreamFrag;

        let headerMetaData = headerMetaDataReg;
        let headerFragNum  = headerMetaData.headerFragNum;

        // Check dataPipeIn has more data after header
        let isHeaderLastFrag = isHeaderLastFragReg;
        let hasDataFragAfterHeader = !isZeroByteEn(shiftedCurDataFragByteEnReg) && isHeaderLastFrag;
        let outDataStream = curDataStreamFrag;

        if (curDataStreamFrag.isLast) begin // Might not have enough data for header
            outDataStream.byteEn = hasDataFragAfterHeader ?
                headerLastFragByteEnReg : curDataStreamFrag.byteEn;
            outDataStream.isLast = True;

            isFirstDataFragReg <= hasDataFragAfterHeader;
            stageReg <= hasDataFragAfterHeader ? (
                isHeaderLastFrag ?
                    EXTRA_LAST_FRAG_OUTPUT : // One one fragment of payload data
                    DATA_OUTPUT              // More than one fragments of payload data
            ) : HEADER_META_DATA_POP;        // No payload data

            if (!hasDataFragAfterHeader) begin
                // Put an empty DataStream into payloadDataStreamFragPreOutQ,
                // even if header has no payload or no enough data for header,
                // this is to align header and data, for easy handling.
                let emptyDataStream = DataStreamFragMetaData {
                    bufIdx: ?,
                    byteEn: 0,
                    isFirst: True,
                    isLast: True
                };
                payloadDataStreamFragPreOutQ.enq(emptyDataStream);
                payloadStreamFragStorageInsertCltInst.putReq(0);
            end
        end
        else begin
            let nextDataStreamFrag = dataPipeIn.first;
            dataPipeIn.deq;

            curDataStreamReg <= nextDataStreamFrag;
            ByteEn tmpByteEn  = truncate(nextDataStreamFrag.byteEn << headerLastFragValidByteNumReg);
            shiftedCurDataFragByteEnReg <= tmpByteEn;

            if (isHeaderLastFrag) begin // Finish extracting header
                outDataStream.byteEn = headerLastFragByteEnReg;
                outDataStream.isLast = True;

                isFirstDataFragReg <= True;
                // No matter header has payload or not, since there exists data fragment
                // after header, so it must switch to DATA_OUTPUT stage.
                stageReg <= DATA_OUTPUT;
            end
            else begin
                headerMetaData.headerFragNum = headerFragNum - 1;
                headerMetaDataReg   <= headerMetaData;
                isHeaderLastFragReg <= isOne(headerFragNum);
            end
        end
        // $display(
        //     "time=%0t:", $time,
        //     " header outDataStream=", fshow(outDataStream),
        //     ", curDataStreamFrag=", fshow(curDataStreamFrag),
        //     ", headerMetaData=", fshow(headerMetaData),
        //     ", shiftedCurDataFragByteEnReg=%h", shiftedCurDataFragByteEnReg,
        //     ", hasDataFragAfterHeader=", fshow(hasDataFragAfterHeader),
        //     ", isHeaderLastFrag=", fshow(isHeaderLastFrag),
        //     ", stageReg=", fshow(stageReg)
        // );
        headerDataStreamOutQ.enq(outDataStream);
    endrule

    rule outputData if (stageReg == DATA_OUTPUT);
        let curDataStreamFrag = curDataStreamReg;
        preDataStreamReg   <= curDataStreamFrag;
        isFirstDataFragReg <= False;

        // ByteEn lastFragByteEn = truncate(curDataStreamFrag.byteEn << headerLastFragValidByteNumReg);
        let noExtraLastFrag = isZeroByteEn(shiftedCurDataFragByteEnReg);
        // let noExtraLastFrag = isZero(lastFragByteEn); // If 256-bit bus, this is 32-bit and reduction

        let outData   = { preDataStreamReg.data, curDataStreamReg.data } >> headerLastFragInvalidBitNumReg;
        let outByteEn = { preDataStreamReg.byteEn, curDataStreamReg.byteEn } >> headerLastFragInvalidByteNumReg;
        let outDataStream = DataStreamFragMetaData {
            bufIdx : ?,
            byteEn : truncate(outByteEn),
            isFirst: isFirstDataFragReg,
            isLast : curDataStreamFrag.isLast && noExtraLastFrag
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
                stageReg <= HEADER_META_DATA_POP;
            end
            else begin
                stageReg <= EXTRA_LAST_FRAG_OUTPUT;
            end
        end
        else begin
            let nextDataStreamFrag = dataPipeIn.first;
            dataPipeIn.deq;

            curDataStreamReg <= nextDataStreamFrag;
            ByteEn tmpByteEn  = truncate(nextDataStreamFrag.byteEn << headerLastFragValidByteNumReg);
            shiftedCurDataFragByteEnReg <= tmpByteEn;
        end
    endrule

    rule extraLastFrag if (stageReg == EXTRA_LAST_FRAG_OUTPUT);
        DATA leftShiftData      = truncate(preDataStreamReg.data << headerLastFragValidBitNumReg);
        ByteEn leftShiftByteEn  = truncate(preDataStreamReg.byteEn << headerLastFragValidByteNumReg);
        let extraLastDataStream = DataStreamFragMetaData {
            bufIdx : ?,
            byteEn: leftShiftByteEn,
            isFirst: isFirstDataFragReg,
            isLast: True
        };
        isFirstDataFragReg <= False;

        // $display("time=%0t: extraLastDataStream=", $time, fshow(extraLastDataStream));
        payloadDataStreamFragPreOutQ.enq(extraLastDataStream);
        payloadStreamFragStorageInsertCltInst.putReq(leftShiftData);

        stageReg <= HEADER_META_DATA_POP;
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


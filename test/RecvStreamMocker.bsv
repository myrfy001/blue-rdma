import FIFOF :: *;
import ExtractAndPrependPipeOut :: *;
import Settings :: *;
import DataTypes :: *;
import Headers :: *;
import RdmaUtils :: *;
import PrimUtils :: *;


typedef 32768 MAX_PAYLOAD_BITS;  // 4kB * 8bit

typedef struct{
    HeaderData  headerBuf;
    HeaderByteEn byteEn;
    HeaderBitNum bitNum;
    Bit#(MAX_PAYLOAD_BITS) payloadBuf;
    Length payloadLen;
} MockHeaderStream deriving(Bits);

typedef struct{
    HeaderData  headerBuf;
    HeaderByteEn byteEn;
    HeaderBitNum bitNum;
} MockPayloadStream deriving(Bits);



function MockHeaderStream appendSegToMockHeaderStream(MockHeaderStream inp, t_seg seg) provisos(Bits#(t_seg, sz_seg), Add#(any_size, sz_seg, 512));
    HeaderData t = {pack(seg), 0};
    inp.headerBuf = inp.headerBuf | (t >> inp.bitNum);
    inp.bitNum = inp.bitNum + fromInteger(valueOf(sz_seg));
    return inp;
endfunction

function MockHeaderStream fillBthOfMockedHeaderStream(MockHeaderStream inp);
    BTH bth = unpack(inp.headerBuf[
        valueOf(HEADER_MAX_DATA_WIDTH)-1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH)
    ]);


    bth.padCnt = calcPadCnt(zeroExtend(inp.payloadLen));
    inp.payloadLen = inp.payloadLen + zeroExtend(bth.padCnt);

    inp.headerBuf[
        valueOf(HEADER_MAX_DATA_WIDTH) - 1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH)
    ] = pack(bth);

    return inp;
endfunction

function MockHeaderStream addPayloadToMockHeaderStream(MockHeaderStream inp, t_payload payloadBuf, Length len) provisos(Bits#(t_payload, sz_payload), Add#(any_size, sz_payload, MAX_PAYLOAD_BITS));
    inp.payloadBuf = {pack(payloadBuf), 0};
    inp.payloadLen = len;
    return inp;
endfunction

interface RecvStreamMocker;
    method Action addDataToMock(MockHeaderStream header);
    interface DataStreamPipeOut streamPipeOut;
endinterface


// typedef struct {
//     HeaderByteNum headerLen;
//     HeaderFragNum headerFragNum;
//     ByteEnBitNum lastFragValidByteNum;
//     Bool hasPayload;
// } HeaderMetaData deriving(Bits, Bounded, Eq);


module mkRecvStreamMocker(RecvStreamMocker);

    FIFOF#(HeaderMetaData) headerMetaQ <- mkFIFOF;
    FIFOF#(DataStream) headerDataQ <- mkFIFOF;
    FIFOF#(DataStream) payloadDataQ <- mkFIFOF;

    FIFOF#(MockHeaderStream) headerStreamInputQ <- mkFIFOF;
    Reg#(Bool) isFirstHeaderReg <- mkReg(True);
    Reg#(Bool) isFirstPayloadReg <- mkReg(True);
    Reg#(HeaderMetaData) headerMetaForCurCalcReg <- mkReg(unpack(0));

    Reg#(MockHeaderStream) curInputRegForHeaderMetaGenReg <- mkRegU;
    Reg#(MockHeaderStream) curInputRegForHeaderStreamGenReg <- mkRegU;
    Reg#(MockHeaderStream) curInputRegForPayloadGenReg <- mkRegU;

    Reg#(Bool) isHeaderMetaGenerateDoneReg <- mkReg(True);
    Reg#(Bool) isHeaderStreamGenerateDoneReg <- mkReg(True);
    Reg#(Bool) isPayloadGenerateDoneReg <- mkReg(True);

    Reg#(Bool) clearReg <- mkReg(False);
    let prependHeader2PipeOut <- mkPrependHeader2PipeOut(
        clearReg,
        toPipeOut(headerDataQ),
        toPipeOut(headerMetaQ),
        toPipeOut(payloadDataQ)
    );


    rule startNewGen if (isHeaderMetaGenerateDoneReg && isHeaderStreamGenerateDoneReg && isPayloadGenerateDoneReg);
        isHeaderMetaGenerateDoneReg <= False;
        isHeaderStreamGenerateDoneReg <= False;
        isPayloadGenerateDoneReg <= False;
        curInputRegForHeaderMetaGenReg <= headerStreamInputQ.first;
        curInputRegForHeaderStreamGenReg <= headerStreamInputQ.first;
        curInputRegForPayloadGenReg <= headerStreamInputQ.first;

        isFirstHeaderReg <= True;
        let headerMeta = genHeaderMetaData(truncate(headerStreamInputQ.first.bitNum >> 3), True);
        headerMetaForCurCalcReg <= headerMeta;

        isFirstPayloadReg <= True;

        headerStreamInputQ.deq;

        $display("11111");
    endrule


    rule genHeaderMeta if (!isHeaderMetaGenerateDoneReg);
        isHeaderMetaGenerateDoneReg <= True;
        let inputReq = curInputRegForHeaderMetaGenReg;
        let headerMeta = genHeaderMetaData(truncate(inputReq.bitNum >> 3), True);
        headerMetaQ.enq(headerMeta);
        $display("output headerMeta====", fshow(headerMeta));
    endrule

    rule genHeaderStream if (!isHeaderStreamGenerateDoneReg);
        isHeaderMetaGenerateDoneReg <= True;

        let inputReq = curInputRegForHeaderStreamGenReg;
        let headerMeta = headerMetaForCurCalcReg;

        DataStream ds = DataStream{
            data: truncateLSB(inputReq.headerBuf),
            byteEn: headerMeta.headerFragNum == 1 ? genByteEn(headerMeta.lastFragValidByteNum) : -1,
            isFirst: isFirstHeaderReg,
            isLast: headerMeta.headerFragNum == 1
        };
        isFirstHeaderReg <= False;

        inputReq.headerBuf = inputReq.headerBuf << valueOf(DATA_BUS_WIDTH);
        curInputRegForHeaderStreamGenReg <= inputReq;

        headerMeta.headerFragNum = headerMeta.headerFragNum - 1;
        headerMetaForCurCalcReg <= headerMeta;

        headerDataQ.enq(ds);

        if (ds.isLast) begin
            isHeaderStreamGenerateDoneReg <= True;
        end

        $display("output headerStream====", fshow(ds));
    endrule

    rule genPayloadStream if (!isPayloadGenerateDoneReg);
        let inputReq = curInputRegForPayloadGenReg;
        let isLast = inputReq.payloadLen <= fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
        DataStream ds = DataStream{
            data: truncateLSB(inputReq.payloadBuf),
            byteEn: isLast ? genByteEn(truncate(inputReq.payloadLen)) : -1,
            isFirst: isFirstPayloadReg,
            isLast: isLast
        };
        isFirstPayloadReg <= False;
        inputReq.payloadBuf = inputReq.payloadBuf << valueOf(DATA_BUS_WIDTH);

        inputReq.payloadLen = inputReq.payloadLen - fromInteger(valueOf(DATA_BUS_BYTE_WIDTH));
        curInputRegForPayloadGenReg <= inputReq;

        payloadDataQ.enq(ds);

        if (isLast) begin
            isPayloadGenerateDoneReg <= True;
        end

        $display("output payloadStream====", fshow(ds));
    endrule



    method Action addDataToMock(MockHeaderStream header);
        headerStreamInputQ.enq(header);
    endmethod

    interface streamPipeOut = prependHeader2PipeOut;
endmodule


import FIFOF :: *;
import SpecialFIFOs :: *;
import ClientServer :: *;
import GetPut :: *;
import DataTypes :: *;
import Headers :: *;
import Vector :: *;
import BRAM :: *;
import Printf:: *;

import PAClib :: *;
import PrimUtils :: *;
import Settings :: *;

function PipeOut#(anytype) toPipeOut(FIFOF#(anytype) queue);
    return f_FIFOF_to_PipeOut(queue);
endfunction

interface BypassClient#(type t_req, type t_resp);
    interface Client#(t_req, t_resp) clt;
    method Action putReq(t_req req);
    method Bool canPutReq;

    method ActionValue#(t_resp) getResp();
    method Bool hasResp;
endinterface

module mkSizedBypassClient#(String name, Integer reqDepth, Integer respDepth)(BypassClient#(t_req, t_resp)) provisos (
    Bits#(t_req, sz_req),
    Bits#(t_resp, sz_resp)
);
    FIFOF#(t_req) reqQ <- mkSizedFIFOF(reqDepth);
    // FIFOF#(t_req) reqQ <- mkBypassFIFOF;
    FIFOF#(t_resp) respQ <- mkSizedFIFOF(respDepth);
    // FIFOF#(t_resp) respQ <- mkBypassFIFOF;

    rule debug;
        if (!reqQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkBypassClient ", fshow(name) , " reqQ");
        end
        if (!respQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkBypassClient ", fshow(name) , " respQ");
        end
    endrule

    interface Client clt;
        interface Get request;
            method ActionValue#(t_req) get();
                reqQ.deq;
                return reqQ.first;
            endmethod
        endinterface
        interface Put response;
            method Action put(t_resp resp);
                respQ.enq(resp);
            endmethod
        endinterface
    endinterface

    method Action putReq(t_req req);
        reqQ.enq(req);
    endmethod

    method Bool canPutReq = reqQ.notFull;

    method ActionValue#(t_resp) getResp();
        respQ.deq;
        return respQ.first;
    endmethod

    method Bool hasResp = respQ.notEmpty;
endmodule

module mkBypassClient#(String name)(BypassClient#(t_req, t_resp)) provisos (
    Bits#(t_req, sz_req),
    Bits#(t_resp, sz_resp)
);
    let t <- mkSizedBypassClient(name, 2, 2);
    return t;
endmodule


interface BypassServer#(type t_req, type t_resp);
    interface Server#(t_req, t_resp) srv;

    method ActionValue#(t_req) getReq();
    method Bool hasReq;

    method Action putResp(t_resp resp);
    method Bool canPutResp;
endinterface

module mkSizedBypassServer#(String name, Integer reqDepth, Integer respDepth)(BypassServer#(t_req, t_resp)) provisos (
    Bits#(t_req, sz_req),
    Bits#(t_resp, sz_resp)
);
    FIFOF#(t_req) reqQ <- mkSizedFIFOF(reqDepth);
    // FIFOF#(t_req) reqQ <- mkBypassFIFOF;
    // FIFOF#(t_resp) respQ <- mkBypassFIFOF;
    FIFOF#(t_resp) respQ <- mkSizedFIFOF(respDepth);

    rule debug;
        if (!reqQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkBypassServer ", fshow(name) , " reqQ");
        end
        if (!respQ.notFull) begin
            $display("time=%0t: ", $time, "FULL_QUEUE_DETECTED: mkBypassServer ", fshow(name) , " respQ");
        end
    endrule

    interface Server srv;
        interface Get response;
            method ActionValue#(t_resp) get();
                respQ.deq;
                return respQ.first;
            endmethod
        endinterface
        interface Put request;
            method Action put(t_req req);
                reqQ.enq(req);
            endmethod
        endinterface
    endinterface

    method Action putResp(t_resp resp);
        respQ.enq(resp);
    endmethod

    method Bool canPutResp = respQ.notFull;

    method ActionValue#(t_req) getReq();
        reqQ.deq;
        return reqQ.first;
    endmethod

    method Bool hasReq = reqQ.notEmpty;

endmodule


module mkBypassServer#(String name)(BypassServer#(t_req, t_resp)) provisos (
    Bits#(t_req, sz_req),
    Bits#(t_resp, sz_resp)
);
     
    let t <- mkSizedBypassServer(name, 2, 2);
    return t;
endmodule


function BTH extractBTH(HeaderData headerData);
    let bth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH)-1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH)
    ]);
    return bth;
endfunction

function RETH extractPriRETH(HeaderData headerData, TransType transType);
    let reth = case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH)
        ]);
        default: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH)
        ]);
    endcase;
    return reth;
endfunction

function RETH extractSecRETH(HeaderData headerData, TransType transType, RdmaOpCode opcode);
    let reth = case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH)
        ]);
        default: begin
            case (opcode)
                RDMA_READ_REQUEST:
                    unpack(headerData[
                        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) -1 :
                        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) - valueOf(RETH_WIDTH)
                    ]);
                // RDMA_READ_RESPONSE_MIDDLE:
                //     unpack(headerData[
                //         valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
                //         valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH)
                //     ]);
                // RDMA_READ_RESPONSE_FIRST, RDMA_READ_RESPONSE_LAST, RDMA_READ_RESPONSE_ONLY:
                //     unpack(headerData[
                //         valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_BYTE_WIDTH) -1 :
                //         valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_BYTE_WIDTH) - valueOf(RETH_WIDTH)
                //     ]);
                default: unpack(0);  // error("Opcode does not support secondary RETH");
            endcase
        end
    endcase;
    return reth;
endfunction


// Timeout related

// RNR timeout settings:
// 0 - 655.36 milliseconds delay
// 1 - 0.01 milliseconds delay
// 2 - 0.02 milliseconds delay
// 3 - 0.03 milliseconds delay
// 4 - 0.04 milliseconds delay
// 5 - 0.06 milliseconds delay
// 6 - 0.08 milliseconds delay
// 7 - 0.12 milliseconds delay
// 8 - 0.16 milliseconds delay
// 9 - 0.24 milliseconds delay
// 10 - 0.32 milliseconds delay
// 11 - 0.48 milliseconds delay
// 12 - 0.64 milliseconds delay
// 13 - 0.96 milliseconds delay
// 14 - 1.28 milliseconds delay
// 15 - 1.92 milliseconds delay
// 16 - 2.56 milliseconds delay
// 17 - 3.84 milliseconds delay
// 18 - 5.12 milliseconds delay
// 19 - 7.68 milliseconds delay
// 20 - 10.24 milliseconds delay
// 21 - 15.36 milliseconds delay
// 22 - 20.48 milliseconds delay
// 23 - 30.72 milliseconds delay
// 24 - 40.96 milliseconds delay
// 25 - 61.44 milliseconds delay
// 26 - 81.92 milliseconds delay
// 27 - 122.88 milliseconds delay
// 28 - 163.84 milliseconds delay
// 29 - 245.76 milliseconds delay
// 30 - 327.68 milliseconds delay
// 31 - 491.52 milliseconds delay
function Integer getRnrTimeOutValue(RnrTimer rnrTimer);
    // RNR timeout value in microseconds
    Integer rnrTimeOutValues[32] = {
        valueOf(TDiv#(TMul#(655360, 1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(10,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(20,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(30,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(40,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(60,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(80,     1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(120,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(160,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(240,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(320,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(480,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(640,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(960,    1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(1280,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(1920,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(2560,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(3840,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(5120,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(7680,   1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(10240,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(15360,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(20480,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(30720,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(40960,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(61440,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(81920,  1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(122880, 1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(163840, 1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(245760, 1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(327680, 1000), TARGET_CYCLE_NS)),
        valueOf(TDiv#(TMul#(491520, 1000), TARGET_CYCLE_NS))
    };
    return rnrTimeOutValues[rnrTimer];
endfunction

// Response timeout settings:
//  0 - infinite
//  1 - 8.192 usec (0.000008 sec)
//  2 - 16.384 usec (0.000016 sec)
//  3 - 32.768 usec (0.000032 sec)
//  4 - 65.536 usec (0.000065 sec)
//  5 - 131.072 usec (0.000131 sec)
//  6 - 262.144 usec (0.000262 sec)
//  7 - 524.288 usec (0.000524 sec)
//  8 - 1048.576 usec (0.00104 sec)
//  9 - 2097.152 usec (0.00209 sec)
//  10 - 4194.304 usec (0.00419 sec)
//  11 - 8388.608 usec (0.00838 sec)
//  12 - 16777.22 usec (0.01677 sec)
//  13 - 33554.43 usec (0.0335 sec)
//  14 - 67108.86 usec (0.0671 sec)
//  15 - 134217.7 usec (0.134 sec)
//  16 - 268435.5 usec (0.268 sec)
//  17 - 536870.9 usec (0.536 sec)
//  18 - 1073742 usec (1.07 sec)
//  19 - 2147484 usec (2.14 sec)
//  20 - 4294967 usec (4.29 sec)
//  21 - 8589935 usec (8.58 sec)
//  22 - 17179869 usec (17.1 sec)
//  23 - 34359738 usec (34.3 sec)
//  24 - 68719477 usec (68.7 sec)
//  25 - 137000000 usec (137 sec)
//  26 - 275000000 usec (275 sec)
//  27 - 550000000 usec (550 sec)
//  28 - 1100000000 usec (1100 sec)
//  29 - 2200000000 usec (2200 sec)
//  30 - 4400000000 usec (4400 sec)
//  31 - 8800000000 usec (8800 sec)
function Integer getTimeOutValue(TimeOutTimer timeOutTimer);
    // Timeout value in nanoseconds
    return case (timeOutTimer)
         1     : valueOf(TDiv#(TMul#(8192, TExp#( 0)), TARGET_CYCLE_NS));
         2     : valueOf(TDiv#(TMul#(8192, TExp#( 1)), TARGET_CYCLE_NS));
         3     : valueOf(TDiv#(TMul#(8192, TExp#( 2)), TARGET_CYCLE_NS));
         4     : valueOf(TDiv#(TMul#(8192, TExp#( 3)), TARGET_CYCLE_NS));
         5     : valueOf(TDiv#(TMul#(8192, TExp#( 4)), TARGET_CYCLE_NS));
         6     : valueOf(TDiv#(TMul#(8192, TExp#( 5)), TARGET_CYCLE_NS));
         7     : valueOf(TDiv#(TMul#(8192, TExp#( 6)), TARGET_CYCLE_NS));
         8     : valueOf(TDiv#(TMul#(8192, TExp#( 7)), TARGET_CYCLE_NS));
         9     : valueOf(TDiv#(TMul#(8192, TExp#( 8)), TARGET_CYCLE_NS));
        10     : valueOf(TDiv#(TMul#(8192, TExp#( 9)), TARGET_CYCLE_NS));
        11     : valueOf(TDiv#(TMul#(8192, TExp#(10)), TARGET_CYCLE_NS));
        12     : valueOf(TDiv#(TMul#(8192, TExp#(11)), TARGET_CYCLE_NS));
        13     : valueOf(TDiv#(TMul#(8192, TExp#(12)), TARGET_CYCLE_NS));
        14     : valueOf(TDiv#(TMul#(8192, TExp#(13)), TARGET_CYCLE_NS));
        15     : valueOf(TDiv#(TMul#(8192, TExp#(14)), TARGET_CYCLE_NS));
        16     : valueOf(TDiv#(TMul#(8192, TExp#(15)), TARGET_CYCLE_NS));
        17     : valueOf(TDiv#(TMul#(8192, TExp#(16)), TARGET_CYCLE_NS));
        18     : valueOf(TDiv#(TMul#(8192, TExp#(17)), TARGET_CYCLE_NS));
        19     : valueOf(TDiv#(TMul#(8192, TExp#(18)), TARGET_CYCLE_NS));
        20     : valueOf(TDiv#(TMul#(8192, TExp#(19)), TARGET_CYCLE_NS));
        21     : valueOf(TDiv#(TMul#(8192, TExp#(20)), TARGET_CYCLE_NS));
        22     : valueOf(TDiv#(TMul#(8192, TExp#(21)), TARGET_CYCLE_NS));
        23     : valueOf(TDiv#(TMul#(8192, TExp#(22)), TARGET_CYCLE_NS));
        24     : valueOf(TDiv#(TMul#(8192, TExp#(23)), TARGET_CYCLE_NS));
        25     : valueOf(TDiv#(TMul#(8192, TExp#(24)), TARGET_CYCLE_NS));
        26     : valueOf(TDiv#(TMul#(8192, TExp#(25)), TARGET_CYCLE_NS));
        27     : valueOf(TDiv#(TMul#(8192, TExp#(26)), TARGET_CYCLE_NS));
        28     : valueOf(TDiv#(TMul#(8192, TExp#(27)), TARGET_CYCLE_NS));
        29     : valueOf(TDiv#(TMul#(8192, TExp#(28)), TARGET_CYCLE_NS));
        30     : valueOf(TDiv#(TMul#(8192, TExp#(29)), TARGET_CYCLE_NS));
        31     : valueOf(TDiv#(TMul#(8192, TExp#(30)), TARGET_CYCLE_NS));
        default: 0; // Infinite
    endcase;
    // return isZero(timeOutTimer) ? 0 : (8192 << (timeOutTimer - 1));
endfunction

// ByteEn related

function ByteEn genByteEnLeftAlign(ByteEnBitNum fragValidByteNum);
    return reverseBits((1 << fragValidByteNum) - 1);
endfunction

function ByteEn genByteEnRightAlign(ByteEnBitNum fragValidByteNum);
    return (1 << fragValidByteNum) - 1;
endfunction

function ByteEnBitNum calcLastFragValidByteNum(Bit#(nSz) len) provisos(
    Add#(TAdd#(1, DATA_BUS_BYTE_NUM_WIDTH), anysizeK, nSz),
    Add#(DATA_BUS_BYTE_NUM_WIDTH, anysizeJ, nSz)
);
    BusByteWidthMask lowerBits = truncate(len);
    
    return isZero(lowerBits) ? fromInteger(valueOf(DATA_BUS_BYTE_WIDTH)) : zeroExtend(lowerBits);
endfunction

function ByteEnBitNum calcFragInvalidByteNum(
    ByteEnBitNum fragValidByteNum
);
    return fromInteger(valueOf(DATA_BUS_BYTE_WIDTH)) - fragValidByteNum;
endfunction


function Tuple2#(ByteEnBitNum, Bool) satAddTwoByteNum(ByteEnBitNum v1, ByteEnBitNum v2);
    let sum = v1 + v2;
    BusByteWidthMask sumLowerBits = truncate(sum);
    Bool isOverflow = ((msb(sum) == 1) && (!isZeroR(sumLowerBits))) || (msb(v1) == 1 && msb(v2) == 1);
    ByteEnBitNum ret = isOverflow ? fromInteger(valueOf(DATA_BUS_BYTE_WIDTH)) : sum;

    return tuple2(ret, isOverflow);
endfunction


function DataStreamEn dataStream2DataStreamEnLeftAlign(DataStream inStream);
    return DataStreamEn{
        data: inStream.data,
        byteEn: genByteEnLeftAlign(inStream.byteNum),
        isFirst: inStream.isFirst,
        isLast: inStream.isLast
    };
endfunction

function DataStreamEn dataStream2DataStreamEnRightAlign(DataStream inStream);
    return DataStreamEn{
        data: inStream.data,
        byteEn: genByteEnRightAlign(inStream.byteNum),
        isFirst: inStream.isFirst,
        isLast: inStream.isLast
    };
endfunction

function DataStream dataStreamEnLeftAlign2DataStream(DataStreamEn inStream);
    return DataStream{
        data: inStream.data,
        byteNum: fromMaybe(?, calcFragByteNumFromByteEnLeftAlign(inStream.byteEn)),
        isFirst: inStream.isFirst,
        isLast: inStream.isLast
    };
endfunction

function BusBitNum getFragEnBitNumByByteEnNum(ByteEnBitNum byteEnNum);
    return zeroExtend(byteEnNum) << 3;
endfunction

// TODO: check timing of the for loop
// TODO: refactor the for loop using case statement
function Maybe#(ByteEnBitNum) calcFragByteNumFromByteEnLeftAlign(ByteEn fragByteEn);
    let rightAlignedByteEn = reverseBits(fragByteEn);
    Maybe#(ByteEnBitNum) byteEnBitNum = tagged Invalid;
    // let step = valueOf(FRAG_MIN_VALID_BYTE_NUM);
    let step = 1;
    // Bool matched = False;
    for (
        Integer idx = 1;
        idx <= valueOf(DATA_BUS_BYTE_WIDTH);
        idx = idx + step
    ) begin
        if (rightAlignedByteEn == (fromInteger(1) << idx) - 1) begin
            byteEnBitNum = tagged Valid fromInteger(idx);
            // matched = True;
        end
    end
    // $display("matched=%b, rightAlignedByteEn=%h", matched, rightAlignedByteEn);
    return byteEnBitNum;
endfunction

// PMTU related

function Integer getPmtuLogValue(PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 :  8; // log2(256)
        IBV_MTU_512 :  9; // log2(512)
        IBV_MTU_1024: 10; // log2(1024)
        IBV_MTU_2048: 11; // log2(2048)
        IBV_MTU_4096: 12; // log2(4096)
    endcase;
endfunction

function PktLen calcPmtuLen(PMTU pmtu);
    return fromInteger(case (pmtu)
        IBV_MTU_256 :  256;
        IBV_MTU_512 :  512;
        IBV_MTU_1024: 1024;
        IBV_MTU_2048: 2048;
        IBV_MTU_4096: 4096;
    endcase);
endfunction

function Bool pktLenEqPMTU(PktLen pktLen, PMTU pmtu);
    let tmpPktLen = pktLen;
    let idx = getPmtuLogValue(pmtu);
    tmpPktLen[idx] = 0;
    return isZero(tmpPktLen);
endfunction

function Bool pktLenGtPMTU(PktLen pktLen, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            (pktLen[8] == 1 && !isZero(pktLen[7: 0]));
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            (pktLen[9] == 1 && !isZero(pktLen[8: 0]));
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            (pktLen[10] == 1 && !isZero(pktLen[9: 0]));
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            (pktLen[11] == 1 && !isZero(pktLen[10: 0]));
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            (pktLen[12] == 1 && !isZero(pktLen[11: 0]));
        end
    endcase;
endfunction

function PktFragNum calcFragNumByPMTU(PMTU pmtu) provisos(
    // Check DATA_BUS_BYTE_WIDTH must be power of 2
    Add#(TLog#(DATA_BUS_BYTE_WIDTH), 1, TLog#(TAdd#(1, DATA_BUS_BYTE_WIDTH)))
);
    let busByteWidth = valueOf(TLog#(DATA_BUS_BYTE_WIDTH));
    let pmtuWidth = getPmtuLogValue(pmtu);
    let shiftAmt = pmtuWidth - busByteWidth;
    return 1 << shiftAmt;
endfunction

// Header related

function HeaderByteEn genHeaderByteEn(HeaderByteNum headerLen);
    return reverseBits( (1 << headerLen) - 1);
endfunction

function Tuple2#(HeaderFragNum, ByteEnBitNum) calcHeaderFragNumAndLastFragValidByeNum(
    HeaderByteNum headerLen
) provisos(
    Add#(HEADER_FRAG_NUM_WIDTH, DATA_BUS_BYTE_NUM_WIDTH, HEADER_MAX_BYTE_NUM_WIDTH)
);
    let headerLastFragValidByteNum = calcLastFragValidByteNum(headerLen);
    HeaderFragNum headerFragNum = truncate(headerLen >> valueOf(DATA_BUS_BYTE_NUM_WIDTH));
    BusByteWidthMask lowerPart = truncate(headerLen);
    if (!isZeroR(lowerPart)) begin
        headerFragNum = headerFragNum + 1;
    end
    return tuple2(headerFragNum, headerLastFragValidByteNum);
endfunction

function HeaderMetaData genHeaderMetaData(
    HeaderByteNum headerLen,
    Bool hasPayload,
    Bool isEmptyHeader,
    RecvPacketSrcMacIpBufferIdx srcMacIpIdx
);
    let { headerFragNum, lastFragValidByteNum } =
        calcHeaderFragNumAndLastFragValidByeNum(headerLen);
    let headerMetaData = HeaderMetaData {
        headerLen: headerLen,
        headerFragNum: headerFragNum,
        lastFragValidByteNum: lastFragValidByteNum,
        hasPayload: hasPayload,
        isEmptyHeader : isEmptyHeader,
        srcMacIpIdx: srcMacIpIdx
    };
    return headerMetaData;
endfunction

function HeaderRDMA genHeaderRDMA(
    HeaderData headerData,
    HeaderByteNum headerLen,
    Bool hasPayload
);
    let headerMetaData = genHeaderMetaData(headerLen, hasPayload, False, 0);
    return HeaderRDMA {
        headerData     : headerData,
        headerByteNum  : headerLen,
        headerMetaData : headerMetaData
    };
endfunction

function HeaderRDMA genEmptyHeaderRDMA(Bool hasPayload);
    let emptyHeaderMetaData = HeaderMetaData {
        headerLen           : 0,
        headerFragNum       : 0,
        lastFragValidByteNum: 0,
        hasPayload          : hasPayload,
        isEmptyHeader       : True,
        srcMacIpIdx         : 0
    };
    return HeaderRDMA {
        headerData    : dontCareValue,
        headerByteNum  : 0,
        headerMetaData: emptyHeaderMetaData
    };
endfunction

function DataStream genFakeHeaderSingleBeatStreamForRawPacketReceiveProcessingRightAlign(ADDR va, RKEY rkey);
    let bth = BTH {
        trans    : TRANS_TYPE_DTLD_EXTENDED,
        opcode   : RDMA_WRITE_ONLY_WITH_IMMEDIATE,
        solicited: False,
        migReq   : unpack(0),
        padCnt   : unpack(0),
        tver     : unpack(0),
        pkey     : unpack(0),
        fecn     : unpack(0),
        becn     : unpack(0),
        resv6    : unpack(0),
        dqpn     : unpack(0),
        ackReq   : False,
        resv7    : unpack(0),
        psn      : 0
    };

    let reth = RETH {
        va: va,
        rkey: rkey,
        dlen: 0
    };

    let immDt = ImmDt {
        data: 0
    };
    let fakeHeaderStream = DataStream{
        data: zeroExtend({ pack(bth), pack(reth), pack(immDt) }),
        byteNum: fromInteger(calcHeaderLenByTransTypeAndRdmaOpCode(TRANS_TYPE_DTLD_EXTENDED, RDMA_WRITE_ONLY_WITH_IMMEDIATE)),
        isFirst: True,
        isLast: True
    };

    return fakeHeaderStream;
endfunction

function Tuple2#(HeaderByteNum, HeaderBitNum) calcHeaderInvalidFragByteAndBitNum(
    HeaderFragNum headerValidFragNum
);
    HeaderFragNum headerInvalidFragNum =
        fromInteger(valueOf(HEADER_MAX_FRAG_NUM)) - headerValidFragNum;
    HeaderByteNum headerInvalidFragByteNum =
        zeroExtend(headerInvalidFragNum) << valueOf(DATA_BUS_BYTE_NUM_WIDTH);
    HeaderBitNum headerInvalidFragBitNum =
        zeroExtend(headerInvalidFragNum) << valueOf(DATA_BUS_BIT_NUM_WIDTH);
    return tuple2(headerInvalidFragByteNum, headerInvalidFragBitNum);
endfunction

function Bool compareAccessTypeFlags(
    FlagsType#(MemAccessTypeFlag) flags1, FlagsType#(MemAccessTypeFlag) flags2
);
    return containFlags(flags1, flags2);
    // return !isZero(pack(flags1 & flags2));
endfunction

function Bool containAccessTypeFlag(
    FlagsType#(MemAccessTypeFlag) flags, MemAccessTypeFlag flag
);
    return containEnum(flags, flag);
    // return !isZero(pack(flags & enum2Flag(flag)));
endfunction

// BTH related

/*
111
110
101
100
011
010
001
000
*/
function Bool psnInRangeExclusive(PSN psn, PSN psnStart, PSN psnEnd);
    let result = False;
    let psnGtStart = psnStart < psn;
    let psnLtEnd = psn < psnEnd;

    if (msb(psnStart) == msb(psnEnd)) begin
        // PSN range no wrap around
        result = psnGtStart && psnLtEnd;
    end
    else begin
        // PSN range has wrap around max PSN
        result = (psnGtStart && msb(psnStart) == msb(psn)) ||
            (psnLtEnd && msb(psn) == msb(psnEnd));
    end
    return result;
endfunction

function PSN calcOldestValidPsn4RQ(PSN ePSN);
    // PSN - 2^23
    return { ~msb(ePSN), removeMSB(ePSN) };
endfunction

function PSN calcPsnDiff(PSN psnA, PSN psnB);
    return truncate({ 1'b1, psnA } - { 1'b0, psnB });
endfunction

function Bool isDefaultPKEY(PKEY pkey);
    return isAllOnesR(pkey);
endfunction

function IndexMR key2IndexMR(Bit#(nSz) key) provisos(
    Add#(KEY_WIDTH, 0, nSz)
);
    return unpack(truncateLSB(key));
endfunction

function ADDR addrAddPsnMultiplyPMTU(ADDR addr, PSN psn, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            { (addr[valueOf(ADDR_WIDTH)-1 : 8] + zeroExtend(psn)), addr[7 : 0] };
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            { (addr[valueOf(ADDR_WIDTH)-1 : 9] + zeroExtend(psn)), addr[8 : 0] };
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            { (addr[valueOf(ADDR_WIDTH)-1 : 10] + zeroExtend(psn)), addr[9 : 0] };
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            { (addr[valueOf(ADDR_WIDTH)-1 : 11] + zeroExtend(psn)), addr[10 : 0] };
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            { (addr[valueOf(ADDR_WIDTH)-1 : 12] + zeroExtend(psn)), addr[11 : 0] };
        end
    endcase;
endfunction

function Length lenSubtractPsnMultiplyPMTU(Length len, PSN psn, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 8] - psn), len[7 : 0] };
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9] - truncate(psn)), len[8 : 0] };
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10] - truncate(psn)), len[9 : 0] };
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11] - truncate(psn)), len[10 : 0] };
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12] - truncate(psn)), len[11 : 0] };
        end
    endcase;
endfunction

function Length lenAddPsnMultiplyPMTU(Length len, PSN psn, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 8] + psn), len[7 : 0] };
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9] + truncate(psn)), len[8 : 0] };
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10] + truncate(psn)), len[9 : 0] };
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11] + truncate(psn)), len[10 : 0] };
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            { (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12] + truncate(psn)), len[11 : 0] };
        end
    endcase;
endfunction

// Exact remaining length is not necessory, zero or non-zero is enough
function Length lenSubtractPktLen(Length len, PktLen pktLen, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9], (len[8 : 0] - truncate(pktLen)) };
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10], (len[9 : 0] - truncate(pktLen)) };
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11], (len[10 : 0] - truncate(pktLen)) };
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12], (len[11 : 0] - truncate(pktLen)) };
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 13], (len[12 : 0] - truncate(pktLen)) };
        end
    endcase;
endfunction

// Before add, len should be multiply of PMTU
function Length lenAddPktLen(Length len, PktLen pktLen, PMTU pmtu);
    let oneAsPSN = 1;
    return pktLenEqPMTU(pktLen, pmtu) ?
        lenAddPsnMultiplyPMTU(len, oneAsPSN, pmtu) :
        case (pmtu)
            IBV_MTU_256 : begin
                // 8 = log2(256)
                { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 8], pktLen[7 : 0] };
            end
            IBV_MTU_512 : begin
                // 9 = log2(512)
                { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9], pktLen[8 : 0] };
            end
            IBV_MTU_1024: begin
                // 10 = log2(1024)
                { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10], pktLen[9 : 0] };
            end
            IBV_MTU_2048: begin
                // 11 = log2(2048)
                { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11], pktLen[10 : 0] };
            end
            IBV_MTU_4096: begin
                // 12 = log2(4096)
                { len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12], pktLen[11 : 0] };
            end
        endcase;
endfunction

function Bool lenGtEqPktLen(Length len, PktLen pktLen, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 9)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9];
            (!isZeroR(lenHighPart) || (len[8 : 0] >= pktLen[8 : 0]));
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 10)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10];
            (!isZeroR(lenHighPart) || (len[9 : 0] >= pktLen[9 : 0]));
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 11)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11];
            (!isZeroR(lenHighPart) || (len[10 : 0] >= pktLen[10 : 0]));
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 12)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12];
            (!isZeroR(lenHighPart) || (len[11 : 0] >= pktLen[11 : 0]));
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 13)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 13];
            (!isZeroR(lenHighPart) || (len[12 : 0] >= pktLen[12 : 0]));
        end
    endcase;
endfunction

// In the modified version of RDMA protocol, the first packet's (e.g., WriteFirst, SendFirst) last byte will be aligned to 
// PMTU boundary, so this packet length will be less or equal to PMTU, which is different from the original RDMA spec.
function PktLen calcFirstPktLenFromAddrAndPMTU(ADDR va, PMTU pmtu);

    PktLen mask = case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            ((1 << 8) - 1);
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            ((1 << 9) - 1);
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            ((1 << 10) - 1);
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            ((1 << 11) - 1);
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            ((1 << 12) - 1);
        end
    endcase;

    PktLen addrLowPart = truncate(va) & mask;
    addrLowPart = calcPmtuLen(pmtu) - addrLowPart;

    return addrLowPart;
endfunction

/*
function Bool lenGtEqPktLen4LastOrOnlyPkt(Length len, PktLen pktLen, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            (len[8 : 0] >= pktLen[8 : 0]);
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            (len[9 : 0] >= pktLen[9 : 0]);
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            (len[10 : 0] >= pktLen[10 : 0]);
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            (len[11 : 0] >= pktLen[11 : 0]);
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            (len[12 : 0] >= pktLen[12 : 0]);
        end
    endcase;
endfunction

function Bool lenEqPktLen(Length len, PktLen pktLen, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 9)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9];
            (isZero(lenHighPart) && (len[8 : 0] == pktLen[8 : 0]));
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 10)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10];
            (isZero(lenHighPart) && (len[9 : 0] == pktLen[9 : 0]));
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 11)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11];
            (isZero(lenHighPart) && (len[10 : 0] == pktLen[10 : 0]));
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 12)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12];
            (isZero(lenHighPart) && (len[11 : 0] == pktLen[11 : 0]));
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 13)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 13];
            (isZero(lenHighPart) && (len[12 : 0] == pktLen[12 : 0]));
        end
    endcase;
endfunction
*/
function Bool lenGtEqPMTU(Length len, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 8)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 8];
            (!isZeroR(lenHighPart)); // truncate len[7 : 0]
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 9)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9];
            (!isZeroR(lenHighPart)); // truncate len[8 : 0]
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 10)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10];
            (!isZeroR(lenHighPart)); // truncate len[9 : 0]
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 11)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11];
            (!isZeroR(lenHighPart)); // truncate len[10 : 0]
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 12)) lenHighPart = truncateLSB(len); // [valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12];
            (!isZeroR(lenHighPart)); // truncate len[11 : 0]
        end
    endcase;
endfunction

function Bool lenGtEqPsnMultiplyPMTU(Length len, PSN psn, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            // 8 = log2(256)
            (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 8] >= psn);
        end
        IBV_MTU_512 : begin
            // 9 = log2(512)
            (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 9] >= psn);
        end
        IBV_MTU_1024: begin
            // 10 = log2(1024)
            (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 10] >= psn);
        end
        IBV_MTU_2048: begin
            // 11 = log2(2048)
            (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 11] >= psn);
        end
        IBV_MTU_4096: begin
            // 12 = log2(4096)
            (len[valueOf(RDMA_MAX_LEN_WIDTH)-1 : 12] >= psn);
        end
    endcase;
endfunction

function PktLen pktLenAddBusByteWidth(PktLen pktLen);
    Bit#(DATA_BUS_BYTE_NUM_WIDTH) lowerPart = truncate(pktLen);
    Bit#(TSub#(PKT_LEN_WIDTH, DATA_BUS_BYTE_NUM_WIDTH)) higherPart = truncateLSB(pktLen);
    return { (higherPart + 1), lowerPart };
endfunction

function Bool fragLenEqBusByteWidth(ByteEnBitNum fragLen);
    let tmpFragLen = fragLen;
    let idx = valueOf(DATA_BUS_BYTE_NUM_WIDTH);
    tmpFragLen[idx] = 0;
    return isZero(tmpFragLen);
endfunction

// Before add, pktLen should be multiply of DATA_BUS_BYTE_WIDTH
function PktLen pktLenAddFragLen(PktLen pktLen, ByteEnBitNum fragLen);
    Bit#(DATA_BUS_BYTE_NUM_WIDTH) lowerPartFragLen = truncate(fragLen);
    Bit#(TSub#(PKT_LEN_WIDTH, DATA_BUS_BYTE_NUM_WIDTH)) higherPartPktLen = truncateLSB(pktLen);
    return fragLenEqBusByteWidth(fragLen) ?
        pktLenAddBusByteWidth(pktLen) :
        { higherPartPktLen, lowerPartFragLen };
endfunction

function Maybe#(TransType) qpType2TransType(TypeQP qpt);
    return case (qpt)
        IBV_QPT_RC        : tagged Valid TRANS_TYPE_RC;
        IBV_QPT_UC        : tagged Valid TRANS_TYPE_UC;
        IBV_QPT_UD        : tagged Valid TRANS_TYPE_UD;
        IBV_QPT_XRC_RECV  ,
        IBV_QPT_XRC_SEND  : tagged Valid TRANS_TYPE_XRC;
        default           : tagged Invalid;
    endcase;
endfunction

function Bool transTypeMatchQpType(TransType tt, TypeQP qpt, Bool isRecvSide);
    return case (tt)
        TRANS_TYPE_CNP: True;
        TRANS_TYPE_RC : (qpt == IBV_QPT_RC);
        TRANS_TYPE_UC : (qpt == IBV_QPT_UC);
        TRANS_TYPE_UD : (qpt == IBV_QPT_UD);
        TRANS_TYPE_XRC: (
            (!isRecvSide && qpt == IBV_QPT_XRC_RECV) ||
            (isRecvSide && qpt == IBV_QPT_XRC_SEND)
        );
        default: False;
    endcase;
endfunction

function Bool qpNeedGenResp(TransType transType);
    return case (transType)
        TRANS_TYPE_RC ,
        TRANS_TYPE_XRC,
        TRANS_TYPE_RD : True;
        // TRANS_TYPE_UC ,
        // TRANS_TYPE_UD ,
        default       : False;
    endcase;
endfunction

function Bool isRawPktTypeQP(TypeQP qpType);
    return qpType == IBV_QPT_RAW_PACKET;
endfunction

function Bool isSupportedReqOpCodeRQ(TypeQP qpt, RdmaOpCode opcode);
    case (qpt)
        IBV_QPT_RC      : return case (opcode)
            SEND_FIRST                    ,
            SEND_MIDDLE                   ,
            SEND_LAST                     ,
            SEND_LAST_WITH_IMMEDIATE      ,
            SEND_ONLY                     ,
            SEND_ONLY_WITH_IMMEDIATE      ,
            SEND_LAST_WITH_INVALIDATE     ,
            SEND_ONLY_WITH_INVALIDATE     : True;
            RDMA_WRITE_FIRST              ,
            RDMA_WRITE_MIDDLE             ,
            RDMA_WRITE_LAST               ,
            RDMA_WRITE_LAST_WITH_IMMEDIATE,
            RDMA_WRITE_ONLY               ,
            RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
            RDMA_READ_REQUEST             : True;
            RDMA_READ_RESPONSE_FIRST      ,
            RDMA_READ_RESPONSE_MIDDLE     ,
            RDMA_READ_RESPONSE_LAST       ,
            RDMA_READ_RESPONSE_ONLY       : True;
            ACKNOWLEDGE                   ,
            ATOMIC_ACKNOWLEDGE            : True;
            COMPARE_SWAP                  ,
            FETCH_ADD                     : True;
            default                       : False;
        endcase;
        default: return False;
    endcase
endfunction

// function PAD calcPadCnt(Length len);
function PAD calcPadCnt(Bit#(nSz) len) provisos(
    Add#(PAD_WIDTH, anysize, nSz)
);
    // PadMask padMask = maxBound;
    // PAD tmpCnt = truncate(len) & padMask;
    PAD tmpCnt = truncate(len);
    PAD padCnt = (1 << valueOf(PAD_WIDTH)) - tmpCnt;
    return padCnt;
endfunction

function Tuple2#(TransType, RdmaOpCode) extractTranTypeAndRdmaOpCode(
    Bit#(nSz) inputData
);
    TransType transType = unpack(inputData[
        valueOf(nSz)-1 :
        valueOf(nSz) - valueOf(TRANS_TYPE_WIDTH)
    ]);
    RdmaOpCode rdmaOpCode = unpack(inputData[
        valueOf(nSz) - valueOf(TRANS_TYPE_WIDTH) - 1 :
        valueOf(nSz) - valueOf(TRANS_TYPE_WIDTH) - valueOf(RDMA_OPCODE_WIDTH)
    ]);

    return tuple2(transType, rdmaOpCode);
endfunction



function AETH extractAETH(HeaderData headerData);
    let aeth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_WIDTH)
    ]);
    return aeth;
endfunction


function NRETH extractNRETH(HeaderData headerData);
    let nreth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_WIDTH) -1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_WIDTH) - valueOf(NRETH_WIDTH)
    ]);
    return nreth;
endfunction



function AtomicAckEth extractAtomicAckEth(HeaderData headerData);
    let atomicAckEth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_WIDTH) -1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(AETH_WIDTH) - valueOf(ATOMIC_ACK_ETH_WIDTH)
    ]);
    return atomicAckEth;
endfunction

function XRCETH extractXRCETH(HeaderData headerData);
    let xrceth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH)
    ]);
    return xrceth;
endfunction



function LETH extractLETH(HeaderData headerData, TransType transType);
    let reth = case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH) - valueOf(LETH_WIDTH)
        ]);
        default: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) - valueOf(LETH_WIDTH)
        ]);
    endcase;
    return reth;
endfunction

function AtomicEth extractAtomicEth(HeaderData headerData, TransType transType);
    let atomicEth = case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(ATOMIC_ETH_WIDTH)
        ]);
        default: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(ATOMIC_ETH_WIDTH)
        ]);
    endcase;
    return atomicEth;
endfunction

function DETH extractDETH(HeaderData headerData);
    let deth = unpack(headerData[
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
        valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(DETH_WIDTH)
    ]);
    return deth;
endfunction

function Bool isAlignedAtomicAddr(ADDR atomicAddr);
    AtomicAddrByteAlignment alignment = truncate(atomicAddr);
    return isZero(alignment);
endfunction

function ImmDt extractImmDt(HeaderData headerData, RdmaOpCode opcode, TransType transType);

    return case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(RETH_WIDTH) - valueOf(IMM_DT_WIDTH)
        ]);
        default: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(RETH_WIDTH) - valueOf(IMM_DT_WIDTH)
        ]);
    endcase;

endfunction

function IETH extractIETH(HeaderData headerData, TransType transType);
    let ieth = case (transType)
        TRANS_TYPE_XRC: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(XRCETH_WIDTH) - valueOf(IETH_WIDTH)
        ]);
        default: unpack(headerData[
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) -1 :
            valueOf(HEADER_MAX_DATA_WIDTH) - valueOf(BTH_WIDTH) - valueOf(IETH_WIDTH)
        ]);
    endcase;
    return ieth;
endfunction

function Bool isCongestionNotificationPkt(BTH bth);
    return { pack(bth.trans), pack(bth.opcode) } == fromInteger(valueOf(ROCE_CNP));
endfunction

function Bool isAckRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        ACKNOWLEDGE             : True;
        default                 : False;
    endcase;
endfunction

function Bool isFirstRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_FIRST              ,
        RDMA_WRITE_FIRST        ,
        RDMA_READ_RESPONSE_FIRST: True;

        default                 : False;
    endcase;
endfunction

function Bool isMiddleRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_MIDDLE              ,
        RDMA_WRITE_MIDDLE        ,
        RDMA_READ_RESPONSE_MIDDLE: True;

        default                  : False;
    endcase;
endfunction

function Bool isLastRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_LAST                     ,
        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_LAST_WITH_INVALIDATE     ,

        RDMA_WRITE_LAST               ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,

        RDMA_READ_RESPONSE_LAST       : True;

        default                       : False;
    endcase;
endfunction

function Bool isOnlyRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_ONLY                     ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        SEND_ONLY_WITH_INVALIDATE     ,

        RDMA_WRITE_ONLY               ,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE,

        RDMA_READ_REQUEST             ,
        COMPARE_SWAP                  ,
        FETCH_ADD                     ,

        RDMA_READ_RESPONSE_ONLY       ,

        ACKNOWLEDGE                   ,
        ATOMIC_ACKNOWLEDGE            : True;

        default                       : False;
    endcase;
endfunction

function Bool isFirstOrOnlyRdmaOpCode(RdmaOpCode opcode);
    return isFirstRdmaOpCode(opcode) || isOnlyRdmaOpCode(opcode);
endfunction

function Bool isFirstOrMiddleRdmaOpCode(RdmaOpCode opcode);
    return isFirstRdmaOpCode(opcode) || isMiddleRdmaOpCode(opcode);
endfunction

function Bool isLastOrOnlyRdmaOpCode(RdmaOpCode opcode);
    return isLastRdmaOpCode(opcode) || isOnlyRdmaOpCode(opcode);
endfunction

function Bool isMiddleOrLastRdmaOpCode(RdmaOpCode opcode);
    return isMiddleRdmaOpCode(opcode) || isLastRdmaOpCode(opcode);
endfunction

function Bool isSendReqRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_FIRST               ,
        SEND_MIDDLE              ,
        SEND_LAST                ,
        SEND_LAST_WITH_IMMEDIATE ,
        SEND_ONLY                ,
        SEND_ONLY_WITH_IMMEDIATE ,
        SEND_LAST_WITH_INVALIDATE,
        SEND_ONLY_WITH_INVALIDATE: True;
        default                  : False;
    endcase;
endfunction

function Bool isWriteReqRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        RDMA_WRITE_FIRST              ,
        RDMA_WRITE_MIDDLE             ,
        RDMA_WRITE_LAST               ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY               ,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
        default                       : False;
    endcase;
endfunction

function Bool isWriteImmReqRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
        default                       : False;
    endcase;
endfunction

function Bool isSendWriteImmReqRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        SEND_FIRST                    ,
        SEND_MIDDLE                   ,
        SEND_LAST                     ,
        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_ONLY                     ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        SEND_LAST_WITH_INVALIDATE     ,
        SEND_ONLY_WITH_INVALIDATE     ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
        default                       : False;
    endcase;
endfunction

function Bool isReadReqRdmaOpCode(RdmaOpCode opcode);
    return opcode == RDMA_READ_REQUEST;
endfunction

function Bool isAtomicReqRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        COMPARE_SWAP,
        FETCH_ADD   : True;
        default     : False;
    endcase;
endfunction

function Bool isReadRespRdmaOpCode(RdmaOpCode opcode);
    return case (opcode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_MIDDLE,
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  : True;
        default                  : False;
    endcase;
endfunction

function Bool isRdmaRespOpCode(RdmaOpCode opcode);
    return case (opcode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_MIDDLE,
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  ,
        ACKNOWLEDGE              ,
        ATOMIC_ACKNOWLEDGE       : True;
        default                  : False;
    endcase;
endfunction

function Bool rdmaRespHasAETH(RdmaOpCode opcode);
    return case (opcode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  ,
        ACKNOWLEDGE              ,
        ATOMIC_ACKNOWLEDGE       : True;
        default                  : False;
    endcase;
endfunction

function Bool isAtomicRespRdmaOpCode(RdmaOpCode opcode);
    return opcode == ATOMIC_ACKNOWLEDGE;
endfunction

function Bool rdmaRespNeedDmaWrite(RdmaOpCode opcode);
    return case (opcode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_MIDDLE,
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  ,
        ATOMIC_ACKNOWLEDGE       : True;
        default                  : False;
    endcase;
endfunction

function Bool rdmaReqNeedDmaWrite(RdmaOpCode opcode);
    return case (opcode)
        RDMA_WRITE_FIRST              ,
        RDMA_WRITE_MIDDLE             ,
        RDMA_WRITE_LAST               ,
        RDMA_WRITE_ONLY               ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
        default                       : False;
    endcase;
endfunction

function Bool rdmaReqHasRETH(RdmaOpCode opcode);
    return case (opcode)
        RDMA_WRITE_FIRST              ,
        RDMA_WRITE_ONLY               ,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE,
        RDMA_READ_REQUEST             : True;
        default                       : False;
    endcase;
endfunction

function Bool rdmaReqHasImmDt(RdmaOpCode opcode);
    return case (opcode)
        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: True;
        default                       : False;
    endcase;
endfunction

function Bool rdmaReqHasIETH(RdmaOpCode opcode);
    return case (opcode)
        SEND_LAST_WITH_INVALIDATE,
        SEND_ONLY_WITH_INVALIDATE: True;
        default                  : False;
    endcase;
endfunction

function RdmaRespType getRdmaRespType(RdmaOpCode opcode, AETH aeth);
    case (opcode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_MIDDLE,
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  ,
        ATOMIC_ACKNOWLEDGE       : return RDMA_RESP_NORMAL;
        ACKNOWLEDGE              : case (aeth.code)
            AETH_CODE_ACK: return RDMA_RESP_NORMAL;
            AETH_CODE_RNR: return RDMA_RESP_RETRY;
            AETH_CODE_NAK: return case (aeth.value)
                zeroExtend(pack(AETH_NAK_SEQ_ERR)): RDMA_RESP_RETRY;
                zeroExtend(pack(AETH_NAK_INV_REQ)),
                zeroExtend(pack(AETH_NAK_RMT_ACC)),
                zeroExtend(pack(AETH_NAK_RMT_OP)) ,
                zeroExtend(pack(AETH_NAK_INV_RD)) : RDMA_RESP_ERROR;
                default                           : RDMA_RESP_UNKNOWN;
            endcase;
            // AETH_CODE_RSVD
            default: return RDMA_RESP_UNKNOWN;
        endcase
        default: return RDMA_RESP_UNKNOWN;
    endcase
endfunction

function RetryReason getRetryReasonFromAETH(AETH aeth);
    return case (aeth.code)
        AETH_CODE_RNR: RETRY_REASON_RNR;
        AETH_CODE_NAK: (
            (aeth.value == zeroExtend(pack(AETH_NAK_SEQ_ERR))) ?
                RETRY_REASON_SEQ_ERR : RETRY_REASON_NOT_RETRY
        );
        default: RETRY_REASON_NOT_RETRY;
    endcase;
endfunction

function Bool checkNormalRespOpCodeSeqSQ(
    RdmaOpCode preOpCode, RdmaOpCode curOpCode
);
    return case (preOpCode)
        RDMA_READ_RESPONSE_FIRST ,
        RDMA_READ_RESPONSE_MIDDLE: (
            curOpCode == RDMA_READ_RESPONSE_MIDDLE ||
            curOpCode == RDMA_READ_RESPONSE_LAST
        );
        RDMA_READ_RESPONSE_LAST  ,
        RDMA_READ_RESPONSE_ONLY  ,
        ACKNOWLEDGE              ,
        ATOMIC_ACKNOWLEDGE       : True;
        default                  : False;
    endcase;
endfunction

function Bool checkNormalReqOpCodeSeqRQ(
    RdmaOpCode preOpCode, RdmaOpCode curOpCode
);
    case (preOpCode)
        SEND_FIRST                    ,
        SEND_MIDDLE                   : return case (curOpCode)
                                            SEND_MIDDLE              ,
                                            SEND_LAST                ,
                                            SEND_LAST_WITH_IMMEDIATE ,
                                            SEND_LAST_WITH_INVALIDATE: True;
                                            default                  : False;
                                        endcase;
        SEND_LAST                     ,
        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_LAST_WITH_INVALIDATE     ,
        SEND_ONLY                     ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        SEND_ONLY_WITH_INVALIDATE     : return True;
        RDMA_WRITE_FIRST              ,
        RDMA_WRITE_MIDDLE             : return case (curOpCode)
                                            RDMA_WRITE_MIDDLE             ,
                                            RDMA_WRITE_LAST               ,
                                            RDMA_WRITE_LAST_WITH_IMMEDIATE: True;
                                            default                       : False;
                                        endcase;
        RDMA_WRITE_LAST               ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY               ,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE,
        RDMA_READ_REQUEST             ,
        COMPARE_SWAP                  ,
        FETCH_ADD                     : return True;

        default                       : return False;
    endcase
endfunction

// WorkReq related

function Bool containWorkReqFlag(
    FlagsType#(WorkReqSendFlag) flags, WorkReqSendFlag flag
);
    return containEnum(flags, flag);
    // return !isZero(pack(flags & enum2Flag(flag)));
endfunction

function ResiduePMTU truncateByPMTU(Bit#(nSz) bits, PMTU pmtu) provisos(
    Add#(MAX_PMTU_WIDTH, anysizeJ, nSz),
    Add#(TSub#(MAX_PMTU_WIDTH, 1), anysizeK, nSz),
    Add#(TSub#(MAX_PMTU_WIDTH, 2), anysizeL, nSz),
    Add#(TSub#(MAX_PMTU_WIDTH, 3), anysizeM, nSz),
    Add#(TSub#(MAX_PMTU_WIDTH, 4), anysizeN, nSz)
);
    return case (pmtu)
        IBV_MTU_256 : begin
            Bit#(TSub#(MAX_PMTU_WIDTH, 4)) residue = truncate(bits); // [7 : 0]
            zeroExtend(residue);
        end
        IBV_MTU_512 : begin
            Bit#(TSub#(MAX_PMTU_WIDTH, 3)) residue = truncate(bits); // [8 : 0]
            zeroExtend(residue);
        end
        IBV_MTU_1024: begin
            Bit#(TSub#(MAX_PMTU_WIDTH, 2)) residue = truncate(bits); // [9 : 0]
            zeroExtend(residue);
        end
        IBV_MTU_2048: begin
            Bit#(TSub#(MAX_PMTU_WIDTH, 1)) residue = truncate(bits); // [10 : 0]
            zeroExtend(residue);
        end
        IBV_MTU_4096: begin
            Bit#(MAX_PMTU_WIDTH) residue = truncate(bits); // [11 : 0]
            zeroExtend(residue);
        end
    endcase;
endfunction

// TODO: remove this function, it should consider start address to calculate PktNum
// The returned PktNum might be zero or one less than actual PktNum
function Tuple2#(PktNum, ResiduePMTU) truncateLenByPMTU(Length len, PMTU pmtu);
    return case (pmtu)
        IBV_MTU_256 : begin
            Bit#(8) residue = truncate(len); // [7 : 0]
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 8)) truncatedLen = truncateLSB(len);
            tuple2(zeroExtend(truncatedLen), zeroExtend(residue));
        end
        IBV_MTU_512 : begin
            Bit#(9) residue = truncate(len); // [8 : 0]
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 9)) truncatedLen = truncateLSB(len);
            tuple2(zeroExtend(truncatedLen), zeroExtend(residue));
        end
        IBV_MTU_1024: begin
            Bit#(10) residue = truncate(len); // [9 : 0]
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 10)) truncatedLen = truncateLSB(len);
            tuple2(zeroExtend(truncatedLen), zeroExtend(residue));
        end
        IBV_MTU_2048: begin
            Bit#(11) residue = truncate(len); // [10 : 0]
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 11)) truncatedLen = truncateLSB(len);
            tuple2(zeroExtend(truncatedLen), zeroExtend(residue));
        end
        IBV_MTU_4096: begin
            Bit#(12) residue = truncate(len); // [11 : 0]
            Bit#(TSub#(RDMA_MAX_LEN_WIDTH, 12)) truncatedLen = truncateLSB(len);
            tuple2(zeroExtend(truncatedLen), zeroExtend(residue));
        end
    endcase;
endfunction

function Tuple2#(PSN, PSN) calcNextAndEndPSN(
    PSN startPSN, PktNum pktNum, Bool isOnlyPkt, PMTU pmtu
);
    let startPlusOne = startPSN + 1;
    // In case pktNum is zero
    PSN nextPSN = isOnlyPkt ? startPlusOne : truncate(zeroExtend(startPSN) + pktNum);
    PSN endPSN = isOnlyPkt ? startPSN : (nextPSN - 1);
    return tuple2(nextPSN, endPSN);
endfunction

function Bool workReqHasAckReq(WorkReq wr);
    return containWorkReqFlag(wr.flags, IBV_SEND_SIGNALED);
endfunction

function Bool workReqRequireAck(WorkReq wr);
    return workReqHasAckReq(wr) || isReadOrAtomicWorkReq(wr.opcode);
endfunction

function Bool workReqNeedDmaReadSQ(WorkReq wr);
    return case (wr.opcode)
        IBV_WR_RDMA_WRITE         ,
        IBV_WR_RDMA_WRITE_WITH_IMM,
        IBV_WR_SEND               ,
        IBV_WR_SEND_WITH_IMM      ,
        IBV_WR_SEND_WITH_INV      : !isZeroR(wr.len);
        default                   : False;
    endcase;
endfunction

function Bool workReqNeedDmaWriteSQ(WorkReq wr);
    return case (wr.opcode)
        IBV_WR_RDMA_READ           : !isZeroR(wr.len);
        IBV_WR_ATOMIC_CMP_AND_SWP  ,
        IBV_WR_ATOMIC_FETCH_AND_ADD: True;
        default                    : False;
    endcase;
endfunction

function Bool workReqHasPayload(WorkReq wr);
    return !(isZeroR(wr.len) || isReadOrAtomicWorkReq(wr.opcode));
endfunction

function Bool workReqNeedWorkCompSQ(WorkReq wr);
    return
        containWorkReqFlag(wr.flags, IBV_SEND_SIGNALED) ||
        isReadOrAtomicWorkReq(wr.opcode);
endfunction

function Bool workReqHasComp(WorkReqOpCode opcode);
    return opcode == IBV_WR_ATOMIC_CMP_AND_SWP;
endfunction

function Bool workReqHasSwap(WorkReqOpCode opcode);
    return case (opcode)
        IBV_WR_ATOMIC_CMP_AND_SWP  ,
        IBV_WR_ATOMIC_FETCH_AND_ADD: True;
        default: False;
    endcase;
endfunction

function Bool isSendWorkReq(WorkReqOpCode opcode);
    return case (opcode)
        IBV_WR_SEND         ,
        IBV_WR_SEND_WITH_IMM,
        IBV_WR_SEND_WITH_INV: True;
        default             : False;
    endcase;
endfunction

function Bool isReadWorkReq(WorkReqOpCode opcode);
    return opcode == IBV_WR_RDMA_READ;
endfunction

function Bool isReadRespWorkReq(WorkReqOpCode opcode);
    return opcode == IBV_WR_RDMA_READ_RESP;
endfunction

function Bool isAtomicWorkReq(WorkReqOpCode opcode);
    return workReqHasSwap(opcode);
endfunction

function Bool isReadOrAtomicWorkReq(WorkReqOpCode opcode);
    return case (opcode)
        IBV_WR_RDMA_READ,
        IBV_WR_ATOMIC_CMP_AND_SWP,
        IBV_WR_ATOMIC_FETCH_AND_ADD: True;
        default: False;
    endcase;
endfunction

function Bool workReqNeedRecvReq(WorkReqOpCode opcode);
    return case (opcode)
        IBV_WR_SEND               ,
        IBV_WR_SEND_WITH_IMM      ,
        IBV_WR_SEND_WITH_INV      ,
        IBV_WR_RDMA_WRITE_WITH_IMM: True;
        default                   : False;
    endcase;
endfunction

function Bool workReqHasImmDt(WorkReqOpCode opcode);
    return case (opcode)
        IBV_WR_RDMA_WRITE_WITH_IMM,
        IBV_WR_SEND_WITH_IMM: True;
        default: False;
    endcase;
endfunction

function Bool workReqHasInv(WorkReqOpCode opcode);
    return opcode == IBV_WR_SEND_WITH_INV;
endfunction

function PendingWorkReq genNewPendingWorkReq(WorkReq wr) = PendingWorkReq {
    wr: wr,
    startPSN: tagged Invalid,
    endPSN: tagged Invalid,
    pktNum: tagged Invalid,
    isOnlyReqPkt: tagged Invalid
};

/*
module mkConnectPendingWorkReqPipeOut2PendingWorkReqQ#(
    PipeOut#(PendingWorkReq) pipeIn, FIFOF#(PendingWorkReq) pendingWorkReqBufQ
)(Empty);
    rule connect;
        let pendingWR = pipeIn.first;
        pendingWorkReqBufQ.enq(pendingWR);
        pipeIn.deq;

        // $display(
        //     // "time=%0t: fill pendingWR=", $time, fshow(pendingWR)
        //     "time=%0t: fill pendingWR.wr.id=%h", $time, pendingWR.wr.id
        // );
    endrule
endmodule
*/

// suppose LKey == RKey
function IndexMR   wr2IndexMR(WorkReq wr)    = lkey2IndexMR(wr.lkey);
function IndexMR   lkey2IndexMR(LKEY lkey)   = unpack(truncateLSB(lkey));
function IndexMR   rkey2IndexMR(RKEY rkey)   = unpack(truncateLSB(rkey));
function KeyPartMR lkey2KeyPartMR(LKEY lkey) = unpack(truncate(lkey));
function KeyPartMR rkey2KeyPartMR(RKEY rkey) = unpack(truncate(rkey));
function RKEY      rkeyFromKeyAndIndexPart(IndexMR idx, KeyPartMR key) = {pack(idx), key};
function LKEY      lkeyFromKeyAndIndexPart(IndexMR idx, KeyPartMR key) = {pack(idx), key};



// WorkComp related

// TODO: support multiple WC flags
function Bool containWorkCompFlag(
    WorkCompFlags flag1, WorkCompFlags flag2
);
    return flag1 == flag2;
endfunction

function Maybe#(WorkCompStatus) pktStatus2WorkCompStatusSQ(
    PktVeriStatus pktStatus
);
    return case (pktStatus)
        PKT_ST_VALID  : tagged Valid IBV_WC_SUCCESS;
        PKT_ST_LEN_ERR: tagged Valid IBV_WC_LOC_LEN_ERR;
        // PKT_ST_QP_ACC_ERR: tagged Valid IBV_WC_LOC_ACCESS_ERR;
        default       : tagged Invalid;
    endcase;
endfunction

function Maybe#(WorkCompOpCode) workReqOpCode2WorkCompOpCode4SQ(WorkReqOpCode wrOpCode);
    return case (wrOpCode)
        IBV_WR_RDMA_WRITE          : tagged Valid IBV_WC_RDMA_WRITE;
        IBV_WR_RDMA_WRITE_WITH_IMM : tagged Valid IBV_WC_RDMA_WRITE;
        IBV_WR_SEND                : tagged Valid IBV_WC_SEND;
        IBV_WR_SEND_WITH_IMM       : tagged Valid IBV_WC_SEND;
        IBV_WR_RDMA_READ           : tagged Valid IBV_WC_RDMA_READ;
        IBV_WR_ATOMIC_CMP_AND_SWP  : tagged Valid IBV_WC_COMP_SWAP;
        IBV_WR_ATOMIC_FETCH_AND_ADD: tagged Valid IBV_WC_FETCH_ADD;
        IBV_WR_LOCAL_INV           : tagged Valid IBV_WC_LOCAL_INV;
        IBV_WR_BIND_MW             : tagged Valid IBV_WC_BIND_MW;
        IBV_WR_SEND_WITH_INV       : tagged Valid IBV_WC_SEND;
        IBV_WR_TSO                 : tagged Valid IBV_WC_TSO;
        // IBV_WR_DRIVER1             : tagged Valid IBV_WC_DRIVER1;
        default                    : tagged Invalid;
    endcase;
endfunction

function Maybe#(WorkCompStatus) genErrWorkCompStatusFromAethSQ(AETH aeth);
    case (aeth.code)
        // AETH_CODE_ACK: return tagged Valid IBV_WC_SUCCESS;
        // AETH_CODE_RNR: return tagged Valid IBV_WC_RNR_RETRY_EXC_ERR;
        AETH_CODE_NAK: return case (aeth.value)
            // zeroExtend(pack(AETH_NAK_SEQ_ERR)): tagged Valid IBV_WC_RETRY_EXC_ERR;
            zeroExtend(pack(AETH_NAK_INV_REQ)): tagged Valid IBV_WC_REM_INV_REQ_ERR;
            zeroExtend(pack(AETH_NAK_RMT_ACC)): tagged Valid IBV_WC_REM_ACCESS_ERR;
            zeroExtend(pack(AETH_NAK_RMT_OP)) : tagged Valid IBV_WC_REM_OP_ERR;
            zeroExtend(pack(AETH_NAK_INV_RD)) : tagged Valid IBV_WC_REM_INV_RD_REQ_ERR;
            default                           : tagged Invalid;
        endcase;
        // AETH_CODE_RSVD
        default: return tagged Invalid;
    endcase
endfunction

function Maybe#(WorkCompOpCode) rdmaOpCode2WorkCompOpCode4RQ(RdmaOpCode opcode);
    return case (opcode)
        SEND_FIRST                    ,
        SEND_MIDDLE                   ,
        SEND_LAST                     ,
        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_ONLY                     ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        SEND_LAST_WITH_INVALIDATE     ,
        SEND_ONLY_WITH_INVALIDATE     : tagged Valid IBV_WC_RECV;

        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: tagged Valid IBV_WC_RECV_RDMA_WITH_IMM;

        default                       : tagged Invalid;
    endcase;
endfunction

function WorkCompFlags rdmaOpCode2WorkCompFlagsRQ(RdmaOpCode opcode);
    return case (opcode)
        SEND_FIRST                    ,
        SEND_MIDDLE                   ,
        SEND_LAST                     ,
        SEND_ONLY                     : IBV_WC_NO_FLAGS;

        SEND_LAST_WITH_IMMEDIATE      ,
        SEND_ONLY_WITH_IMMEDIATE      ,
        RDMA_WRITE_LAST_WITH_IMMEDIATE,
        RDMA_WRITE_ONLY_WITH_IMMEDIATE: IBV_WC_WITH_IMM;

        SEND_LAST_WITH_INVALIDATE     ,
        SEND_ONLY_WITH_INVALIDATE     : IBV_WC_WITH_INV;

        default                       : IBV_WC_NO_FLAGS;
    endcase;
endfunction

// PipeOut related

module mkBufferN#(
    Integer depth, PipeOut#(anytype) pipeIn
)(PipeOut#(anytype)) provisos(Bits#(anytype, tSz));
    let resultPipeOut <- mkBuffer_n(depth, pipeIn);
    return resultPipeOut;
endmodule

module mkPipeOutMux#(
    Bool sel, PipeOut#(anytype) pipeIn1, PipeOut#(anytype) pipeIn2
)(PipeOut#(anytype)) provisos(Bits#(anytype, tSz));
    FIFOF#(anytype) pipeMuxOutQ <- mkFIFOF;
/*
    rule debug;
        $display(
            "time=%0t: mkPipeOutMux debug", $time,
            ", sel=", fshow(sel),
            ", pipeIn1.notEmpty=", fshow(pipeIn1.notEmpty),
            ", pipeIn2.notEmpty=", fshow(pipeIn2.notEmpty)
        );
    endrule
*/
    rule outputPipeIn1 if (sel);
        pipeMuxOutQ.enq(pipeIn1.first);
        pipeIn1.deq;
        // $display("time=%0t:", $time, " mkPipeOutMux, sel=", fshow(sel));
    endrule

    rule outputPipeIn2 if (!sel);
        pipeMuxOutQ.enq(pipeIn2.first);
        pipeIn2.deq;
        // $display("time=%0t:", $time, " mkPipeOutMux, sel=", fshow(sel));
    endrule

    return toPipeOut(pipeMuxOutQ);
endmodule
/*
module mkPipeOutBuffer#(PipeOut#(anytype) pipeIn
)(PipeOut#(anytype)) provisos(Bits#(anytype, anysize));
    FIFOF#(anytype) bufferQ <- mkFIFOF;
    mkConnection(toPut(bufferQ), toGet(pipeIn));
    return toPipeOut(bufferQ);
endmodule

function PipeOut#(anytype) muxPipeOut(
    Bool sel, PipeOut#(anytype) pipeIn1, PipeOut#(anytype) pipeIn2
);
    PipeOut#(anytype) resultPipeOut = interface PipeOut;
        method anytype first();
            return sel ? pipeIn1.first : pipeIn2.first;
        endmethod

        method Action deq();
            if (sel) begin
                pipeIn1.deq;
            end
            else begin
                pipeIn2.deq;
            end
        endmethod

        method Bool notEmpty();
            return sel ? pipeIn1.notEmpty : pipeIn2.notEmpty;
        endmethod
    endinterface;

    return resultPipeOut;
endfunction

function Tuple2#(PipeOut#(anytype), PipeOut#(anytype)) deMuxPipeOut(
    Bool sel, PipeOut#(anytype) pipeIn
);
    PipeOut#(anytype) p1 = interface PipeOut;
        method anytype first() if (sel);
            return pipeIn.first;
        endmethod
        method Action deq() if (sel);
            pipeIn.deq;
        endmethod
        method Bool notEmpty() if (sel);
            return pipeIn.notEmpty;
        endmethod
    endinterface;

    PipeOut#(anytype) p2 = interface PipeOut;
        method anytype first() if (!sel);
            return pipeIn.first;
        endmethod
        method Action deq() if (!sel);
            pipeIn.deq;
        endmethod
        method Bool notEmpty() if (!sel);
            return pipeIn.notEmpty;
        endmethod
    endinterface;

    return tuple2(p1, p2);
endfunction
*/
function Rules genEmptyPipeOutRule(
    PipeOut#(anytype) inputPipeOut, String assertMsg
);
    return (
        rules
            rule checkEmptyPipeIn;
                immAssert(
                    !inputPipeOut.notEmpty,
                    assertMsg,
                    $format(
                        "inputPipeOut.notEmpty=",
                        fshow(inputPipeOut.notEmpty),
                        " should be empty"
                    )
                );
            endrule
        endrules
    );
endfunction

// PayloadConsumer related

// TODO: check discard duplicate or ghost reponses has
// no response from PayloadConsumer will not incur bugs.
function ActionValue#(PayloadConReq) genDiscardPayloadReq(
    PktFragNum fragNum,
    PktLen len
);
    actionvalue
        immAssert(
            !isZero(fragNum),
            "fragNum non-zero assertion @ genDiscardPayloadReq()",
            $format("fragNum=%0d", fragNum, " should be non-zero")
        );
        let discardReq = PayloadConReq {
            fragNum    : fragNum,
            consumeInfo: tagged DiscardPayloadInfo DmaWriteMetaData {
                startAddr: 0,
                len      : len     
            }
        };
        return discardReq;
    endactionvalue
endfunction

module mkConnectionWithAction#(
    Get#(anytype) getIn,
    Put#(anytype) putOut,
    function Action connectAction(anytype connectVal)
)(Empty) provisos(
    FShow#(anytype),
    Bits#(anytype, anysize)
    // Bits#(DataStream, anysize)
);
    Reg#(BTH) bthReg <- mkRegU;

    rule connect;
        let data <- getIn.get;
        putOut.put(data);
        connectAction(data);

    endrule
endmodule


function Bit#(width) swapEndian(Bit#(width) data) provisos(Mul#(8, byteNum, width));
    Vector#(byteNum, Bit#(BYTE_WIDTH)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction

function Bit#(width) swapEndianBit(Bit#(width) data) provisos(Mul#(1, byteNum, width));
    Vector#(byteNum, Bit#(1)) dataVec = unpack(data);
    return pack(reverse(dataVec));
endfunction


function DataStream reverseStream(DataStream st);
    st.data = swapEndian(st.data);
    return st;
endfunction

function DataStreamEn reverseStreamEnAndData(DataStreamEn st);
    st.data = swapEndian(st.data);
    st.byteEn = swapEndianBit(st.byteEn);
    return st;
endfunction

function DataStreamEn reverseStreamEnOnly(DataStreamEn st);
    st.byteEn = swapEndianBit(st.byteEn);
    return st;
endfunction




interface RingbufStorage#(type t_data, type t_idx);
    interface Get#(t_idx) allocSlotIdx;
    interface Put#(Tuple2#(t_idx, t_data)) saveData;
    interface Server#(Tuple2#(t_idx, Bool), t_data) readFragSrv;
endinterface


module mkRingbufStorage#(String name, Bool checkOverflow)(RingbufStorage#(t_data, t_idx)) provisos (
    Bits#(t_data, sz_data),
    Bits#(t_idx, sz_idx),
    Alias#(Bit#(sz_idxNoGuard), t_idxNoGuard),
    Add#(sz_idxNoGuard, 1, sz_idx),
    FShow#(t_idx),
    Bitwise#(t_idx),
    Eq#(t_idx),
    Arith#(t_idx)
);
    BypassServer#(Tuple2#(t_idx, Bool), t_data) readFragSrvInst    <- mkBypassServer(sprintf("%s readFragSrvInst", name));

    BRAM_Configure cfg = defaultValue;
    BRAM2Port#(t_idxNoGuard, t_data) bramBuffer <- mkBRAM2Server(cfg);
    Reg#(t_idx) idxGeneratorReg <- mkReg(unpack(0));
    Reg#(t_idx) lastConsumeIdxReg <- mkReg(unpack(0));

    FIFOF#(t_idx) allocedIdxQ <- mkFIFOF;


    rule handleAllocIdx;
        allocedIdxQ.enq(idxGeneratorReg);
        idxGeneratorReg <= idxGeneratorReg + 1;
    endrule

    rule handleReadReq;
        let {addr, isOnlyUpadteLastConsumeIndex} <- readFragSrvInst.getReq;
        lastConsumeIdxReg <= addr;
        if (!isOnlyUpadteLastConsumeIndex) begin
            bramBuffer.portB.request.put(BRAMRequest{
                write: False,
                responseOnWrite:False,
                address: truncate(pack(addr)),
                datain: ?
            });
        end

        $display(
            "time=%0t:", $time, " RingbufStorage new output entry",
            ", name=", fshow(name),
            ", reqIdx=", fshow(addr),
            ", lastConsumeIdxReg=", fshow(lastConsumeIdxReg),
            ", isOnlyUpadteLastConsumeIndex=", fshow(isOnlyUpadteLastConsumeIndex)
        );

    endrule

    rule outputReadResp;
        let resp <- bramBuffer.portB.response.get;
        readFragSrvInst.putResp(resp);
    endrule

    interface Put saveData;
        method Action put(Tuple2#(t_idx, t_data) storeReq);
            let {idx, data} = storeReq;
            bramBuffer.portA.request.put(BRAMRequest{
                write: True,
                responseOnWrite:False,
                address: truncate(pack(idx)),
                datain: data
            });
    
            // if the substruct result's highest bit is one, the overflow
            if (checkOverflow) begin
                immAssert(
                    ((idx - lastConsumeIdxReg) >> valueOf(sz_idxNoGuard)) == 0,
                    "buf overfllow @ RingbufStorage",
                    $format(
                        "ringbufName=", fshow(name), "idx=", fshow(idx), " lastConsumeIdxReg=", fshow(lastConsumeIdxReg)
                    )
                );
            end
    
            $display(
                "time=%0t:", $time, "RingbufStorage new input entry",
                ", name=", fshow(name),
                ", idx=", fshow(idx),
                " lastConsumeIdxReg=", fshow(lastConsumeIdxReg) 
            );
        endmethod
    endinterface

    interface allocSlotIdx = toGet(allocedIdxQ);
    interface readFragSrv = readFragSrvInst.srv;
endmodule
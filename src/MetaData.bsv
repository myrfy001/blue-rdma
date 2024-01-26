import BRAM :: *;
import ClientServer :: *;
import Cntrs :: *;
import Connectable :: *;
import FIFOF :: *;
import PAClib :: *;
import Vector :: *;

import DataTypes :: *;
import Headers :: *;
import PrimUtils :: *;
import Settings :: *;
import RdmaUtils :: *;

typedef enum {
    TAG_VEC_RECV_REQ,
    TAG_VEC_RESP_INSERT,
    TAG_VEC_RESP_REMOVE
} TagVecState deriving(Bits, Eq);

// interface TagVecSrv#(numeric type vSz, type anytype);
//     interface Server#(
//         Tuple3#(Bool, anytype, UInt#(TLog#(vSz))),
//         Tuple3#(Bool, UInt#(TLog#(vSz)), anytype)
//     ) srvPort;
//     method Maybe#(anytype) getItem(UInt#(TLog#(vSz)) index);
//     method Action clear();
//     method Bool notEmpty();
//     method Bool notFull();
// endinterface

// module mkTagVecSrv(TagVecSrv#(vSz, anytype)) provisos(
//     FShow#(anytype),
//     Bits#(anytype, tSz),
//     NumAlias#(TLog#(vSz), vLogSz),
//     NumAlias#(TAdd#(1, vLogSz), cntSz),
//     Add#(TLog#(vSz), 1, TLog#(TAdd#(1, vSz))) // vSz must be power of 2
// );
//     Vector#(vSz, Reg#(anytype)) dataVec <- replicateM(mkRegU);
//     Vector#(vSz, Reg#(Bool))     tagVec <- replicateM(mkReg(False));

//     FIFOF#(Tuple3#(Bool, anytype, UInt#(TLog#(vSz))))  reqQ <- mkFIFOF;
//     FIFOF#(Tuple3#(Bool, UInt#(TLog#(vSz)), anytype)) respQ <- mkFIFOF;

//     Reg#(TagVecState) tagVecStateReg <- mkReg(TAG_VEC_RECV_REQ);
//     Reg#(Maybe#(UInt#(TLog#(vSz)))) maybeInsertIdxReg <- mkRegU;
//     Reg#(Bool) respSuccessReg <- mkRegU;

//     Reg#(Bool)    emptyReg <- mkReg(True);
//     Reg#(Bool)     fullReg <- mkReg(False);
//     Reg#(Bool) clearReg[2] <- mkCReg(2, False);

//     Count#(Bit#(cntSz)) itemCnt <- mkCount(0);

//     (* no_implicit_conditions, fire_when_enabled *)
//     rule clearAll if (clearReg[1]);
//         writeVReg(tagVec, replicate(False));
//         reqQ.clear;
//         respQ.clear;

//         tagVecStateReg <= TAG_VEC_RECV_REQ;
//         emptyReg       <= True;
//         fullReg        <= False;
//         clearReg[1]    <= False;
//         itemCnt        <= 0;
//     endrule

//     rule recvReq if (!clearReg[1] && tagVecStateReg == TAG_VEC_RECV_REQ);
//         let { insertOrRemove, insertVal, removeIdx } = reqQ.first;
//         // reqQ.deq;

//         let almostFull  = isAllOnesR(removeMSB(itemCnt));
//         let almostEmpty = isOne(itemCnt);
//         let maybeIndex  = findElem(False, readVReg(tagVec));

//         if (insertOrRemove) begin // Insert
//             maybeInsertIdxReg <= maybeIndex;

//             if (!fullReg) begin
//                 itemCnt.incr(1);
//                 emptyReg <= False;
//                 fullReg <= almostFull;
//             end

//             respSuccessReg <= !fullReg;
//             tagVecStateReg <= TAG_VEC_RESP_INSERT;
//         end
//         else begin // Remove
//             let removeTag = tagVec[removeIdx];

//             if (removeTag) begin
//                 itemCnt.decr(1);
//                 emptyReg <= almostEmpty;
//                 fullReg <= False;
//             end

//             respSuccessReg <= removeTag;
//             tagVecStateReg <= TAG_VEC_RESP_REMOVE;
//         end
//     endrule

//     rule genInsertResp if (!clearReg[1] && tagVecStateReg == TAG_VEC_RESP_INSERT);
//         let { insertOrRemove, insertVal, removeIdx } = reqQ.first;
//         reqQ.deq;

//         let insertIdx = dontCareValue;
//         if (respSuccessReg) begin
//             immAssert(
//                 isValid(maybeInsertIdxReg),
//                 "maybeInsertIdxReg assertion @ mkTagVecSrv",
//                 $format(
//                     "maybeInsertIdxReg=", fshow(maybeInsertIdxReg),
//                     " should be valid"
//                 )
//             );
//             insertIdx = unwrapMaybe(maybeInsertIdxReg);
//             tagVec[insertIdx]  <= True;
//             dataVec[insertIdx] <= insertVal;
//         end

//         respQ.enq(tuple3(respSuccessReg, insertIdx, insertVal));
//         tagVecStateReg <= TAG_VEC_RECV_REQ;
//     endrule

//     rule genRemoveResp if (!clearReg[1] && tagVecStateReg == TAG_VEC_RESP_REMOVE);
//         let { insertOrRemove, insertVal, removeIdx } = reqQ.first;
//         reqQ.deq;

//         let removeVal = dataVec[removeIdx];
//         tagVec[removeIdx] <= False;

//         respQ.enq(tuple3(respSuccessReg, removeIdx, removeVal));
//         tagVecStateReg <= TAG_VEC_RECV_REQ;
//     endrule

//     interface srvPort = toGPServer(reqQ, respQ);

//     method Maybe#(anytype) getItem(UInt#(vLogSz) index);
//         return (tagVec[index]) ? (tagged Valid dataVec[index]) : (tagged Invalid);
//     endmethod

//     method Action clear();
//         clearReg[0] <= True;
//     endmethod

//     method Bool notEmpty() = !emptyReg;
//     method Bool notFull()  = !fullReg;
// endmodule

// MR related

typedef struct {
    ADDR laddr;
    Length len;
    FlagsType#(MemAccessTypeFlag) accFlags;
    HandlerPD pdHandler;
    KeyPartMR keyPart;
} MemRegion deriving(Bits, FShow);

typedef struct {
    Bool      allocOrNot;
    MemRegion mr;
    Bool      lkeyOrNot;
    LKEY      lkey;
    RKEY      rkey;
} ReqMR deriving(Bits, FShow);

typedef struct {
    Bool      successOrNot;
    MemRegion mr;
    LKEY      lkey;
    RKEY      rkey;
} RespMR deriving(Bits, FShow);

typedef Server#(ReqMR, RespMR) SrvPortMR;

// interface MetaDataMRs;
//     interface SrvPortMR srvPort;
//     method Maybe#(MemRegion) getMemRegionByLKey(LKEY lkey);
//     method Maybe#(MemRegion) getMemRegionByRKey(RKEY rkey);
//     method Action clear();
//     method Bool notEmpty();
//     method Bool notFull();
// endinterface

// module mkMetaDataMRs(MetaDataMRs) provisos(
//     Add#(TMul#(MAX_MR_PER_PD, MAX_PD), 0, MAX_MR) // MAX_MR == MAX_MR_PER_PD * MAX_PD
// );
//     TagVecSrv#(MAX_MR_PER_PD, MemRegion) mrTagVec <- mkTagVecSrv;

//     function Tuple2#(LKEY, RKEY) genLocalAndRmtKey(IndexMR mrIndex, MemRegion mr);
//         LKEY lkey = { pack(mrIndex), mr.lkeyPart };
//         RKEY rkey = { pack(mrIndex), mr.rkeyPart };
//         return tuple2(lkey, rkey);
//     endfunction

//     // function IndexMR lkey2IndexMR(LKEY lkey) = unpack(truncateLSB(lkey));
//     // function IndexMR rkey2IndexMR(RKEY rkey) = unpack(truncateLSB(rkey));
    function IndexMR key2IndexMR(Bit#(KEY_WIDTH) key) = unpack(truncateLSB(key));

//     interface srvPort = interface SrvPortMR;
//         interface request = interface Put#(ReqMR);
//             method Action put(ReqMR mrReq);
//                 // let mrIndex = mrReq.lkeyOrNot ?
//                 //     lkey2IndexMR(mrReq.lkey) : rkey2IndexMR(mrReq.rkey);
//                 let mrReqKey = mrReq.lkeyOrNot ? mrReq.lkey : mrReq.rkey;
//                 let mrIndex  = key2IndexMR(mrReqKey);
//                 mrTagVec.srvPort.request.put(tuple3(
//                     mrReq.allocOrNot, mrReq.mr, mrIndex
//                 ));
//             endmethod
//         endinterface;

//         interface response = interface Get#(RespMR);
//             method ActionValue#(RespMR) get();
//                 let { successOrNot, mrIndex, mr } <- mrTagVec.srvPort.response.get;

//                 let { lkey, rkey } = genLocalAndRmtKey(mrIndex, mr);
//                 let mrResp = RespMR {
//                     successOrNot: successOrNot,
//                     mr          : mr,
//                     lkey        : lkey,
//                     rkey        : rkey
//                 };
//                 return mrResp;
//             endmethod
//         endinterface;
//     endinterface;

//     method Maybe#(MemRegion) getMemRegionByLKey(LKEY lkey);
//         let mrIndex = key2IndexMR(lkey);
//         return mrTagVec.getItem(mrIndex);
//     endmethod

//     method Maybe#(MemRegion) getMemRegionByRKey(RKEY rkey);
//         let mrIndex = key2IndexMR(rkey);
//         return mrTagVec.getItem(mrIndex);
//     endmethod

//     method Action  clear() = mrTagVec.clear;
//     method Bool notEmpty() = mrTagVec.notEmpty;
//     method Bool  notFull() = mrTagVec.notFull;
// endmodule

// PD related

// typedef struct {
//     Bool allocOrNot;
//     KeyPD pdKey;
//     HandlerPD pdHandler;
// } ReqPD deriving(Bits, FShow);

// typedef struct {
//     Bool successOrNot;
//     HandlerPD pdHandler;
//     KeyPD pdKey;
// } RespPD deriving(Bits, FShow);

// typedef Server#(ReqPD, RespPD) SrvPortPD;

// interface MetaDataPDs;
//     interface SrvPortPD srvPort;
//     method Bool isValidPD(HandlerPD pdHandler);
//     method Maybe#(MetaDataMRs) getMRs4PD(HandlerPD pdHandler);
//     method Action clear();
//     method Bool notEmpty();
//     method Bool notFull();
// endinterface

// function IndexPD getIndexPD(HandlerPD pdHandler) = unpack(truncateLSB(pdHandler));

// module mkMetaDataPDs(MetaDataPDs);
//     TagVecSrv#(MAX_PD, KeyPD) pdTagVec <- mkTagVecSrv;
//     Vector#(MAX_PD, MetaDataMRs) pdMrVec <- replicateM(mkMetaDataMRs);

//     function Action clearAllMRs(MetaDataMRs mrMetaData);
//         action
//             mrMetaData.clear;
//         endaction
//     endfunction

//     interface srvPort = interface SrvPortPD;
//         interface request = interface Put#(ReqPD);
//             method Action put(ReqPD pdReq);
//                 IndexPD pdIndex = getIndexPD(pdReq.pdHandler);
//                 pdTagVec.srvPort.request.put(tuple3(
//                     pdReq.allocOrNot, pdReq.pdKey, pdIndex
//                 ));
//             endmethod
//         endinterface;

//         interface response = interface Get#(RespPD);
//             method ActionValue#(RespPD) get();
//                 let { successOrNot, pdIndex, pdKey } <- pdTagVec.srvPort.response.get;

//                 HandlerPD pdHandler = { pack(pdIndex), pdKey };
//                 let pdResp = RespPD {
//                     successOrNot: successOrNot,
//                     pdHandler   : pdHandler,
//                     pdKey       : pdKey
//                 };
//                 return pdResp;
//             endmethod
//         endinterface;
//     endinterface;

//     method Bool isValidPD(HandlerPD pdHandler);
//         let pdIndex = getIndexPD(pdHandler);
//         return isValid(pdTagVec.getItem(pdIndex));
//     endmethod

//     method Maybe#(MetaDataMRs) getMRs4PD(HandlerPD pdHandler);
//         let pdIndex = getIndexPD(pdHandler);
//         return isValid(pdTagVec.getItem(pdIndex)) ?
//             (tagged Valid pdMrVec[pdIndex]) : (tagged Invalid);
//     endmethod

//     method Action clear();
//         pdTagVec.clear;
//         mapM_(clearAllMRs, pdMrVec);
//     endmethod

//     method Bool notEmpty() = pdTagVec.notEmpty;
//     method Bool notFull()  = pdTagVec.notFull;
// endmodule





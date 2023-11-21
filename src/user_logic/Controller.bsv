import Arbiter :: *;
import UserLogicTypes :: *;
import ClientServer :: *;

typedef Client#(MetaDataReq, MetaDataResp) MetaDataClt;

interface UserLogicController;
    interface DmaReadClt xdmaReadClt;
    interface DmaWriteClt xdmaWriteClt;
    interface MetaDataClt metaDataClt;
endinterface




// FIFOF#(DmaReadResp) dataPathReadRespQ <- mkFIFOF;
// FIFOF#(DmaWriteReq) dataPathWriteReqQ <- mkFIFOF;
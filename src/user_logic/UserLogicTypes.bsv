
import Settings :: *;
import DataTypes :: *;

typedef 12 CONTROL_REG_ADDR_WIDTH;
typedef 4 CONTROL_REG_DATA_STRB_WIDTH;
typedef TMul#(CONTROL_REG_DATA_STRB_WIDTH, 8) CONTROL_DATA_WIDTH;
typedef 64 HOST_ADDR_WIDTH;

typedef enum {
    RdmaCsrCmdTypeModifyFirstStagePgt = 0,
    RdmaCsrCmdTypeModifySecondStagePgt = 1,
    RdmaCsrCmdTypeMaxGuard = 16'hFFFF // padding to make this enum use 8 bit
} RdmaCsrCmdType deriving(Bits, Eq, FShow);

typedef Bit#(16) ControlCmdReqId;
typedef Bit#(8)  ControlCmdErrCode;

typedef struct {
    RdmaCsrCmdType cmdType;
    ControlCmdReqId reqId;
} RdmaCsrCmdTypeAndId deriving(Bits, FShow);

typedef struct {
    RdmaCsrCmdType cmdType;
    ControlCmdReqId reqId;
    DataStream dataStream;
} DmaFetchedCmd deriving(Bits, FShow);

typedef struct {
    ControlCmdReqId finishedReqId;
    ControlCmdErrCode  errorCode;
} RdmaCmdExecuteResponse deriving(Bits, FShow);

typedef struct {
    Bit#(CONTROL_REG_DATA_WIDTH) ctlRegCmdSize;
    Bit#(HOST_ADDR_WIDTH) ctlRegCmdAddr;
    RdmaCsrCmdTypeAndId ctlRegCmdTypeAndId;
} RdmaControlCmdEntry deriving(Bits, FShow);

typedef 32 CONTROL_REG_DATA_WIDTH;

typedef 64 PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED;
typedef TDiv#(DATA_BUS_WIDTH, PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED) PGT_SECOND_STAGE_ENTRY_REQUEST_PER_STREAM_FRAME;
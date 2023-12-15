
import Settings :: *;
import DataTypes :: *;
import Headers :: *;
import UserLogicSettings :: *;
import ClientServer :: *;
import Controller :: *;
import PrimUtils :: *;

typedef 12 CSR_ADDR_WIDTH;
typedef 4 CSR_DATA_STRB_WIDTH;
typedef TMul#(CSR_DATA_STRB_WIDTH, BYTE_WIDTH) CSR_DATA_WIDTH;
typedef 64 HOST_ADDR_WIDTH;

typedef Bit#(CSR_ADDR_WIDTH) CsrAddr;
typedef Bit#(CSR_DATA_WIDTH) CsrData;


typedef struct {
    t_addr addr;
    t_data data;
} CsrWriteRequest#(type t_addr, type t_data) deriving(Bits);

typedef struct {
    Bit#(0) flag;
} CsrWriteResponse deriving(Bits);

typedef struct {
    t_addr addr;
} CsrReadRequest#(type t_addr) deriving(Bits);

typedef struct {
    t_data data;
} CsrReadResponse#(type t_data) deriving(Bits);



// typedef enum {
//     RdmaCsrCmdTypeModifyFirstStagePgt = 0,
//     RdmaCsrCmdTypeModifySecondStagePgt = 1,
//     RdmaCsrCmdTypeMaxGuard = 16'hFFFF // padding to make this enum use 8 bit
// } RdmaCsrCmdType deriving(Bits, Eq, FShow);

typedef Bit#(16) ControlCmdReqId;
typedef Bit#(8)  ControlCmdErrCode;


typedef 64 PGT_SECOND_STAGE_ENTRY_REQUEST_SIZE_PADDED;


typedef 12 PAGE_OFFSET_BIT_WIDTH;
typedef TSub#(SizeOf#(ADDR), 1) PAGE_NUMBER_BITS_RANGE_HIGH_POS_4K;
typedef PAGE_OFFSET_BIT_WIDTH PAGE_NUMBER_BITS_RANGE_LOW_POS_4K;
typedef 11 PAGE_OFFSET_BITS_RANGE_HIGH_POS_4K;


typedef UInt#(PAGE_OFFSET_BIT_WIDTH) PageOffset4k;
typedef UInt#(TSub#(SizeOf#(ADDR), PAGE_OFFSET_BIT_WIDTH)) PageNumber4k;


typedef 256 PCIE_MRRS;
typedef PCIE_MRRS RINGBUF_BLOCK_READ_LEN;
typedef TMul#(PCIE_MRRS, BYTE_WIDTH) RINGBUF_READ_BLOCK_BIT_WIDTH;
typedef TLog#(RINGBUF_READ_BLOCK_BIT_WIDTH) RINGBUF_DMA_ACCESS_LEN_WIDTH;
typedef Bit#(RINGBUF_DMA_ACCESS_LEN_WIDTH) RingbufDMABlockAccessLen;
typedef TDiv#(PCIE_MRRS, USER_LOGIC_DESCRIPTOR_BYTE_WIDTH) RINGBUF_DESC_ENTRY_PER_READ_BLOCK;
typedef Bit#(TLog#(RINGBUF_DESC_ENTRY_PER_READ_BLOCK)) RingbufReadBlockInnerOffset;
typedef TLog#(PCIE_MRRS) RINGBUF_READ_BLOCK_BYTE_WIDTH;

typedef 1 RINGBUF_DESC_OPCODE_OFFSET;
typedef 6 RINGBUF_DESC_OPCODE_LENGTH;
typedef Bit#(RINGBUF_DESC_OPCODE_LENGTH) RingbufRawDescriptorOpcode;


typedef Bit#(20) UserLogicDmaLen;

typedef TMul#(DATA_BUS_WIDTH, 2) DATA_BUS_WIDE_WIDTH;
typedef TMul#(DATA_BUS_BYTE_WIDTH, 2) DATA_BUS_WIDE_BYTE_WIDTH;
typedef Bit#(DATA_BUS_WIDE_WIDTH)      DATA_WIDE;
typedef Bit#(DATA_BUS_WIDE_BYTE_WIDTH) ByteEnWide;

typedef struct {
    DATA_WIDE data;
    ByteEnWide byteEn;
    Bool isFirst;
    Bool isLast;
} DataStreamWide deriving(Bits, Bounded, Eq, FShow);

typedef struct {
    ADDR addr;
    UserLogicDmaLen len;
} UserLogicDmaH2cReq deriving(Bits, FShow);

typedef struct {
    DataStream dataStream;
} UserLogicDmaH2cResp deriving(Bits, FShow);

typedef struct {
    DataStreamWide dataStream;
} UserLogicDmaH2cWideResp deriving(Bits, FShow);


typedef struct {
    ADDR addr;
    UserLogicDmaLen len;
    DataStream dataStream;
} UserLogicDmaC2hReq deriving(Bits, FShow);

typedef struct {
    ADDR addr;
    UserLogicDmaLen len;
    DataStreamWide dataStream;
} UserLogicDmaC2hWideReq deriving(Bits, FShow);

typedef struct {
} UserLogicDmaC2hResp deriving(Bits, FShow);


typedef Server#(UserLogicDmaH2cReq, UserLogicDmaH2cResp)    UserLogicDmaReadSrv;
typedef Server#(UserLogicDmaC2hReq, UserLogicDmaC2hResp)    UserLogicDmaWriteSrv;
typedef Client#(UserLogicDmaH2cReq, UserLogicDmaH2cResp)    UserLogicDmaReadClt;
typedef Client#(UserLogicDmaC2hReq, UserLogicDmaC2hResp)    UserLogicDmaWriteClt;

typedef Server#(UserLogicDmaH2cReq, UserLogicDmaH2cWideResp)    UserLogicDmaReadWideSrv;
typedef Server#(UserLogicDmaC2hWideReq, UserLogicDmaC2hResp)    UserLogicDmaWriteWideSrv;
typedef Client#(UserLogicDmaH2cReq, UserLogicDmaH2cWideResp)    UserLogicDmaReadWideClt;
typedef Client#(UserLogicDmaC2hWideReq, UserLogicDmaC2hResp)    UserLogicDmaWriteWideClt;

typedef 2 XDMA_GEARBOX_WIDE_VECTOR_LEN;
typedef 1 XDMA_GEARBOX_NARROW_VECTOR_LEN;

typedef Bit#(USER_LOGIC_DESCRIPTOR_BIT_WIDTH) RingbufRawDescriptor;
typedef Bit#(RINGBUF_NUMBER_WIDTH) RingbufNumber;

typedef 2 COMMAND_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT;
typedef 2 SQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT;
typedef 1 RQ_QUEUE_DESCRIPTOR_MAX_IN_USE_SEG_COUNT;
typedef 1 CQ_DESCRIPTOR_MAX_IN_USE_SEG_COUNT;

typedef enum {
    CmdQueueOpcodeUpdateFirstStagePGT = 'h0,
    CmdQueueOpcodeUpdateSecondStagePGT = 'h1,
    CmdQueueOpcodePdManagement = 'h2,
    CmdQueueOpcodeMrManagement = 'h3,
    CmdQueueOpcodeQpManagement = 'h4
} CommandQueueOpcode deriving(Bits, Eq);

typedef Bit#(TLog#(CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT)) DescriptorSegmentIndex;

typedef struct {
    Bit#(32)    userData;
    Bit#(20)    reserved1;
    Bool        isSuccessOrNeedSignalCplt;
    Bit#(4)     extraSegmentCnt;
    Bit#(6)     opCode;
    Bool        valid;
}CmdQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    Bit#(32)                reserved1;
    Bit#(32)                index;
    Bit#(32)                pointedToSecondStageCount;
    Bit#(32)                pointedToSecondStageIndex;
    Bit#(64)                baseVA;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueReqDescUpdateFirstStagePGT deriving(Bits, FShow);

typedef struct {
    Bit#(64)                reserved1;
    Bit#(32)                dmaReadLength;
    Bit#(32)                startIndex;
    Bit#(64)                dmaAddr;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueReqDescUpdateSecondStagePGT deriving(Bits, FShow);

typedef struct {
    Bit#(64)                reserved1;
    Bit#(64)                reserved2;
    Bit#(64)                reserved3;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueRespDescUpdatePGT deriving(Bits, FShow);

typedef struct {
    Bit#(64)                reserved1;
    Bit#(64)                reserved2;
    Bit#(31)                reserved3;
    Bool                    isAlloc;
    HandlerPD               pdHandler;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueReqDescPdManagement deriving(Bits, FShow);

typedef struct {
    Bit#(64)                reserved1;
    Bit#(64)                reserved2;
    HandlerPD               pdHandler;
    Bit#(32)                reserved3;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueRespDescPdManagement deriving(Bits, FShow);

typedef struct {
    Bit#(32)                reserved1;
    Length                  mrLen;
    ADDR                    startAddr;
    Bit#(16)                reserved2;
    MemAccessTypeFlag       accessFlag;
    Bit#(7)                 reserved3;
    Bool                    isAlloc;
    HandlerPD               pdHandler;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueReqDescMrManagementSeg0 deriving(Bits, FShow);

typedef struct {
    Bit#(64) reserved1;
    Bit#(64) reserved2;
    Bit#(64) reserved3;
    RKEY      rkey;
    LKEY      lkey;
}CmdQueueReqDescMrManagementSeg1 deriving(Bits, FShow);

typedef struct {
    Bit#(64)                reserved1;
    Bit#(64)                reserved2;
    RKEY                    rkey;
    LKEY                    lkey;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueRespDescMrManagement deriving(Bits, FShow);


typedef struct {
    Bit#(64)                reserved1;
    Bit#(23)                reserved2;
    Bool                    sqSigAll;
    Bit#(4)                 reserved3;
    TypeQP                  qpType;
    Bit#(6)                 reserved4;
    QpAttrMaskFlag          qpAttrMask;
    QPN                     qpn;
    Bit#(6)                 reserved5;
    QpReqType               qpReqType;
    HandlerPD               pdHandler;
    CmdQueueDescCommonHead  commonHeader;
}CmdQueueReqDescQpManagementSeg0 deriving(Bits, FShow);

typedef struct {
    Bit#(16)                        reserved1;
    RetryCnt                        rnrRetry;
    TimeOutTimer                    timeout;
    RetryCnt                        retryCnt;
    RnrTimer                        minRnrTimer;
    PendingReqCnt                   maxDestReadAtomic;
    PendingReqCnt                   maxReadAtomic;
    PKEY                            pkeyIndex;
    Bit#(7)                         reserved2;
    Bool                            sqDraining;
    InlineDataSize                  maxInlineData;
    ScatterGatherElemCnt            maxRecvSGE;
    ScatterGatherElemCnt            maxSendSGE;
    FlagsType#(MemAccessTypeFlag)   qpAccessFlags;
    QPN                             dqpn;
    PendingReqCnt                   maxRecvWR;
    PSN                             sqPSN;
    PendingReqCnt                   maxSendWR;
    PSN                             rqPSN;
    QKEY                            qkey;
    Bit#(21)                        reserved3;
    PMTU                            pmtu;
    StateQP                         curQpState;
    StateQP                         qpState;
}CmdQueueReqDescQpManagementSeg1 deriving(Bits, FShow);

typedef CmdQueueReqDescQpManagementSeg0 CmdQueueRespDescQpManagementSeg0;
typedef CmdQueueReqDescQpManagementSeg1 CmdQueueRespDescQpManagementSeg1;


typedef struct {
    Length          len;
    Bit#(20)        reserved1;
    Bool            isSuccessOrNeedSignalCplt;
    Bit#(4)         extraSegmentCnt;
    Bit#(2)         reserved2;
    WorkReqOpCode   opCode;
    Bool            valid;
}SendQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    RKEY                        rkey;
    LKEY                        lkey;
    ADDR                        raddr;
    ADDR                        laddr;
    SendQueueDescCommonHead     commonHeader;
}SendQueueReqDescSeg0 deriving(Bits, FShow);

typedef struct {
    Bit#(64)        reserved1;
    Bit#(64)        reserved2;
    Bit#(64)        reserved3;
    Bit#(32)        reserved4;
    QPN             sqpn;
    Bool            solicited;
    Bit#(2)         reserved5;
    WorkReqSendFlag flags;
}SendQueueReqDescSeg1 deriving(Bits, FShow);

typedef struct {
    Length          len;
    Bit#(20)        reserved1;
    Bool            isSuccessOrNeedSignalCplt;
    Bit#(4)         extraSegmentCnt;
    Bit#(2)         reserved2;
    WorkReqOpCode   opCode;
    Bool            valid;
}RecvQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    Bit#(8)                     reserved1;
    QPN                         sqpn;
    LKEY                        lkey;
    Bit#(64)                    reserved2;
    ADDR                        laddr;
    RecvQueueDescCommonHead     commonHeader;
}RecvQueueReqDesc deriving(Bits, FShow);



typedef struct {
    Length          len;
    Bit#(21)        reserved1;
    Bit#(4)         extraSegmentCnt;
    Bit#(6)         reserved2;
    Bool            valid;
}CompQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    Bit#(40)                    reserved1;
    QPN                         qpn;
    Bit#(64)                    reserved2;
    Bit#(16)                    reserved3;
    PKEY                        pkey;
    Bit#(11)                    reserved4;
    WorkCompStatus              status;
    Bit#(1)                     reserved5;
    WorkCompFlags               flags;
    WorkCompOpCode              opcode;
    CompQueueDescCommonHead     commonHeader;
}CompQueueReqDesc deriving(Bits, FShow);
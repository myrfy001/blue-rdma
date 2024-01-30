import Reserved :: *;
import Settings :: *;
import DataTypes :: *;
import Headers :: *;
import UserLogicSettings :: *;
import ClientServer :: *;
import PrimUtils :: *;

typedef 20 CSR_ADDR_WIDTH;
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
    CmdQueueOpcodeUpdateMrTable = 'h0,
    CmdQueueOpcodeUpdatePGT = 'h1,
    CmdQueueOpcodePdManagement = 'h2,
    CmdQueueOpcodeQpManagement = 'h3
} CommandQueueOpcode deriving(Bits, Eq);

typedef Bit#(TLog#(CMD_QUEUE_DESCRIPTOR_MAX_SEGMENT_CNT)) DescriptorSegmentIndex;

typedef struct {
    Bit#(32)                userData;
    ReservedZero#(20)       reserved1;
    Bool                    isSuccessOrNeedSignalCplt;
    Bit#(4)                 extraSegmentCnt;
    Bit#(6)                 opCode;
    Bool                    valid;
} CmdQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    ReservedZero#(7)            reserved1;
    Bit#(17)                    pgtOffset;
    Bit#(8)                     accFlags;
    Bit#(32)                    pdHandler;
    Bit#(32)                    mrKey;
    Bit#(32)                    mrLength;
    Bit#(64)                    mrBaseVA;
    CmdQueueDescCommonHead      commonHeader;
} CmdQueueReqDescUpdateMrTable deriving(Bits, FShow);

typedef struct {
    ReservedZero#(64)               reserved1;
    Bit#(32)                        dmaReadLength;
    Bit#(32)                        startIndex;
    Bit#(64)                        dmaAddr;
    CmdQueueDescCommonHead          commonHeader;
} CmdQueueReqDescUpdatePGT deriving(Bits, FShow);

typedef struct {
    ReservedZero#(64)               reserved1;
    ReservedZero#(64)               reserved2;
    ReservedZero#(64)               reserved3;
    CmdQueueDescCommonHead          commonHeader;
} CmdQueueRespDescUpdatePGT deriving(Bits, FShow);


typedef struct {
    ReservedZero#(104)              reserved1;      // 104 bits
    ReservedZero#(5)                reserved2;      // 5   bits
    PMTU                            pmtu;           // 3   bits
    FlagsType#(MemAccessTypeFlag)   rqAccessFlags;  // 8   bits
    ReservedZero#(4)                reserved3;      // 4   bits
    TypeQP                          qpType;         // 4   bits
    HandlerPD                       pdHandler;      // 32  bits
    QPN                             qpn;            // 24  bits
    ReservedZero#(6)                reserved4;      // 6   bits
    Bool                            isError;        // 1   bit
    Bool                            isValid;        // 1   bit
    CmdQueueDescCommonHead          commonHeader;   // 64  bits
} CmdQueueReqDescQpManagementSeg0 deriving(Bits, FShow);


typedef CmdQueueReqDescQpManagementSeg0 CmdQueueRespDescQpManagementSeg0;



typedef struct {
    Length                  len;
    ReservedZero#(20)       reserved1;
    Bool                    isSuccessOrNeedSignalCplt;
    Bit#(4)                 extraSegmentCnt;
    ReservedZero#(2)        reserved2;
    WorkReqOpCode           opCode;
    Bool                    valid;
} SendQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    RKEY                        rkey;
    LKEY                        lkey;
    ADDR                        raddr;
    ADDR                        laddr;
    SendQueueDescCommonHead     commonHeader;
} SendQueueReqDescSeg0 deriving(Bits, FShow);

typedef struct {
    ReservedZero#(64)       reserved1;
    ReservedZero#(64)       reserved2;
    ReservedZero#(64)       reserved3;
    ReservedZero#(32)       reserved4;
    QPN                     sqpn;
    Bool                    solicited;
    ReservedZero#(2)        reserved5;
    WorkReqSendFlag         flags;
} SendQueueReqDescSeg1 deriving(Bits, FShow);

typedef struct {
    Length                  len;
    ReservedZero#(20)       reserved1;
    Bool                    isSuccessOrNeedSignalCplt;
    Bit#(4)                 extraSegmentCnt;
    ReservedZero#(2)        reserved2;
    WorkReqOpCode           opCode;
    Bool                    valid;
} RecvQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    ReservedZero#(8)                    reserved1;
    QPN                                 sqpn;
    LKEY                                lkey;
    ReservedZero#(64)                   reserved2;
    ADDR                                laddr;
    RecvQueueDescCommonHead             commonHeader;
} RecvQueueReqDesc deriving(Bits, FShow);



typedef struct {
    Length                  len;
    ReservedZero#(21)       reserved1;
    Bit#(4)                 extraSegmentCnt;
    ReservedZero#(6)        reserved2;
    Bool                    valid;
} CompQueueDescCommonHead deriving(Bits, FShow);

typedef struct {
    ReservedZero#(40)                   reserved1;
    QPN                                 qpn;
    ReservedZero#(64)                   reserved2;
    ReservedZero#(16)                   reserved3;
    PKEY                                pkey;
    ReservedZero#(11)                   reserved4;
    WorkCompStatus                      status;
    ReservedZero#(1)                    reserved5;
    WorkCompFlags                       flags;
    WorkCompOpCode                      opcode;
    CompQueueDescCommonHead             commonHeader;
} CompQueueReqDesc deriving(Bits, FShow);


typedef struct {
    ReservedZero#(6)                reserved1;    // 6
    Bool                            ackReq;       // 1
    Bool                            solicited;    // 1
    PSN                             psn;          // 24
    QPN                             dqpn;         // 24
    RdmaOpCode                      opcode;       // 5
    TransType                       trans;        // 3
} PktMeatReportQueueDescFragBTH deriving(Bits, FShow);

typedef struct {
    Length                  dlen;         // 32
    RKEY                    rkey;         // 32
    ADDR                    va;           // 64
} PktMeatReportQueueDescFragRETH deriving(Bits, FShow);

typedef struct {
    AethCode                code;         // 2
    AethValue               value;        // 5
    MSN                     msn;          // 24
    PSN                     lastRetryPSN; // 24
} PktMeatReportQueueDescFragAETH deriving(Bits, FShow);

typedef struct {
    RKEY                            secondaryRkey;   // 32
    ADDR                            secondaryVa;     // 64 
} PktMeatReportQueueDescFragSecondaryRETH deriving(Bits, FShow);

typedef struct {
    IMM                             data;           // 32
} PktMeatReportQueueDescFragImmDT deriving(Bits, FShow);

typedef struct {
    ReservedZero#(184)              reserved1;      // 184
    PktMeatReportQueueDescFragBTH   bth;            // 64
    RdmaReqStatus                   reqStatus;      // 8 
} PktMeatReportQueueDescBth deriving(Bits, FShow);

typedef struct {
    ReservedZero#(24)               reserved1;      // 24
    PktMeatReportQueueDescFragImmDT immDt;          // 32
    PktMeatReportQueueDescFragRETH  reth;           // 128
    PktMeatReportQueueDescFragBTH   bth;            // 64
    RdmaReqStatus                   reqStatus;      // 8 
} PktMeatReportQueueDescBthRethImmDT deriving(Bits, FShow);

typedef struct {
    ReservedZero#(129)              reserved1;      // 129
    PktMeatReportQueueDescFragAETH  aeth;           // 55
    PktMeatReportQueueDescFragBTH   bth;            // 64
    RdmaReqStatus                   reqStatus;      // 8 
} PktMeatReportQueueDescBthAeth deriving(Bits, FShow);

typedef struct {
    ReservedZero#(160)                          reserved1;       // 160
    PktMeatReportQueueDescFragSecondaryRETH     secReth;         // 96
} PktMeatReportQueueDescSecondaryReth deriving(Bits, FShow);


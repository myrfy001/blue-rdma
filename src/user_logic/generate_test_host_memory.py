# coding: utf-8

from ctypes import *
from itertools import islice

TOTAL_MEMORY_SIZE = 1024 * 1024 * 64

PGT_ENTRY_OFFSET = 0x200
PGT_ENTRY_CNT = 0x20
PGT_ENTRY_SIZE = 0x08
PGT_MR_VASE_VA = 0xFBABCDCEEEEE0001

CMD_QUEUE_H2C_RINGBUF_START_PA = 0x00
CMD_QUEUE_C2H_RINGBUF_START_PA = 0x1000
RQ_RINGBUF_START_PA = 0x2000
SQ_RINGBUF_START_PA = 0x3000
RCQ_RINGBUF_START_PA = 0x4000
SCQ_RINGBUF_START_PA = 0x5000

PGT_TABLE_START_ADDR = 0x10000

MR_0_PA_START = 0x100000

REQ_SIDE_VA_ADDR = PGT_MR_VASE_VA & 0xFFFFFFFFFFE00000 + 0x200

RESP_SIDE_VA_ADDR = PGT_MR_VASE_VA & 0xFFFFFFFFFFE00000 + 0x90000

SEND_SIDE_LKEY = 0x6622
SEND_SIDE_RKEY = SEND_SIDE_LKEY
RECV_SIDE_LKEY = 0x6622
RECV_SIDE_RKEY = RECV_SIDE_LKEY
PKEY_INDEX = 0


def batched(iterable, n):
    it = iter(iterable)
    while True:
        batch = list(islice(it, n))
        if not batch:
            return
        yield batch


def dump_to_str(memory):
    with open("test_host_memory.hex", "w") as fo:
        for i, line in enumerate(batched(memory, 64)):
            fo.write(bytes(reversed(line)).hex())
            fo.write("\n")


def memcpy(dst, start_addr, src):
    dst[start_addr:start_addr + len(src)] = src


class CmdQueueReqDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 6),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_0", c_int, 20),
                ("F_CMD_QUEUE_USER_DATA", c_int, 32),
                ]


class CmdQueueRespDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 6),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_RESP_SUCCESS", c_int, 1),
                ("F_RESERVED_0", c_int, 20),
                ("F_CMD_QUEUE_USER_DATA", c_int, 32),
                ]


class CmdQueueDescUpdateFirstStagePGT(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_PGT_FIRST_STAGE_BASE_VA", c_longlong),
                ("F_PGT_FIRST_STAGE_POINTED_TO_OFFSET", c_int, 32),
                ("F_PGT_FIRST_STAGE_POINTED_TO_COUNT", c_int, 32),
                ("F_PGT_FIRST_STAGE_INDEX", c_int, 32),
                ("F_RESERVED_0", c_int, 32),
                ]


class CmdQueueDescUpdateSecondStagePGT(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_PGT_SECOND_STAGE_SRC_DATA_ADDR", c_longlong),
                ("F_PGT_SECOND_STAGE_OFFSET", c_int, 32),
                ("F_PGT_SECOND_STAGE_SRC_DATA_LEN", c_int, 32),
                ("F_RESERVED_0", c_longlong),
                ]


class CmdQueueDescPdManagement(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_PD_ADMIN_PD_HANDLER", c_int, 32),
                ("F_PD_ADMIN_IS_ALLOC", c_int, 1),
                ("F_RESERVED_0", c_int, 31),
                ("F_RESERVED_1", c_longlong, 64),
                ("F_RESERVED_2", c_longlong, 64),
                ]


class CmdQueueDescMrManagementSeg0(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_MR_ADMIN_PD_HANDLER", c_int, 32),
                ("F_MR_ADMIN_IS_ALLOC", c_int, 1),
                ("F_RESERVED_0", c_int, 7),
                ("F_MR_ADMIN_ACCESS_FLAG", c_int, 8),
                ("F_RESERVED_1", c_int, 16),
                ("F_RESERVED_2", c_longlong, 64),
                ("F_RESERVED_3", c_longlong, 64),
                ]


class CmdQueueDescMrManagementSeg1(Structure):
    _fields_ = [("F_MR_ADMIN_LKEY", c_int, 32),
                ("F_MR_ADMIN_RKEY", c_int, 32),
                ("F_RESERVED_1", c_longlong, 64),
                ("F_RESERVED_2", c_longlong, 64),
                ("F_RESERVED_3", c_longlong, 64),
                ]


class StateQP:
    IBV_QPS_RESET = 0
    IBV_QPS_INIT = 1
    IBV_QPS_RTR = 2
    IBV_QPS_RTS = 3
    IBV_QPS_SQD = 4
    IBV_QPS_SQE = 5
    IBV_QPS_ERR = 6
    IBV_QPS_UNKNOWN = 7
    IBV_QPS_CREATE = 8


class TypeQP:
    IBV_QPT_RC = 2
    IBV_QPT_UC = 3
    IBV_QPT_UD = 4
    # IBV_QPT_RAW_PACKET = 8
    IBV_QPT_XRC_SEND = 9
    IBV_QPT_XRC_RECV = 10
    # IBV_QPT_DRIVER = 0xff


class QpReqType:
    REQ_QP_CREATE = 0
    REQ_QP_DESTROY = 1
    REQ_QP_MODIFY = 2
    REQ_QP_QUERY = 3


class MemAccessTypeFlag:
    IBV_ACCESS_NO_FLAGS = 0  # Not defined in rdma-core
    IBV_ACCESS_LOCAL_WRITE = 1  # (1 << 0)
    IBV_ACCESS_REMOTE_WRITE = 2  # (1 << 1)
    IBV_ACCESS_REMOTE_READ = 4  # (1 << 2)
    IBV_ACCESS_REMOTE_ATOMIC = 8  # (1 << 3)
    IBV_ACCESS_MW_BIND = 16  # (1 << 4)
    IBV_ACCESS_ZERO_BASED = 32  # (1 << 5)
    IBV_ACCESS_ON_DEMAND = 64  # (1 << 6)
    IBV_ACCESS_HUGETLB = 128  # (1 << 7)
    # IBV_ACCESS_RELAXED_ORDERING    = IBV_ACCESS_OPTIONAL_FIRST


class QpAttrMaskFlag:
    IBV_QP_NO_FLAGS = 0       # Not defined in rdma-core
    IBV_QP_STATE = 1       # 1 << 0
    IBV_QP_CUR_STATE = 2       # 1 << 1
    IBV_QP_EN_SQD_ASYNC_NOTIFY = 4       # 1 << 2
    IBV_QP_ACCESS_FLAGS = 8       # 1 << 3
    IBV_QP_PKEY_INDEX = 16      # 1 << 4
    IBV_QP_PORT = 32      # 1 << 5
    IBV_QP_QKEY = 64      # 1 << 6
    IBV_QP_AV = 128     # 1 << 7
    IBV_QP_PATH_MTU = 256     # 1 << 8
    IBV_QP_TIMEOUT = 512     # 1 << 9
    IBV_QP_RETRY_CNT = 1024    # 1 << 10
    IBV_QP_RNR_RETRY = 2048    # 1 << 11
    IBV_QP_RQ_PSN = 4096    # 1 << 12
    IBV_QP_MAX_QP_RD_ATOMIC = 8192    # 1 << 13
    IBV_QP_ALT_PATH = 16384   # 1 << 14
    IBV_QP_MIN_RNR_TIMER = 32768   # 1 << 15
    IBV_QP_SQ_PSN = 65536   # 1 << 16
    IBV_QP_MAX_DEST_RD_ATOMIC = 131072  # 1 << 17
    IBV_QP_PATH_MIG_STATE = 262144  # 1 << 18
    IBV_QP_CAP = 524288  # 1 << 19
    IBV_QP_DEST_QPN = 1048576  # 1 << 20
    # These bits were supported on older kernels, but never exposed from libibverbs
    # _IBV_QP_SMAC               = 1 << 21
    # _IBV_QP_ALT_SMAC           = 1 << 22
    # _IBV_QP_VID                = 1 << 23
    # _IBV_QP_ALT_VID            = 1 << 24
    IBV_QP_RATE_LIMIT = 33554432  # 1 << 25


class PMTU:
    IBV_MTU_256 = 1
    IBV_MTU_512 = 2
    IBV_MTU_1024 = 3
    IBV_MTU_2048 = 4
    IBV_MTU_4096 = 5


class CmdQueueDescQpManagementSeg0(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_QP_ADMIN_PD_HANDLER", c_int, 32),
                ("F_QP_ADMIN_REQ_TYPE", c_int, 2),
                ("F_RESERVED_0", c_int, 6),
                ("F_QP_ADMIN_QPN", c_int, 24),
                ("F_QP_ADMIN_ATTR_MASK", c_int, 26),
                ("F_RESERVED_1", c_int, 6),
                ("F_QP_ADMIN_QP_TYPE", c_int, 4),
                ("F_RESERVED_2", c_int, 4),
                ("F_QP_ADMIN_SQ_SIG_ALL", c_int, 1),
                ("F_RESERVED_3", c_int, 23),
                ("F_RESERVED_4", c_longlong, 64),
                ]


class CmdQueueDescQpManagementSeg1(Structure):
    _fields_ = [("F_QP_ADMIN_QP_STATE", c_int, 4),
                ("F_QP_ADMIN_QP_CURRENT_STATE", c_int, 4),
                ("F_QP_ADMIN_PMTU", c_int, 3),
                ("F_RESERVED_0", c_int, 21),
                ("F_QP_ADMIN_QKEY", c_int, 32),
                ("F_QP_ADMIN_RQ_PSN", c_int, 24),
                ("F_QP_ADMIN_CAP_MAX_SEND_WR", c_int, 8),
                ("F_QP_ADMIN_SQ_PSN", c_int, 24),
                ("F_QP_ADMIN_CAP_MAX_RECV_WR", c_int, 8),
                ("F_QP_ADMIN_DQPN", c_int, 24),
                ("F_QP_ADMIN_ACCESS_FLAG", c_int, 8),
                ("F_QP_ADMIN_CAP_MAX_SEND_SGE", c_int, 8),
                ("F_QP_ADMIN_CAP_MAX_RECV_SGE", c_int, 8),
                ("F_QP_ADMIN_CAP_MAX_INLINE_DATA", c_int, 8),
                ("F_QP_ADMIN_SQ_DRAINING", c_int, 1),
                ("F_RESERVED_1", c_int, 7),
                ("F_QP_ADMIN_PKEY_INDEX", c_int, 16),
                ("F_QP_ADMIN_MAX_READ_ATOMIC", c_int, 8),
                ("F_QP_ADMIN_MAX_DEST_READ_ATOMIC", c_int, 8),
                ("F_QP_ADMIN_MIN_RNR_TIMER", c_int, 5),
                ("F_QP_ADMIN_RETRY_CNT", c_int, 3),
                ("F_QP_ADMIN_TIMEOUT", c_int, 5),
                ("F_QP_ADMIN_RNR_RETRY", c_int, 3),
                ("F_RESERVED_2", c_int, 16),
                ]


class CmdQueueDescOperators:
    F_OPCODE_CMDQ_UPDATE_FIRST_STAGE_PGT = 0x00
    F_OPCODE_CMDQ_UPDATE_SECOND_STAGE_PGT = 0x01
    F_OPCODE_CMDQ_MANAGE_PD = 0x02
    F_OPCODE_CMDQ_MANAGE_MR = 0x03
    F_OPCODE_CMDQ_MANAGE_QP = 0x04


class SendQueueDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 4),
                ("F_RESERVED_0", c_int, 2),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_1", c_int, 20),
                ("F_DATA_LEN", c_int, 32),
                ]


class SendQueueDescSeg0(Structure):
    _fields_ = [("common_header", SendQueueDescCommonHeader),
                ("F_L_ADDR", c_longlong, 64),
                ("F_R_ADDR", c_longlong, 64),
                ("F_LKEY", c_int, 32),
                ("F_RKEY", c_int, 32),
                ]


class SendQueueDescSeg1(Structure):
    _fields_ = [("F_SQ_SEND_FLAG", c_int, 5),
                ("F_RESERVED_0", c_int, 2),
                ("F_SQ_SOLICITED", c_int, 1),
                ("F_SQ_SQPN", c_int, 24),
                ("F_RESERVED_1", c_int, 32),
                ("F_RESERVED_2", c_longlong, 64),
                ("F_RESERVED_3", c_longlong, 64),
                ("F_RESERVED_4", c_longlong, 64),
                ]


class WorkReqOpCode:
    IBV_WR_RDMA_WRITE = 0
    IBV_WR_RDMA_WRITE_WITH_IMM = 1
    IBV_WR_SEND = 2
    IBV_WR_SEND_WITH_IMM = 3
    IBV_WR_RDMA_READ = 4
    IBV_WR_ATOMIC_CMP_AND_SWP = 5
    IBV_WR_ATOMIC_FETCH_AND_ADD = 6
    IBV_WR_LOCAL_INV = 7
    IBV_WR_BIND_MW = 8
    IBV_WR_SEND_WITH_INV = 9
    IBV_WR_TSO = 10
    IBV_WR_DRIVER1 = 11


class WorkReqSendFlag:
    IBV_SEND_NO_FLAGS = 0  # Not defined in rdma-core
    IBV_SEND_FENCE = 1
    IBV_SEND_SIGNALED = 2
    IBV_SEND_SOLICITED = 4
    IBV_SEND_INLINE = 8
    IBV_SEND_IP_CSUM = 16


class RecvQueueDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 4),
                ("F_RESERVED_0", c_int, 2),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_1", c_int, 20),
                ("F_DATA_LEN", c_int, 32),
                ]


class SendQueueDesc(Structure):
    _fields_ = [("common_header", RecvQueueDescCommonHeader),
                ("F_L_ADDR", c_longlong, 64),
                ("F_RESERVED_0", c_longlong, 64),
                ("F_LKEY", c_int, 32),
                ("F_SQ_SQPN", c_int, 24),
                ("F_RESERVED_1", c_int, 8),
                ]


class CpltQueueDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_RESERVED_0", c_int, 7),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_1", c_int, 20),
                ("F_DATA_LEN", c_int, 32),
                ]


class CpltQueueDesc(Structure):
    _fields_ = [("common_header", CpltQueueDescCommonHeader),
                ("F_CQ_OPCODE", c_int, 8),
                ("F_CQ_FLAGS", c_int, 7),
                ("F_RESERVED_0", c_int, 1),
                ("F_CQ_STATUS", c_int, 5),
                ("F_RESERVED_1", c_int, 11),
                ("F_CQ_PKEY", c_int, 16),
                ("F_RESERVED_2", c_int, 16),
                ("F_RESERVED_3", c_longlong, 64),
                ("F_SQ_SQPN", c_int, 24),
                ("F_RESERVED_4", c_longlong, 40),
                ]


cmd_queue_common_header = CmdQueueReqDescCommonHeader(
    F_VALID=1,
    F_SEGMENT_CNT=0,
    F_SIGNAL_CPLT=0,
)

memory = bytearray(TOTAL_MEMORY_SIZE)

# generate pgt first level modify descriptor

cmd_queue_desc_current_write_addr = CMD_QUEUE_H2C_RINGBUF_START_PA

cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_FIRST_STAGE_PGT
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x00
obj = CmdQueueDescUpdateFirstStagePGT(
    common_header=cmd_queue_common_header,
    F_PGT_FIRST_STAGE_BASE_VA=PGT_MR_VASE_VA,
    F_PGT_FIRST_STAGE_POINTED_TO_OFFSET=PGT_ENTRY_OFFSET,
    F_PGT_FIRST_STAGE_POINTED_TO_COUNT=PGT_ENTRY_CNT,
    F_PGT_FIRST_STAGE_INDEX=0x00
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

# generate pgt second level modify descriptor

cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_SECOND_STAGE_PGT
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x01
obj = CmdQueueDescUpdateSecondStagePGT(
    common_header=cmd_queue_common_header,
    F_PGT_SECOND_STAGE_SRC_DATA_ADDR=PGT_TABLE_START_ADDR,
    F_PGT_SECOND_STAGE_OFFSET=PGT_ENTRY_OFFSET,
    F_PGT_SECOND_STAGE_SRC_DATA_LEN=PGT_ENTRY_SIZE * PGT_ENTRY_CNT,
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

# generate create PD request
cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_PD
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x02
obj = CmdQueueDescPdManagement(
    common_header=cmd_queue_common_header,
    F_PD_ADMIN_PD_HANDLER=0x6611,
    F_PD_ADMIN_IS_ALLOC=1,
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

# generate create MR request
pd_handler = 0x6611  # in practise, this should be returned by hardware

cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_MR
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x03
cmd_queue_common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescMrManagementSeg0(
    common_header=cmd_queue_common_header,
    F_MR_ADMIN_PD_HANDLER=pd_handler,
    F_MR_ADMIN_IS_ALLOC=1,
    F_MR_ADMIN_ADDR=PGT_MR_VASE_VA,
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

obj = CmdQueueDescMrManagementSeg1(
    F_MR_ADMIN_LKEY=SEND_SIDE_LKEY,
    F_MR_ADMIN_RKEY=SEND_SIDE_RKEY,
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20


# generate create QP request

cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x04
cmd_queue_common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescQpManagementSeg0(
    common_header=cmd_queue_common_header,
    F_QP_ADMIN_PD_HANDLER=pd_handler,
    F_QP_ADMIN_REQ_TYPE=QpReqType.REQ_QP_CREATE,
    F_QP_ADMIN_QP_TYPE=TypeQP.IBV_QPT_RC,
    F_QP_ADMIN_SQ_SIG_ALL=1,
)
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

obj = CmdQueueDescQpManagementSeg1()
memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20


# generate QP modify to init request
cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x05
cmd_queue_common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescQpManagementSeg0(
    common_header=cmd_queue_common_header,
    F_QP_ADMIN_PD_HANDLER=pd_handler,
    F_QP_ADMIN_REQ_TYPE=QpReqType.REQ_QP_MODIFY,
    F_QP_ADMIN_ATTR_MASK=QpAttrMaskFlag.IBV_QP_STATE | QpAttrMaskFlag.IBV_QP_ACCESS_FLAGS | QpAttrMaskFlag.IBV_QP_PKEY_INDEX,
)
memcpy(memory,   cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

obj = CmdQueueDescQpManagementSeg1(
    F_QP_ADMIN_QP_STATE=StateQP.IBV_QPS_INIT,
    F_QP_ADMIN_ACCESS_FLAG=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE,
    F_QP_ADMIN_PKEY_INDEX=PKEY_INDEX,

)
memcpy(memory,  cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20


# generate QP modify to RTR request
cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x06
cmd_queue_common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescQpManagementSeg0(
    common_header=cmd_queue_common_header,
    F_QP_ADMIN_PD_HANDLER=pd_handler,
    F_QP_ADMIN_REQ_TYPE=QpReqType.REQ_QP_MODIFY,
    F_QP_ADMIN_ATTR_MASK=(
        QpAttrMaskFlag.IBV_QP_STATE | QpAttrMaskFlag.IBV_QP_PATH_MTU | QpAttrMaskFlag.IBV_QP_RQ_PSN |
        QpAttrMaskFlag.IBV_QP_DEST_QPN | QpAttrMaskFlag.IBV_QP_MAX_DEST_RD_ATOMIC | QpAttrMaskFlag.IBV_QP_MIN_RNR_TIMER),
)
memcpy(memory,   cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

obj = CmdQueueDescQpManagementSeg1(
    F_QP_ADMIN_QP_STATE=StateQP.IBV_QPS_RTR,
    F_QP_ADMIN_PMTU=PMTU.IBV_MTU_1024,
    F_QP_ADMIN_RQ_PSN=1,
    F_QP_ADMIN_DQPN=1,
    F_QP_ADMIN_MAX_DEST_READ_ATOMIC=1,
    F_QP_ADMIN_MIN_RNR_TIMER=1,
)
memcpy(memory,  cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20


# generate QP modify to RTS request
cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x07
cmd_queue_common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescQpManagementSeg0(
    common_header=cmd_queue_common_header,
    F_QP_ADMIN_PD_HANDLER=pd_handler,
    F_QP_ADMIN_REQ_TYPE=QpReqType.REQ_QP_MODIFY,
    F_QP_ADMIN_ATTR_MASK=(
        QpAttrMaskFlag.IBV_QP_STATE | QpAttrMaskFlag.IBV_QP_TIMEOUT | QpAttrMaskFlag.IBV_QP_RETRY_CNT |
        QpAttrMaskFlag.IBV_QP_RNR_RETRY | QpAttrMaskFlag.IBV_QP_SQ_PSN | QpAttrMaskFlag.IBV_QP_MAX_QP_RD_ATOMIC),
)
memcpy(memory,   cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20

obj = CmdQueueDescQpManagementSeg1(
    F_QP_ADMIN_QP_STATE=StateQP.IBV_QPS_RTS,
    F_QP_ADMIN_MAX_READ_ATOMIC=1,
    F_QP_ADMIN_TIMEOUT=1,
    F_QP_ADMIN_RETRY_CNT=1,
    F_QP_ADMIN_RNR_RETRY=1,
)
memcpy(memory,  cmd_queue_desc_current_write_addr, bytes(obj))
cmd_queue_desc_current_write_addr += 0x20


# generate second level PGT entry
PgtEntries = c_longlong * 0x20
entries = PgtEntries()


for i in range(len(entries)):
    entries[i] = MR_0_PA_START + i * (1 << 21)  # 2MB page size

memcpy(memory, PGT_TABLE_START_ADDR, bytes(entries))


# generate send queue WQE
send_queue_desc_current_write_addr = SQ_RINGBUF_START_PA
send_queue_common_header = SendQueueDescCommonHeader(
    F_VALID=1,
    F_OP_CODE=WorkReqOpCode.IBV_WR_RDMA_READ,
    F_SEGMENT_CNT=1,
    F_SIGNAL_CPLT=0,
    F_DATA_LEN=0xF000
)

obj = SendQueueDescSeg0(
    common_header=send_queue_common_header,
    F_L_ADDR=REQ_SIDE_VA_ADDR,
    F_R_ADDR=RESP_SIDE_VA_ADDR,
    F_LKEY=SEND_SIDE_LKEY,
    F_RKEY=SEND_SIDE_RKEY,
)
memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
send_queue_desc_current_write_addr += 0x20

obj = SendQueueDescSeg1(
    F_SQ_SEND_FLAG=WorkReqSendFlag.IBV_SEND_NO_FLAGS,
    F_SQ_SOLICITED=0,
    F_SQ_SQPN=1,
)
memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
send_queue_desc_current_write_addr += 0x20

dump_to_str(memory)

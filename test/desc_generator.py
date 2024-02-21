# coding: utf-8

import subprocess
from ctypes import *
from itertools import islice

TOTAL_MEMORY_SIZE = 1024 * 1024 * 64

DESCRIPTOR_SIZE = 0x20

PGT_ENTRY_OFFSET = 0x200
PGT_ENTRY_CNT = 0x20
PGT_ENTRY_SIZE = 0x08
# PGT_MR0_BASE_VA = 0xFBABCDCEEEEE0001
PGT_MR0_BASE_VA = 0x0000000000000000

CMD_QUEUE_H2C_RINGBUF_START_PA = 0x00
CMD_QUEUE_C2H_RINGBUF_START_PA = 0x1000
RQ_RINGBUF_START_PA = 0x2000
SQ_RINGBUF_START_PA = 0x3000
RCQ_RINGBUF_START_PA = 0x4000
SCQ_RINGBUF_START_PA = 0x5000

PGT_TABLE_START_ADDR = 0x10000

HUGEPAGE_2M_ADDR_MASK = 0xFFFFFFFFFFE00000
HUGEPAGE_2M_BYTE_CNT = 0x200000

# MR_0_PA_START = 0x100000
MR_0_PA_START = 0x0

MR_0_PTE_COUNT = 0x20
MR_0_LENGTH = MR_0_PTE_COUNT * HUGEPAGE_2M_BYTE_CNT
print("MR_0_LENGTH=", hex(MR_0_LENGTH))


print("PGT_MR0_BASE_VA=", hex(PGT_MR0_BASE_VA))
# REQ_SIDE_VA_ADDR = (PGT_MR0_BASE_VA & HUGEPAGE_2M_ADDR_MASK) + 0x1FFFFE
REQ_SIDE_VA_ADDR = (PGT_MR0_BASE_VA & HUGEPAGE_2M_ADDR_MASK) + 0x200000
print("REQ_SIDE_VA_ADDR=", hex(REQ_SIDE_VA_ADDR))
RESP_SIDE_VA_ADDR = (PGT_MR0_BASE_VA & HUGEPAGE_2M_ADDR_MASK) + 0x90000
print("RESP_SIDE_VA_ADDR=", hex(RESP_SIDE_VA_ADDR))

SEND_SIDE_LKEY = 0x6622
SEND_SIDE_RKEY = SEND_SIDE_LKEY
RECV_SIDE_LKEY = 0x6622
RECV_SIDE_RKEY = RECV_SIDE_LKEY
PKEY_INDEX = 0

SEND_SIDE_QPN = 0x6611
SEND_SIDE_PD_HANDLER = 0x6611  # in practise, this should be returned by hardware


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


class CmdQueueDescUpdateMrTable(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_MR_TABLE_MR_BASE_VA", c_longlong),
                ("F_MR_TABLE_MR_LENGTH", c_int, 32),
                ("F_MR_TABLE_MR_KEY", c_int, 32),
                ("F_MR_TABLE_PD_HANDLER", c_int, 32),
                ("F_MR_TABLE_ACC_FLAGS", c_int, 8),
                ("F_MR_TABLE_PGT_OFFSET", c_int, 17),
                ("F_RESERVED_0", c_int, 7),
                ]


class CmdQueueDescUpdatePGT(Structure):
    _fields_ = [("common_header", CmdQueueReqDescCommonHeader),
                ("F_PGT_DMA_ADDR", c_longlong),
                ("F_PGT_START_INDEX", c_int, 32),
                ("F_PGT_DMA_READ_LENGTH", c_int, 32),
                ("F_RESERVED_0", c_longlong),
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
                ("F_QP_ADMIN_IS_VALID", c_int, 1),
                ("F_QP_ADMIN_IS_ERROR", c_int, 1),
                ("F_RESERVED_0", c_int, 6),
                ("F_QP_ADMIN_QPN", c_int, 24),
                ("F_QP_ADMIN_PD_HANDLER", c_int, 32),
                ("F_QP_ADMIN_QP_TYPE", c_int, 4),
                ("F_RESERVED_1", c_int, 4),
                ("F_QP_ADMIN_ACCESS_FLAG", c_int, 8),
                ("F_QP_ADMIN_PMTU", c_int, 3),
                ("F_RESERVED_2", c_longlong, 45),
                ("F_RESERVED_3", c_longlong, 64),
                ]


class CmdQueueDescOperators:
    F_OPCODE_CMDQ_UPDATE_MR_TABLE = 0x00
    F_OPCODE_CMDQ_UPDATE_PGT = 0x01
    F_OPCODE_CMDQ_MANAGE_QP = 0x02


class SendQueueDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 4),
                ("F_IS_LAST", c_int, 1),
                ("F_IS_FIRST", c_int, 1),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_1", c_int, 20),
                ("F_TOTAL_LEN", c_int, 32),
                ]


class SendQueueDescSeg0(Structure):
    _fields_ = [("common_header", SendQueueDescCommonHeader),
                ("F_R_ADDR", c_longlong, 64),
                ("F_RKEY", c_int, 32),
                ("F_DST_IP", c_int, 32),
                ("F_RESERVED_1", c_longlong, 64),
                ]


class SendQueueDescSeg1(Structure):
    _fields_ = [("F_PMTU", c_int, 3),
                ("F_RESERVED_8", c_int, 5),

                ("F_FLAGS", c_int, 5),
                ("F_RESERVED_7", c_int, 3),

                ("F_QP_TYPE", c_int, 4),
                ("F_RESERVED_6", c_int, 4),

                ("F_SEG_CNT", c_int, 3),
                ("F_RESERVED_5", c_int, 5),


                ("F_PSN", c_int, 24),
                ("F_RESERVED_4", c_int, 8),

                ("F_MAC_ADDR", c_longlong, 48),
                ("F_RESERVED_3", c_int, 16),

                ("F_DQPN", c_int, 24),
                ("F_RESERVED_2", c_int, 8),

                ("F_IMM", c_int, 32),

                ("F_RESERVED_1", c_longlong, 64),


                ]


class SendQueueReqDescFragSGE(Structure):
    _fields_ = [("F_LKEY", c_int, 32),
                ("F_LEN", c_int, 32),
                ("F_LADDR", c_longlong, 64),
                ]


class SendQueueReqDescVariableLenSGE(Structure):
    _fields_ = [("F_SGE2", SendQueueReqDescFragSGE),
                ("F_SGE1", SendQueueReqDescFragSGE),
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


def generate_memory_image():

    cmd_queue_common_header = CmdQueueReqDescCommonHeader(
        F_VALID=1,
        F_SEGMENT_CNT=0,
        F_SIGNAL_CPLT=0,
    )

    memory = bytearray(TOTAL_MEMORY_SIZE)

    PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_256

    # generate create MR request

    cmd_queue_desc_current_write_addr = CMD_QUEUE_H2C_RINGBUF_START_PA

    cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_MR_TABLE
    cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x00
    obj = CmdQueueDescUpdateMrTable(
        common_header=cmd_queue_common_header,
        F_MR_TABLE_MR_BASE_VA=PGT_MR0_BASE_VA,
        F_MR_TABLE_MR_LENGTH=MR_0_LENGTH,
        F_MR_TABLE_MR_KEY=SEND_SIDE_LKEY,
        F_MR_TABLE_PD_HANDLER=SEND_SIDE_PD_HANDLER,
        F_MR_TABLE_ACC_FLAGS=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
        F_MR_TABLE_PGT_OFFSET=PGT_ENTRY_OFFSET,
    )
    memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
    cmd_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    # generate pgt modify descriptor

    cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_PGT
    cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x01
    obj = CmdQueueDescUpdatePGT(
        common_header=cmd_queue_common_header,
        F_PGT_DMA_ADDR=PGT_TABLE_START_ADDR,
        F_PGT_START_INDEX=PGT_ENTRY_OFFSET,
        F_PGT_DMA_READ_LENGTH=PGT_ENTRY_SIZE * PGT_ENTRY_CNT,
    )
    memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
    cmd_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    # generate update QP context request

    cmd_queue_common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
    cmd_queue_common_header.F_CMD_QUEUE_USER_DATA = 0x04
    obj = CmdQueueDescQpManagementSeg0(
        common_header=cmd_queue_common_header,
        F_QP_ADMIN_IS_VALID=True,
        F_QP_ADMIN_IS_ERROR=False,
        F_QP_ADMIN_QPN=SEND_SIDE_QPN,
        F_QP_ADMIN_PD_HANDLER=SEND_SIDE_PD_HANDLER,
        F_QP_ADMIN_QP_TYPE=TypeQP.IBV_QPT_RC,
        F_QP_ADMIN_ACCESS_FLAG=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
        F_QP_ADMIN_PMTU=PMTU_VALUE_FOR_TEST,
    )
    memcpy(memory, cmd_queue_desc_current_write_addr, bytes(obj))
    cmd_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    # generate second level PGT entry
    PgtEntries = c_longlong * MR_0_PTE_COUNT
    entries = PgtEntries()

    for i in range(len(entries)):
        entries[i] = MR_0_PA_START + i * (1 << 21)  # 2MB page size

    memcpy(memory, PGT_TABLE_START_ADDR, bytes(entries))

    # generate send queue WQE
    send_queue_desc_current_write_addr = SQ_RINGBUF_START_PA
    send_queue_common_header = SendQueueDescCommonHeader(
        F_VALID=1,
        F_OP_CODE=WorkReqOpCode.IBV_WR_RDMA_WRITE,
        F_IS_LAST=1,
        F_IS_FIRST=1,
        F_SEGMENT_CNT=3,
        F_SIGNAL_CPLT=1,
        F_TOTAL_LEN=0x0003 + 1
    )

    obj = SendQueueDescSeg0(
        common_header=send_queue_common_header,
        F_R_ADDR=RESP_SIDE_VA_ADDR,
        F_RKEY=SEND_SIDE_RKEY,
        F_DST_IP=0x11223344,
    )
    memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
    send_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    obj = SendQueueDescSeg1(
        F_IMM=1,
        F_DQPN=0x6611,
        F_MAC_ADDR=0xAABBCCDDEEFF,
        F_PSN=0x22,
        F_SEG_CNT=4,
        F_QP_TYPE=TypeQP.IBV_QPT_RC,
        F_FLAGS=WorkReqSendFlag.IBV_SEND_NO_FLAGS,
        F_PMTU=PMTU_VALUE_FOR_TEST
    )
    memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
    send_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    sge1 = SendQueueReqDescFragSGE(
        F_LKEY=SEND_SIDE_LKEY,
        F_LEN=1,
        F_LADDR=REQ_SIDE_VA_ADDR
    )
    sge2 = SendQueueReqDescFragSGE(
        F_LKEY=SEND_SIDE_LKEY,
        F_LEN=1,
        F_LADDR=REQ_SIDE_VA_ADDR+1
    )

    obj = SendQueueReqDescVariableLenSGE(
        F_SGE1=sge1,
        F_SGE2=sge2
    )
    memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
    send_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    sge1 = SendQueueReqDescFragSGE(
        F_LKEY=SEND_SIDE_LKEY,
        F_LEN=1,
        F_LADDR=REQ_SIDE_VA_ADDR+2
    )
    sge2 = SendQueueReqDescFragSGE(
        F_LKEY=SEND_SIDE_LKEY,
        F_LEN=1,
        F_LADDR=REQ_SIDE_VA_ADDR+3
    )

    obj = SendQueueReqDescVariableLenSGE(
        F_SGE1=sge1,
        F_SGE2=sge2
    )
    memcpy(memory, send_queue_desc_current_write_addr, bytes(obj))
    send_queue_desc_current_write_addr += DESCRIPTOR_SIZE

    # Put some data at send side memory to debug
    memory[REQ_SIDE_VA_ADDR] = 0xBB
    memory[REQ_SIDE_VA_ADDR+1] = 0xCC
    memory[REQ_SIDE_VA_ADDR+2] = 0xDD
    memory[REQ_SIDE_VA_ADDR+3] = 0xEE

    return memory

# coding: utf-8

from ctypes import *
from itertools import islice

TOTAL_MEMORY_SIZE = 1024 * 1024 * 64

CMD_QUEUE_H2C_RINGBUF_START_PA = 0x00
CMD_QUEUE_C2H_RINGBUF_START_PA = 0x1000


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


common_header = CmdQueueReqDescCommonHeader(
    F_VALID=1,
    F_SEGMENT_CNT=0,
    F_SIGNAL_CPLT=0,
)

memory = bytearray(TOTAL_MEMORY_SIZE)

# generate first level modify descriptor
PGT_ENTRY_OFFSET = 0x200
PGT_ENTRY_CNT = 0x20
PGT_ENTRY_SIZE = 0x08
PGT_MR_VASE_VA = 0xFBABCDCEEEEE0001

common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_FIRST_STAGE_PGT
common_header.F_CMD_QUEUE_USER_DATA = 0x00
obj = CmdQueueDescUpdateFirstStagePGT(
    common_header=common_header,
    F_PGT_FIRST_STAGE_BASE_VA=PGT_MR_VASE_VA,
    F_PGT_FIRST_STAGE_POINTED_TO_OFFSET=PGT_ENTRY_OFFSET,
    F_PGT_FIRST_STAGE_POINTED_TO_COUNT=PGT_ENTRY_CNT,
    F_PGT_FIRST_STAGE_INDEX=0x00
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0, bytes(obj))

# generate second level modify descriptor
PGT_TABLE_START_ADDR = 0x100

common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_SECOND_STAGE_PGT
common_header.F_CMD_QUEUE_USER_DATA = 0x01
obj = CmdQueueDescUpdateSecondStagePGT(
    common_header=common_header,
    F_PGT_SECOND_STAGE_SRC_DATA_ADDR=PGT_TABLE_START_ADDR,
    F_PGT_SECOND_STAGE_OFFSET=PGT_ENTRY_OFFSET,
    F_PGT_SECOND_STAGE_SRC_DATA_LEN=PGT_ENTRY_SIZE * PGT_ENTRY_CNT,
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0x20, bytes(obj))

# generate create PD request
common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_PD
common_header.F_CMD_QUEUE_USER_DATA = 0x02
obj = CmdQueueDescPdManagement(
    common_header=common_header,
    F_PD_ADMIN_PD_HANDLER=0x6611,
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0x40, bytes(obj))

# generate create MR request
pd_handler = 0x6611  # in practise, this should be returned by hardware

common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_MR
common_header.F_CMD_QUEUE_USER_DATA = 0x03
common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescMrManagementSeg0(
    common_header=common_header,
    F_MR_ADMIN_PD_HANDLER=pd_handler,
    F_MR_ADMIN_IS_ALLOC=1,
    F_MR_ADMIN_ADDR=PGT_MR_VASE_VA,
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0x60, bytes(obj))

obj = CmdQueueDescMrManagementSeg1(
    F_MR_ADMIN_LKEY=0x6622,
    F_MR_ADMIN_RKEY=0x6633,
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0x80, bytes(obj))


# generate create QP request

common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP
common_header.F_CMD_QUEUE_USER_DATA = 0x04
common_header.F_SEGMENT_CNT = 1
obj = CmdQueueDescQpManagementSeg0(
    common_header=common_header,
    F_QP_ADMIN_PD_HANDLER=pd_handler,
)
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0xA0, bytes(obj))

obj = CmdQueueDescQpManagementSeg1()
memcpy(memory, CMD_QUEUE_H2C_RINGBUF_START_PA + 0xC0, bytes(obj))


# generate second level PGT entry
PgtEntries = c_longlong * 0x20
entries = PgtEntries()

for i in range(len(entries)):
    entries[i] = 0xFFBBFFBBFFBBFF00 + i

memcpy(memory, PGT_TABLE_START_ADDR, bytes(entries))

dump_to_str(memory)

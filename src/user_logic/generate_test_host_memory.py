# coding: utf-8

from ctypes import *
from itertools import islice

TOTAL_MEMORY_SIZE = 1024 * 1024 * 64


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


class CmdQueueDescCommonHeader(Structure):
    _fields_ = [("F_VALID", c_int, 1),
                ("F_OP_CODE", c_int, 6),
                ("F_SEGMENT_CNT", c_int, 4),
                ("F_SIGNAL_CPLT", c_int, 1),
                ("F_RESERVED_0", c_int, 20),
                ("F_CMD_QUEUE_USER_DATA", c_int, 32),
                ]


class CmdQueueDescUpdateFirstStagePGT(Structure):
    _fields_ = [("common_header", CmdQueueDescCommonHeader),
                ("F_PGT_FIRST_STAGE_BASE_VA", c_longlong),
                ("F_PGT_FIRST_STAGE_POINTED_TO_OFFSET", c_int, 32),
                ("F_PGT_FIRST_STAGE_POINTED_TO_COUNT", c_int, 32),
                ("F_PGT_FIRST_STAGE_INDEX", c_int, 32),
                ("F_RESERVED_0", c_int, 32),
                ]


class CmdQueueDescUpdateSecondStagePGT(Structure):
    _fields_ = [("common_header", CmdQueueDescCommonHeader),
                ("F_PGT_SECOND_STAGE_SRC_DATA_ADDR", c_longlong),
                ("F_PGT_SECOND_STAGE_OFFSET", c_int, 32),
                ("F_PGT_SECOND_STAGE_SRC_DATA_LEN", c_int, 32),
                ("F_RESERVED_0", c_longlong),
                ]


class CmdQueueDescOperators:
    F_OPCODE_CMDQ_UPDATE_FIRST_STAGE_PGT = 0x00
    F_OPCODE_CMDQ_UPDATE_SECOND_STAGE_PGT = 0x01


common_header = CmdQueueDescCommonHeader(
    F_VALID=1,
    F_SEGMENT_CNT=0,
    F_SIGNAL_CPLT=0,
)

memory = bytearray(TOTAL_MEMORY_SIZE)

# generate first level modify descriptor
PGT_ENTRY_OFFSET = 0x200
PGT_ENTRY_CNT = 0x20
PGT_ENTRY_SIZE = 0x08

common_header.F_OP_CODE = CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_FIRST_STAGE_PGT
common_header.F_CMD_QUEUE_USER_DATA = 0x00
obj = CmdQueueDescUpdateFirstStagePGT(
    common_header=common_header,
    F_PGT_FIRST_STAGE_BASE_VA=0xFBABCDCEEEEE0001,
    F_PGT_FIRST_STAGE_POINTED_TO_OFFSET=PGT_ENTRY_OFFSET,
    F_PGT_FIRST_STAGE_POINTED_TO_COUNT=PGT_ENTRY_CNT,
    F_PGT_FIRST_STAGE_INDEX=0x00
)
memcpy(memory, 0, bytes(obj))

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
memcpy(memory, 0x20, bytes(obj))


# generate second level PGT entry
PgtEntries = c_longlong * 0x20
entries = PgtEntries()

for i in range(len(entries)):
    entries[i] = 0xFFBBFFBBFFBBFF00 + i

memcpy(memory, PGT_TABLE_START_ADDR, bytes(entries))

dump_to_str(memory)

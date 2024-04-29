def generate_ringbuf_csr_addr(is_h2c, queue_index, reg_index):
    a = 1 if is_h2c else 0
    a <<= 3
    a |= queue_index & 0b111
    a <<= 10
    a |= reg_index & 0x3FF
    a <<= 2
    return a

def generate_csr_addr(group_index, group_offset):
    a = group_index
    a <<= 10
    a |= group_offset
    a <<= 2
    return a


class CsrGroupIndex:
    C2hQueue0 = 0x000
    C2hQueue1 = 0x001
    H2cQueue0 = 0x008
    H2cQueue1 = 0x009
    HardwareConsts = 0x020
    MaxGuard = 0x0FF

class CsrGroupOffsetForHardwareConst:
    HwVersion = 0x0
    MaxGuard = 0x3FF

class CsrGroupOffsetForRingbuf:
    BaseAddrLow = 0x0
    BaseAddrHigh = 0x1
    HeadPointer = 0x2
    TailPointer = 0x3
    MaxGuard = 0x3FF

CSR_ADDR_CMD_REQ_QUEUE_ADDR_LOW = generate_ringbuf_csr_addr(
    True, 0, CsrGroupOffsetForRingbuf.BaseAddrLow)
CSR_ADDR_CMD_REQ_QUEUE_ADDR_HIGH = generate_ringbuf_csr_addr(
    True, 0, CsrGroupOffsetForRingbuf.BaseAddrHigh)
CSR_ADDR_CMD_REQ_QUEUE_HEAD = generate_ringbuf_csr_addr(True, 0, CsrGroupOffsetForRingbuf.HeadPointer)
CSR_ADDR_CMD_REQ_QUEUE_TAIL = generate_ringbuf_csr_addr(True, 0, CsrGroupOffsetForRingbuf.TailPointer)

CSR_ADDR_CMD_RESP_QUEUE_HEAD = generate_ringbuf_csr_addr(False, 0, CsrGroupOffsetForRingbuf.HeadPointer)
CSR_ADDR_CMD_RESP_QUEUE_TAIL = generate_ringbuf_csr_addr(False, 0, CsrGroupOffsetForRingbuf.TailPointer)
CSR_ADDR_CMD_RESP_QUEUE_ADDR_LOW = generate_ringbuf_csr_addr(
    False, 0, CsrGroupOffsetForRingbuf.BaseAddrLow)
CSR_ADDR_CMD_RESP_QUEUE_ADDR_HIGH = generate_ringbuf_csr_addr(
    False, 0, CsrGroupOffsetForRingbuf.BaseAddrHigh)

CSR_ADDR_SEND_QUEUE_HEAD = generate_ringbuf_csr_addr(True, 1, CsrGroupOffsetForRingbuf.HeadPointer)
CSR_ADDR_SEND_QUEUE_TAIL = generate_ringbuf_csr_addr(True, 1, CsrGroupOffsetForRingbuf.TailPointer)
CSR_ADDR_SEND_QUEUE_ADDR_LOW = generate_ringbuf_csr_addr(
    True, 1, CsrGroupOffsetForRingbuf.BaseAddrLow)
CSR_ADDR_SEND_QUEUE_ADDR_HIGH = generate_ringbuf_csr_addr(
    True, 1, CsrGroupOffsetForRingbuf.BaseAddrHigh)


CSR_ADDR_META_REPORT_QUEUE_HEAD = generate_ringbuf_csr_addr(False, 1, CsrGroupOffsetForRingbuf.HeadPointer)
CSR_ADDR_META_REPORT_QUEUE_TAIL = generate_ringbuf_csr_addr(False, 1, CsrGroupOffsetForRingbuf.TailPointer)
CSR_ADDR_META_REPORT_QUEUE_ADDR_LOW = generate_ringbuf_csr_addr(
    False, 1, CsrGroupOffsetForRingbuf.BaseAddrLow)
CSR_ADDR_META_REPORT_QUEUE_ADDR_HIGH = generate_ringbuf_csr_addr(
    False, 1, CsrGroupOffsetForRingbuf.BaseAddrHigh)


CSR_ADDR_HARDWARE_CONST_HW_VERSION = generate_csr_addr(CsrGroupIndex.HardwareConsts, CsrGroupOffsetForHardwareConst.HwVersion)
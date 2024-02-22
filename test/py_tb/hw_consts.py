def generate_csr_addr(is_h2c, queue_index, reg_index):
    a = 1 if is_h2c else 0
    a <<= 3
    a |= queue_index & 0b111
    a <<= 10
    a |= reg_index & 0x3FF
    a <<= 2
    return a


CsrIdxRbBaseAddrLow = 0x0
CsrIdxRbBaseAddrHigh = 0x1
CsrIdxRbHead = 0x2
CsrIdxRbTail = 0x3

CSR_ADDR_CMD_REQ_QUEUE_ADDR_LOW = generate_csr_addr(
    True, 0, CsrIdxRbBaseAddrLow)
CSR_ADDR_CMD_REQ_QUEUE_ADDR_HIGH = generate_csr_addr(
    True, 0, CsrIdxRbBaseAddrHigh)
CSR_ADDR_CMD_REQ_QUEUE_HEAD = generate_csr_addr(True, 0, CsrIdxRbHead)
CSR_ADDR_CMD_REQ_QUEUE_TAIL = generate_csr_addr(True, 0, CsrIdxRbTail)

CSR_ADDR_CMD_RESP_QUEUE_HEAD = generate_csr_addr(False, 0, CsrIdxRbHead)
CSR_ADDR_CMD_RESP_QUEUE_TAIL = generate_csr_addr(False, 0, CsrIdxRbTail)
CSR_ADDR_CMD_RESP_QUEUE_ADDR_LOW = generate_csr_addr(
    False, 0, CsrIdxRbBaseAddrLow)
CSR_ADDR_CMD_RESP_QUEUE_ADDR_HIGH = generate_csr_addr(
    False, 0, CsrIdxRbBaseAddrHigh)

CSR_ADDR_SEND_QUEUE_HEAD = generate_csr_addr(True, 1, CsrIdxRbHead)
CSR_ADDR_SEND_QUEUE_TAIL = generate_csr_addr(True, 1, CsrIdxRbTail)
CSR_ADDR_SEND_QUEUE_ADDR_LOW = generate_csr_addr(
    True, 1, CsrIdxRbBaseAddrLow)
CSR_ADDR_SEND_QUEUE_ADDR_HIGH = generate_csr_addr(
    True, 1, CsrIdxRbBaseAddrHigh)


CSR_ADDR_META_REPORT_QUEUE_HEAD = generate_csr_addr(False, 1, CsrIdxRbHead)
CSR_ADDR_META_REPORT_QUEUE_TAIL = generate_csr_addr(False, 1, CsrIdxRbTail)
CSR_ADDR_META_REPORT_QUEUE_ADDR_LOW = generate_csr_addr(
    False, 1, CsrIdxRbBaseAddrLow)
CSR_ADDR_META_REPORT_QUEUE_ADDR_HIGH = generate_csr_addr(
    False, 1, CsrIdxRbBaseAddrHigh)

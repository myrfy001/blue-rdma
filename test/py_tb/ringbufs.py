import time

from desccriptors import *
from hw_consts import *


class Ringbuf:
    def __init__(self, backend_mem, mock_host, is_h2c, head_csr_addr, tail_csr_addr, desc_size=32, ringbuf_len=128) -> None:
        if not is_power_of_2(desc_size):
            raise ("desc_size must be power of 2")
        if not is_power_of_2(ringbuf_len):
            raise ("ringbuf_len must be power of 2")

        self.backend_mem = backend_mem
        self.head = 0
        self.tail = 0
        self.desc_size = desc_size
        self.ringbuf_len = ringbuf_len
        self.ringbuf_idx_mask = ringbuf_len - 1
        self.mock_host = mock_host
        self.is_h2c = is_h2c
        self.head_csr_addr = head_csr_addr
        self.tail_csr_addr = tail_csr_addr

    def sync_pointers(self):
        if (self.is_h2c):
            self.mock_host.write_csr_blocking(self.head_csr_addr, self.head)
            new_tail = self.mock_host.read_csr_blocking(self.tail_csr_addr)
            self.set_tail_pointer_with_guard_bit(new_tail)
        else:
            self.mock_host.write_csr_blocking(self.tail_csr_addr, self.tail)
            new_head = self.mock_host.read_csr_blocking(self.head_csr_addr)
            self.set_head_pointer_with_guard_bit(new_head)

    def is_full(self):
        is_guard_bit_same = (
            self.head ^ self.tail) & self.ringbuf_len != self.ringbuf_len

        head_idx = self.head & self.ringbuf_idx_mask
        tail_idx = self.tail & self.ringbuf_idx_mask
        return (head_idx == tail_idx) and (not is_guard_bit_same)

    def is_empty(self):
        is_guard_bit_same = (
            self.head ^ self.tail) & self.ringbuf_len != self.ringbuf_len

        head_idx = self.head & self.ringbuf_idx_mask
        tail_idx = self.tail & self.ringbuf_idx_mask
        return (head_idx == tail_idx) and (is_guard_bit_same)

    def set_head_pointer_with_guard_bit(self, head):
        self.head = head

    def set_tail_pointer_with_guard_bit(self, tail):
        self.tail = tail

    def enq(self, element):
        if self.is_full():
            raise Exception("Ringbuf Full")

        raw_element = bytes(element)
        if len(raw_element) != self.desc_size:
            raise Exception("Descriptor size is not ", self.desc_size)

        head_idx = self.head & self.ringbuf_idx_mask
        self.head += 1
        write_start_addr = head_idx * self.desc_size

        self.backend_mem[write_start_addr: write_start_addr +
                         self.desc_size] = raw_element

    def deq(self):
        if self.is_empty():
            raise Exception("Ringbuf Empty")
        tail_idx = self.tail & self.ringbuf_idx_mask
        self.tail += 1
        read_start_addr = tail_idx * self.desc_size
        raw_element = self.backend_mem[read_start_addr: read_start_addr +
                                       self.desc_size]

        return raw_element

    def deq_blocking(self):
        while self.is_empty():
            self.sync_pointers()
            time.sleep(0.001)
        return self.deq()


class RingbufCommandReqQueue:
    def __init__(self, backend_mem, addr, mock_host) -> None:
        self.rb = Ringbuf(backend_mem=backend_mem.buf[addr:], mock_host=mock_host, is_h2c=True,
                          head_csr_addr=CSR_ADDR_CMD_REQ_QUEUE_HEAD, tail_csr_addr=CSR_ADDR_CMD_REQ_QUEUE_TAIL)
        mock_host.write_csr_blocking(
            CSR_ADDR_CMD_REQ_QUEUE_ADDR_LOW, addr & 0xFFFFFFFF)
        mock_host.write_csr_blocking(
            CSR_ADDR_CMD_REQ_QUEUE_ADDR_HIGH, addr >> 32)

    def sync_pointers(self):
        self.rb.sync_pointers()

    def put_desc_update_mr_table(self, base_va, length, key, pd_handle, pgt_offset, acc_flag, user_data=0):
        cmd_queue_common_header = CmdQueueReqDescCommonHeader(
            F_VALID=1,
            F_SEGMENT_CNT=0,
            F_OP_CODE=CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_MR_TABLE,
            F_CMD_QUEUE_USER_DATA=user_data,
        )

        obj = CmdQueueDescUpdateMrTable(
            common_header=cmd_queue_common_header,
            F_MR_TABLE_MR_BASE_VA=base_va,
            F_MR_TABLE_MR_LENGTH=length,
            F_MR_TABLE_MR_KEY=key,
            F_MR_TABLE_PD_HANDLER=pd_handle,
            F_MR_TABLE_ACC_FLAGS=acc_flag,
            # F_MR_TABLE_ACC_FLAGS=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
            F_MR_TABLE_PGT_OFFSET=pgt_offset,
        )
        self.rb.enq(obj)

    def put_desc_update_pgt(self, dma_addr, dma_length, start_index, user_data=0):
        cmd_queue_common_header = CmdQueueReqDescCommonHeader(
            F_VALID=1,
            F_SEGMENT_CNT=0,
            F_OP_CODE=CmdQueueDescOperators.F_OPCODE_CMDQ_UPDATE_PGT,
            F_CMD_QUEUE_USER_DATA=user_data,
        )
        obj = CmdQueueDescUpdatePGT(
            common_header=cmd_queue_common_header,
            F_PGT_DMA_ADDR=dma_addr,
            F_PGT_START_INDEX=start_index,
            F_PGT_DMA_READ_LENGTH=dma_length,
        )
        self.rb.enq(obj)

    def put_desc_update_qp(self, qpn, pd_handler, qp_type, acc_flag, pmtu, user_data=0):
        cmd_queue_common_header = CmdQueueReqDescCommonHeader(
            F_VALID=1,
            F_SEGMENT_CNT=0,
            F_OP_CODE=CmdQueueDescOperators.F_OPCODE_CMDQ_MANAGE_QP,
            F_CMD_QUEUE_USER_DATA=user_data,
        )
        obj = CmdQueueDescQpManagementSeg0(
            common_header=cmd_queue_common_header,
            F_QP_ADMIN_IS_VALID=True,
            F_QP_ADMIN_IS_ERROR=False,
            F_QP_ADMIN_QPN=qpn,
            F_QP_ADMIN_PD_HANDLER=pd_handler,
            F_QP_ADMIN_QP_TYPE=qp_type,
            # F_QP_ADMIN_ACCESS_FLAG=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
            F_QP_ADMIN_ACCESS_FLAG=acc_flag,
            F_QP_ADMIN_PMTU=pmtu,
        )
        self.rb.enq(obj)

    def put_desc_set_udp_param(self, gateway, netmask, ip_addr, mac_addr, user_data=0):
        cmd_queue_common_header = CmdQueueReqDescCommonHeader(
            F_VALID=1,
            F_SEGMENT_CNT=0,
            F_OP_CODE=CmdQueueDescOperators.F_OPCODE_CMDQ_SET_NETWORK_PARAM,
            F_CMD_QUEUE_USER_DATA=user_data,
        )
        obj = CmdQueueDescSetNetworkParam(
            common_header=cmd_queue_common_header,
            F_NET_PARAM_GATEWAY=gateway,
            F_NET_PARAM_NETMASK=netmask,
            F_NET_PARAM_IPADDR=ip_addr,
            F_NET_PARAM_MACADDR=mac_addr,
        )
        self.rb.enq(obj)

    def put_desc_set_raw_packet_receive_meta(self, base_addr, mr_key, user_data=0):
        cmd_queue_common_header = CmdQueueReqDescCommonHeader(
            F_VALID=1,
            F_SEGMENT_CNT=0,
            F_OP_CODE=CmdQueueDescOperators.F_OPCODE_CMDQ_SET_RAW_PACKET_RECEIVE_META,
            F_CMD_QUEUE_USER_DATA=user_data,
        )
        obj = CmdQueueDescSetRawPacketReceiveMeta(
            common_header=cmd_queue_common_header,
            F_RAW_PACKET_META_BASE_ADDR=base_addr,
            F_RAW_PACKET_META_MR_KEY=mr_key,
        )
        self.rb.enq(obj)


class RingbufCommandRespQueue:
    def __init__(self, backend_mem, addr, mock_host) -> None:
        self.rb = Ringbuf(backend_mem=backend_mem.buf[addr:], mock_host=mock_host, is_h2c=False,
                          head_csr_addr=CSR_ADDR_CMD_RESP_QUEUE_HEAD, tail_csr_addr=CSR_ADDR_CMD_RESP_QUEUE_TAIL)
        mock_host.write_csr_blocking(
            CSR_ADDR_CMD_RESP_QUEUE_ADDR_LOW, addr & 0xFFFFFFFF)
        mock_host.write_csr_blocking(
            CSR_ADDR_CMD_RESP_QUEUE_ADDR_HIGH, addr >> 32)

    def sync_pointers(self):
        self.rb.sync_pointers()

    def deq(self):
        return self.rb.deq()

    def deq_blocking(self):
        self.rb.deq_blocking()


class RingbufSendQueue:
    def __init__(self, backend_mem, addr, mock_host) -> None:
        self.rb = Ringbuf(backend_mem=backend_mem.buf[addr:], mock_host=mock_host, is_h2c=True,
                          head_csr_addr=CSR_ADDR_SEND_QUEUE_HEAD, tail_csr_addr=CSR_ADDR_SEND_QUEUE_TAIL)
        mock_host.write_csr_blocking(
            CSR_ADDR_SEND_QUEUE_ADDR_LOW, addr & 0xFFFFFFFF)
        mock_host.write_csr_blocking(CSR_ADDR_SEND_QUEUE_ADDR_HIGH, addr >> 32)

    def sync_pointers(self):
        self.rb.sync_pointers()

    def put_work_request(self, opcode, is_first, is_last, sgl, r_va, r_key, r_ip, r_mac, dqpn, psn, qp_type=TypeQP.IBV_QPT_RC, pmtu=PMTU.IBV_MTU_256, send_flag=WorkReqSendFlag.IBV_SEND_NO_FLAGS, imm_data=0):

        total_len = sum([x.F_LEN for x in sgl])
        send_queue_common_header = SendQueueDescCommonHeader(
            F_VALID=1,
            F_OP_CODE=opcode,
            F_IS_LAST=is_last,
            F_IS_FIRST=is_first,
            F_SEGMENT_CNT=int(1 + (len(sgl)+1) / 2),
            F_TOTAL_LEN=total_len
        )

        obj = SendQueueDescSeg0(
            common_header=send_queue_common_header,
            F_R_ADDR=r_va,
            F_RKEY=r_key,
            F_DST_IP=r_ip,
        )
        self.rb.enq(obj)

        obj = SendQueueDescSeg1(
            F_IMM=imm_data,
            F_DQPN=dqpn,
            F_MAC_ADDR=r_mac,
            F_PSN=psn,
            F_SEG_CNT=len(sgl),
            F_QP_TYPE=qp_type,
            F_FLAGS=send_flag,
            F_PMTU=pmtu
        )
        self.rb.enq(obj)

        for i in range(0, len(sgl), 2):
            obj = SendQueueReqDescVariableLenSGE(F_SGE1=sgl[i])
            if i+1 < len(sgl):
                obj.F_SGE2 = sgl[i+1]
            self.rb.enq(obj)


class RingbufMetaReportQueue:
    def __init__(self, backend_mem, addr, mock_host) -> None:
        self.rb = Ringbuf(backend_mem=backend_mem.buf[addr:], mock_host=mock_host, is_h2c=False,
                          head_csr_addr=CSR_ADDR_META_REPORT_QUEUE_HEAD, tail_csr_addr=CSR_ADDR_META_REPORT_QUEUE_TAIL)
        mock_host.write_csr_blocking(
            CSR_ADDR_META_REPORT_QUEUE_ADDR_LOW, addr & 0xFFFFFFFF)
        mock_host.write_csr_blocking(
            CSR_ADDR_META_REPORT_QUEUE_ADDR_HIGH, addr >> 32)

    def sync_pointers(self):
        self.rb.sync_pointers()

    def deq(self):
        return self.rb.deq()

    def deq_blocking(self):
        self.rb.deq_blocking()

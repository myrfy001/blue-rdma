# coding:utf-8
import socket
from ctypes import *
from enum import IntEnum
from multiprocessing import shared_memory
import threading
import atexit
import time
from hw_consts import *
from ringbufs import *


# Define the types we need.
class CtypesEnum(IntEnum):
    """A ctypes-compatible IntEnum superclass."""
    @classmethod
    def from_param(cls, obj):
        return int(obj)


class CEnumRpcOpcode(CtypesEnum):
    RpcOpcodePcieBarGetReadReq = 1
    RpcOpcodePcieBarPutReadResp = 2
    RpcOpcodePcieBarGetWriteReq = 3
    RpcOpcodePcieBarPutWriteResp = 4
    RpcOpcodePcieMemWrite = 5
    RpcOpcodePcieMemRead = 6


class CStructRpcHeader(Structure):
    _fields_ = [
        ("opcode", c_int),
        ("client_id", c_longlong),
        ("tag", c_longlong),
    ]


class CStructBarIoInfo(Structure):
    _fields_ = [
        ("value", c_longlong),
        ("addr", c_longlong),
        ("valid", c_longlong),
        ("pci_tag", c_longlong),
    ]


class CStructRpcPcieBarAccessMessage(Structure):
    _fields_ = [
        ("header", CStructRpcHeader),
        ("payload", CStructBarIoInfo),
    ]


class CStructMemIoInfo(Structure):
    _fields_ = [
        ("word_addr", c_longlong),
        ("word_width", c_longlong),
        ("data", c_ubyte * 64),
        ("byte_en", c_ubyte * 8),
    ]


class CStructRpcPcieMemoryAccessMessage(Structure):
    _fields_ = [
        ("header", CStructRpcHeader),
        ("payload", CStructMemIoInfo),
    ]


class BluesimRpcServer:
    def __init__(self, lister_addr, listen_port) -> None:
        self.rpc_code_2_handler_map = {}
        self.rpc_code_2_payload_size_map = {}
        self.listen_addr = lister_addr
        self.listen_port = listen_port

    def register_opcode(self, opcode, handler, payload_size):
        self.rpc_code_2_handler_map[opcode] = handler
        self.rpc_code_2_payload_size_map[opcode] = payload_size

    def run(self):
        server_thread = threading.Thread(target=self._run)
        server_thread.start()

    def _run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((self.listen_addr, self.listen_port))
        server_socket.listen(1)

        print('TCP server started')

        raw_req_buf = bytearray(4096)

        while True:
            client_socket, client_address = server_socket.accept()
            print('Client connected:', client_address)

            while True:
                recv_pointer = 0
                recv_cnt = client_socket.recv_into(
                    raw_req_buf, sizeof(CStructRpcHeader))
                if recv_cnt != sizeof(CStructRpcHeader):
                    raise Exception("receive broken rpc header")
                rpc_header = CStructRpcHeader.from_buffer(raw_req_buf)
                remain_size = self.rpc_code_2_payload_size_map[rpc_header.opcode]
                recv_pointer = recv_cnt

                while remain_size > 0:

                    # recv_cnt = client_socket.recv_into(
                    #     raw_req_buf[recv_pointer:], remain_size)
                    t = client_socket.recv(remain_size)
                    recv_cnt = len(t)
                    raw_req_buf[recv_pointer:recv_pointer+recv_cnt] = t
                    if recv_cnt == 0:
                        raise Exception("bluesim exited, connection broken.")
                    remain_size -= recv_cnt
                    recv_pointer += recv_cnt

                self.rpc_code_2_handler_map[rpc_header.opcode](
                    client_socket, raw_req_buf)


HOST = '127.0.0.1'
PORT = 9876


class MockHostMem:
    def __init__(self, shared_mem_name, shared_mem_size) -> None:
        self.shared_mem_name = shared_mem_name
        self.shared_mem_size = shared_mem_size

        try:
            self.shared_mem_obj = shared_memory.SharedMemory(
                shared_mem_name, True, shared_mem_size)
        except FileExistsError:
            self.shared_mem_obj = shared_memory.SharedMemory(
                shared_mem_name, False, shared_mem_size)

        self.buf = self.shared_mem_obj.buf
        atexit.register(self.__deinit__)

    def __deinit__(self):
        self.shared_mem_obj.unlink()


class MockNicAndHost:
    def __init__(self, main_memory: MockHostMem) -> None:
        self.main_memory = main_memory
        self.bluesim_rpc_server = BluesimRpcServer(HOST, PORT)
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarGetReadReq, self.rpc_handler_pcie_bar_get_read_req, sizeof(CStructBarIoInfo))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarPutReadResp, self.rpc_handler_pcie_bar_put_read_resp, sizeof(CStructBarIoInfo))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarGetWriteReq, self.rpc_handler_pcie_bar_get_write_req, sizeof(CStructBarIoInfo))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarPutWriteResp, self.rpc_handler_pcie_bar_put_write_resp, sizeof(CStructBarIoInfo))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieMemWrite, self.rpc_handler_pcie_mem_write_req, sizeof(CStructMemIoInfo))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieMemRead, self.rpc_handler_pcie_mem_read_req, sizeof(CStructMemIoInfo))

        self.pending_bar_write_req = []
        self.pending_bar_write_resp = []
        self.pending_bar_read_req = []
        self.pending_bar_read_req_waiting_dict = {}
        self.pending_bar_read_resp = {}

        self.pcie_tlp_read_tag_counter = 0

    def _get_next_pcie_tlp_read_tag(self):
        if self.pcie_tlp_read_tag_counter == 32:
            self.pcie_tlp_read_tag_counter = 0
        else:
            self.pcie_tlp_read_tag_counter += 1
        return self.pcie_tlp_read_tag_counter

    def rpc_handler_pcie_bar_get_read_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieBarAccessMessage.from_buffer(raw_req_buf)
        if self.pending_bar_read_req:
            req_payload = self.pending_bar_read_req.pop(0)
            req.payload = req_payload
        else:
            req.payload.valid = 0
        client_socket.send(bytes(req))

    def rpc_handler_pcie_bar_put_read_resp(self, client_socket, raw_req_buf):
        resp = CStructRpcPcieBarAccessMessage.from_buffer(raw_req_buf)
        if resp.payload.pci_tag in self.pending_bar_read_resp:
            raise Exception(
                "pcie read tag conflict, maybe too many outstanding requests")
        self.pending_bar_read_resp[resp.payload.pci_tag] = resp
        self.pending_bar_read_req_waiting_dict[resp.payload.pci_tag].set()
        # this Op doesn't send response

    def rpc_handler_pcie_bar_get_write_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieBarAccessMessage.from_buffer(raw_req_buf)
        if self.pending_bar_write_req:
            req_payload = self.pending_bar_write_req.pop(0)
            req.payload = req_payload
        else:
            req.payload.valid = 0
        client_socket.send(bytes(req))

    def rpc_handler_pcie_bar_put_write_resp(self, client_socket, raw_req_buf):
        resp = CStructRpcPcieBarAccessMessage.from_buffer(raw_req_buf)
        # comment out the following line since we don't care write respone now
        # self.pending_bar_write_resp.append(resp)

        # this Op doesn't send response

    def rpc_handler_pcie_mem_write_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieMemoryAccessMessage.from_buffer(raw_req_buf)
        byte_cnt_per_word = req.payload.word_width >> 3
        host_mem_start_addr = req.payload.word_addr * byte_cnt_per_word
        byte_en = int.from_bytes(req.payload.byte_en, byteorder="little")
        for idx in range(byte_cnt_per_word):
            if byte_en & 0x01 == 0x01:
                self.main_memory.buf[
                    host_mem_start_addr + idx] = int(req.payload.data[idx])
            byte_en >>= 1
        # this Op doesn't send response

    def rpc_handler_pcie_mem_read_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieMemoryAccessMessage.from_buffer(raw_req_buf)
        byte_cnt_per_word = req.payload.word_width >> 3
        host_mem_start_addr = req.payload.word_addr * byte_cnt_per_word
        req.payload.data[0:byte_cnt_per_word] = self.main_memory.buf[host_mem_start_addr: host_mem_start_addr+byte_cnt_per_word]
        client_socket.send(bytes(req))

    def write_csr(self, addr, value):
        self.pending_bar_write_req.append(
            CStructBarIoInfo(valid=1, addr=addr, value=value))

    # blocking version of read_csr, wait until we get response
    def read_csr_blocking(self, addr):
        tag = self._get_next_pcie_tlp_read_tag()
        self.pending_bar_read_req.append(
            CStructBarIoInfo(valid=1, addr=addr, value=0, pci_tag=tag))
        evt = threading.Event()
        if tag in self.pending_bar_read_req_waiting_dict:
            raise Exception(
                "pcie read tag conflict, maybe too many outstanding requests")
        self.pending_bar_read_req_waiting_dict[tag] = evt
        evt.wait()
        del self.pending_bar_read_req_waiting_dict[tag]
        resp = self.pending_bar_read_resp.pop(tag)
        return resp.payload.value


if __name__ == "__main__":

    TOTAL_MEMORY_SIZE = 1024 * 1024 * 64
    PGT_ENTRY_OFFSET = 0x200
    PGT_ENTRY_CNT = 0x20
    PGT_ENTRY_SIZE = 0x08
    # PGT_MR0_BASE_VA = 0xFBABCDCEEEEE0001
    PGT_MR0_BASE_VA = 0x0000000000000000

    CMD_QUEUE_H2C_RINGBUF_START_PA = 0x00
    CMD_QUEUE_C2H_RINGBUF_START_PA = 0x1000
    SEND_QUEUE_RINGBUF_START_PA = 0x2000
    META_REPORT_QUEUE_RINGBUF_START_PA = 0x3000

    PGT_TABLE_START_PA_IN_HOST_MEM = 0x10000

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

    SEND_SIDE_KEY = 0x6622
    RECV_SIDE_KEY = 0x6622
    PKEY_INDEX = 0

    SEND_SIDE_QPN = 0x6611
    SEND_SIDE_PD_HANDLER = 0x6611  # in practise, this should be returned by hardware

    PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_256

    RECV_SIDE_IP = 0x11223344
    RECE_SIDE_MAC = 0xAABBCCDDEEFF
    RECV_SIDE_QPN = 0x6611
    SEND_SIDE_PSN = 0x22

    host_mem = MockHostMem("/bluesim1", 1024*1024*64)
    mock_nic = MockNicAndHost(host_mem)
    mock_nic.bluesim_rpc_server.run()

    cmd_req_queue = RingbufCommandReqQueue(
        host_mem, CMD_QUEUE_H2C_RINGBUF_START_PA, mock_host=mock_nic)
    cmd_resp_queue = RingbufCommandRespQueue(
        host_mem, CMD_QUEUE_C2H_RINGBUF_START_PA, mock_host=mock_nic)
    send_queue = RingbufSendQueue(
        host_mem, SEND_QUEUE_RINGBUF_START_PA, mock_host=mock_nic)
    meta_report_queue = RingbufMetaReportQueue(
        host_mem, META_REPORT_QUEUE_RINGBUF_START_PA, mock_host=mock_nic)

    cmd_req_queue.put_desc_update_mr_table(
        base_va=PGT_MR0_BASE_VA,
        length=MR_0_LENGTH,
        key=SEND_SIDE_KEY,
        pd_handle=SEND_SIDE_PD_HANDLER,
        pgt_offset=PGT_ENTRY_OFFSET,
        acc_flag=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
    )

    cmd_req_queue.put_desc_update_pgt(
        dma_addr=PGT_TABLE_START_PA_IN_HOST_MEM,
        dma_length=PGT_ENTRY_CNT * PGT_ENTRY_SIZE,
        start_index=PGT_ENTRY_OFFSET,
    )

    cmd_req_queue.put_desc_update_qp(
        qpn=SEND_SIDE_QPN,
        pd_handler=SEND_SIDE_PD_HANDLER,
        qp_type=TypeQP.IBV_QPT_RC,
        acc_flag=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
        pmtu=PMTU_VALUE_FOR_TEST,
    )

    # generate second level PGT entry
    PgtEntries = c_longlong * MR_0_PTE_COUNT
    entries = PgtEntries()

    for i in range(len(entries)):
        entries[i] = MR_0_PA_START + i * HUGEPAGE_2M_BYTE_CNT

    bytes_to_copy = bytes(entries)
    host_mem.buf[PGT_TABLE_START_PA_IN_HOST_MEM:PGT_TABLE_START_PA_IN_HOST_MEM +
                 len(bytes_to_copy)] = bytes_to_copy

    # ring doorbell
    cmd_req_queue.sync_pointers()

    # read cmd resp queue head pointer to check if all cmd executed
    for t in range(3):
        cmd_resp_queue.deq_blocking()

    # move send queue head to send WQE
    send_queue.put_work_request(
        opcode=WorkReqOpCode.IBV_WR_RDMA_WRITE,
        is_first=True,
        is_last=True,
        sgl=[
            SendQueueReqDescFragSGE(
                F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR),
            SendQueueReqDescFragSGE(
                F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+1),
            SendQueueReqDescFragSGE(
                F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+2),
            SendQueueReqDescFragSGE(
                F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+3),
        ],
        total_len=4,
        r_va=RESP_SIDE_VA_ADDR,
        r_key=RECV_SIDE_KEY,
        r_ip=RECV_SIDE_IP,
        r_mac=RECE_SIDE_MAC,
        dqpn=RECV_SIDE_QPN,
        psn=SEND_SIDE_PSN,
    )

    send_queue.sync_pointers()

    meta_report_queue.deq_blocking()

    print(bytes(mock_nic.main_memory.buf[0x90000:0x90004]))

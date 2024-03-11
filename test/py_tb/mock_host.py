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
import math


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
    RpcOpcodeNetIfcPutTxData = 7
    RpcOpcodeNetIfcGetRxData = 8


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


class CStructRpcNetIfcRxTxPayload(Structure):
    _fields_ = [
        ("data", c_ubyte * 64),
        ("byte_en", c_ubyte * 8),
        ("reserved", c_ubyte),
        ("is_fisrt", c_ubyte),
        ("is_last", c_ubyte),
        ("is_valid", c_ubyte),
    ]


class CStructRpcNetIfcRxTxMessage(Structure):
    _fields_ = [
        ("header", CStructRpcHeader),
        ("payload", CStructRpcNetIfcRxTxPayload),
    ]


class BluesimRpcServer:
    def __init__(self, lister_addr, listen_port) -> None:
        self.rpc_code_2_handler_map = {}
        self.rpc_code_2_payload_size_map = {}
        self.listen_addr = lister_addr
        self.listen_port = listen_port
        self.server_thread = None
        self.stop_flag = False

    def register_opcode(self, opcode, handler, payload_size):
        self.rpc_code_2_handler_map[opcode] = handler
        self.rpc_code_2_payload_size_map[opcode] = payload_size

    def run(self):
        self.stop_flag = False
        self.server_thread = threading.Thread(target=self._run)
        self.server_thread.start()

    def stop(self):
        self.stop_flag = True

    def _run(self):
        server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server_socket.bind((self.listen_addr, self.listen_port))
        server_socket.listen(1)

        print('TCP server started')

        raw_req_buf = bytearray(4096)

        while not self.stop_flag:
            client_socket, client_address = server_socket.accept()
            client_socket.settimeout(1.5)
            print('Client connected:', client_address)

            while not self.stop_flag:
                recv_pointer = 0
                recv_cnt = client_socket.recv_into(
                    raw_req_buf, sizeof(CStructRpcHeader))

                if recv_cnt != sizeof(CStructRpcHeader):
                    raise Exception("receive broken rpc header")
                rpc_header = CStructRpcHeader.from_buffer(raw_req_buf)
                remain_size = self.rpc_code_2_payload_size_map[rpc_header.opcode] - sizeof(
                    CStructRpcHeader)
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
PORT = 9874


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


class NetworkDataAgent:
    def __init__(self, mock_host):
        self.mock_host = mock_host
        self.tx_data_buf = b""
        self.full_tx_data_list = []

    def put_tx_frag(self, frag):
        ones = math.log2(frag.byte_en + 1)
        self.tx_data_buf += frag.data[:ones]
        if frag.is_last:
            self.full_tx_data_list.append(self.tx_data_buf)
            self.tx_data_buf = b""

    def put_full_rx_data(self, data):
        remain_size = len(data)
        while remain_size > 0:
            if remain_size > 64:
                data_trunk = data[:64]
                data = data[64:]
                self.mock_host.put_net_ifc_rx_data_to_nic(
                    CStructRpcNetIfcRxTxMessage(
                        header=CStructRpcHeader(
                            opcode=CEnumRpcOpcode.RpcOpcodeNetIfcGetRxData,
                        ),
                        payload=CStructRpcNetIfcRxTxPayload(
                            data=data,
                            byte_en=0xFFFFFFFFFFFFFFFF,
                            is_last=0,
                            is_valid=1
                        )
                    )
                )
            else:
                self.mock_host.put_net_ifc_rx_data_to_nic(
                    CStructRpcNetIfcRxTxMessage(
                        header=CStructRpcHeader(
                            opcode=CEnumRpcOpcode.RpcOpcodeNetIfcGetRxData,
                        ),
                        payload=CStructRpcNetIfcRxTxPayload(
                            data=data,
                            byte_en=(1 << remain_size) - 1,
                            is_last=1,
                            is_valid=1
                        )
                    )
                )

    def get_full_tx_packet(self):
        if self.full_tx_data_list:
            return self.full_tx_data_list.pop(0)
        else:
            return None


# rx_packet_wait_time can be used to mimic line-rate receive, the MockHost will block simulator and wait
# up to rx_packet_wait_time seconds. This is because tx simulator may run slower than rx simulator, so from
# rx simulator's point of view, the packet received from network interface is non-continous. if we try to
# block rx simulator, then it may see continous input packet data at every clock-cycle
class MockNicAndHost:
    def __init__(self, main_memory: MockHostMem, host=HOST, port=PORT, rx_packet_wait_time=0) -> None:
        self.main_memory = main_memory
        self.bluesim_rpc_server = BluesimRpcServer(host, port)
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarGetReadReq, self.rpc_handler_pcie_bar_get_read_req, sizeof(CStructRpcPcieBarAccessMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarPutReadResp, self.rpc_handler_pcie_bar_put_read_resp, sizeof(CStructRpcPcieBarAccessMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarGetWriteReq, self.rpc_handler_pcie_bar_get_write_req, sizeof(CStructRpcPcieBarAccessMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieBarPutWriteResp, self.rpc_handler_pcie_bar_put_write_resp, sizeof(CStructRpcPcieBarAccessMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieMemWrite, self.rpc_handler_pcie_mem_write_req, sizeof(CStructRpcPcieMemoryAccessMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodePcieMemRead, self.rpc_handler_pcie_mem_read_req, sizeof(CStructRpcPcieMemoryAccessMessage))

        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodeNetIfcGetRxData, self.rpc_handler_net_ifc_get_rx_req, sizeof(CStructRpcNetIfcRxTxMessage))
        self.bluesim_rpc_server.register_opcode(
            CEnumRpcOpcode.RpcOpcodeNetIfcPutTxData, self.rpc_handler_net_ifc_put_tx_req, sizeof(CStructRpcNetIfcRxTxMessage))

        self.pending_bar_write_req = []
        self.pending_bar_write_req_waiting_dict = {}
        self.pending_bar_write_resp = {}

        self.pending_bar_read_req = []
        self.pending_bar_read_req_waiting_dict = {}
        self.pending_bar_read_resp = {}

        self.pending_network_packet_tx = []
        self.pending_network_packet_tx_sema = threading.Semaphore(0)
        self.pending_network_packet_rx = []

        self.pcie_tlp_read_tag_counter = 0
        self.rx_packet_wait_time = rx_packet_wait_time

    def run(self):
        self.bluesim_rpc_server.run()

    def stop(self):
        self.bluesim_rpc_server.stop()

    def _get_next_pcie_tlp_tag(self):
        if self.pcie_tlp_read_tag_counter == 32:
            self.pcie_tlp_read_tag_counter = 0
        else:
            self.pcie_tlp_read_tag_counter += 1
        return self.pcie_tlp_read_tag_counter

    def rpc_handler_pcie_bar_get_read_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieBarAccessMessage.from_buffer_copy(raw_req_buf)
        if self.pending_bar_read_req:
            req_payload = self.pending_bar_read_req.pop(0)
            req.payload = req_payload
        else:
            req.payload.valid = 0
        client_socket.send(bytes(req))

    def rpc_handler_pcie_bar_put_read_resp(self, client_socket, raw_req_buf):
        resp = CStructRpcPcieBarAccessMessage.from_buffer_copy(raw_req_buf)
        if resp.payload.pci_tag in self.pending_bar_read_resp:
            raise Exception(
                "pcie read tag conflict, maybe too many outstanding requests")
        self.pending_bar_read_resp[resp.payload.pci_tag] = resp
        self.pending_bar_read_req_waiting_dict[resp.payload.pci_tag].set()
        # this Op doesn't send response

    def rpc_handler_pcie_bar_get_write_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieBarAccessMessage.from_buffer_copy(raw_req_buf)
        if self.pending_bar_write_req:
            req_payload = self.pending_bar_write_req.pop(0)
            req.payload = req_payload
        else:
            req.payload.valid = 0
        client_socket.send(bytes(req))

    def rpc_handler_pcie_bar_put_write_resp(self, client_socket, raw_req_buf):
        resp = CStructRpcPcieBarAccessMessage.from_buffer_copy(raw_req_buf)
        if resp.payload.pci_tag in self.pending_bar_write_resp:
            raise Exception(
                "pcie write tag conflict, maybe too many outstanding requests")
        self.pending_bar_write_resp[resp.payload.pci_tag] = resp
        self.pending_bar_write_req_waiting_dict[resp.payload.pci_tag].set()

        # this Op doesn't send response

    def rpc_handler_pcie_mem_write_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieMemoryAccessMessage.from_buffer_copy(raw_req_buf)
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
        req = CStructRpcPcieMemoryAccessMessage.from_buffer_copy(raw_req_buf)
        byte_cnt_per_word = req.payload.word_width >> 3
        host_mem_start_addr = req.payload.word_addr * byte_cnt_per_word
        req.payload.data[0:byte_cnt_per_word] = self.main_memory.buf[host_mem_start_addr: host_mem_start_addr+byte_cnt_per_word]
        client_socket.send(bytes(req))

    def rpc_handler_net_ifc_get_rx_req(self, client_socket, raw_req_buf):
        req = CStructRpcNetIfcRxTxMessage.from_buffer_copy(raw_req_buf)
        print("rx Q len=", len(self.pending_network_packet_rx))

        if self.rx_packet_wait_time > 0:
            start_time = time.time()
            while (not self.pending_network_packet_rx and time.time()-start_time < self.rx_packet_wait_time):
                pass

        if self.pending_network_packet_rx:
            req_payload = self.pending_network_packet_rx.pop(0)
            req.payload = req_payload
        else:
            req.payload.is_valid = 0

        client_socket.send(bytes(req))

    def rpc_handler_net_ifc_put_tx_req(self, client_socket, raw_req_buf):
        req = CStructRpcNetIfcRxTxMessage.from_buffer_copy(raw_req_buf)
        self.pending_network_packet_tx.append(req.payload)
        self.pending_network_packet_tx_sema.release()
        # this Op doesn't send response

    def write_csr_blocking(self, addr, value):
        tag = self._get_next_pcie_tlp_tag()
        self.pending_bar_write_req.append(
            CStructBarIoInfo(valid=1, addr=addr, value=value, pci_tag=tag))
        evt = threading.Event()
        if tag in self.pending_bar_write_req_waiting_dict:
            raise Exception(
                "pcie write tag conflict, maybe too many outstanding requests")
        self.pending_bar_write_req_waiting_dict[tag] = evt
        evt.wait()
        del self.pending_bar_write_req_waiting_dict[tag]
        resp = self.pending_bar_write_resp.pop(tag)
        return resp.payload.value

    # blocking version of read_csr, wait until we get response
    def read_csr_blocking(self, addr):
        tag = self._get_next_pcie_tlp_tag()
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

    def get_net_ifc_tx_data_from_nic_blocking(self):
        self.pending_network_packet_tx_sema.acquire()
        print("tx Q len=", len(self.pending_network_packet_tx))
        return self.pending_network_packet_tx.pop(0)

    def put_net_ifc_rx_data_to_nic(self, frag):
        return self.pending_network_packet_rx.append(frag)

    @staticmethod
    def do_self_loopback(nic):
        def _self_loopback_thread():
            while True:
                data = nic.get_net_ifc_tx_data_from_nic_blocking()
                nic.put_net_ifc_rx_data_to_nic(data)
        loopback_thread = threading.Thread(target=_self_loopback_thread)
        loopback_thread.start()

    @staticmethod
    def connect_two_card(nic_a, nic_b):
        def _forward_a():
            while True:
                data = nic_a.get_net_ifc_tx_data_from_nic_blocking()
                nic_b.put_net_ifc_rx_data_to_nic(data)

        def _forward_b():
            while True:
                data = nic_b.get_net_ifc_tx_data_from_nic_blocking()
                nic_a.put_net_ifc_rx_data_to_nic(data)
        forward_thread_a = threading.Thread(target=_forward_a)
        forward_thread_b = threading.Thread(target=_forward_b)
        forward_thread_a.start()
        forward_thread_b.start()


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

SEND_BYTE_COUNT = 1024*32


def test_case():
    host_mem = MockHostMem("/bluesim1", TOTAL_MEMORY_SIZE)
    mock_nic = MockNicAndHost(host_mem)
    MockNicAndHost.do_self_loopback(mock_nic)
    mock_nic.run()

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
    for _ in range(3):
        cmd_resp_queue.deq_blocking()

    # move send queue head to send WQE
    sgl = [
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR),
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+1),
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+2),
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=SEND_BYTE_COUNT-3, F_LADDR=REQ_SIDE_VA_ADDR+3),
    ]
    send_queue.put_work_request(
        opcode=WorkReqOpCode.IBV_WR_RDMA_WRITE,
        is_first=True,
        is_last=True,
        sgl=sgl,
        r_va=RESP_SIDE_VA_ADDR,
        r_key=RECV_SIDE_KEY,
        r_ip=RECV_SIDE_IP,
        r_mac=RECE_SIDE_MAC,
        dqpn=RECV_SIDE_QPN,
        psn=SEND_SIDE_PSN,
        pmtu=PMTU_VALUE_FOR_TEST,
    )

    # prepare send data
    for i in range(SEND_BYTE_COUNT):
        mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR+i] = (0xBB + i) & 0xFF
        mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR+i] = 0

    send_queue.sync_pointers()
    for _ in range(2):
        meta_report_queue.deq_blocking()

    src_mem = mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR:
                                       REQ_SIDE_VA_ADDR+SEND_BYTE_COUNT]
    dst_mem = mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR:
                                       RESP_SIDE_VA_ADDR+SEND_BYTE_COUNT]

    if src_mem != dst_mem:
        print("Error: DMA Target mem is not the same as source mem")
    else:
        print("PASS")

    mock_nic.stop()


if __name__ == "__main__":
    # must wrap test case in a function, so when the function returned, the memory view will be cleaned
    # otherwise, there will be an warning at program exit.
    test_case()

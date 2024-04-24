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
import struct
from scapy.all import Ether
from collections import deque
from abc import ABC, abstractmethod

"""
Define the types we need.
"""


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
            client_socket.settimeout(5)
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
                    raw_req_buf[recv_pointer:recv_pointer + recv_cnt] = t
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

        self.buf[:] = b"\0" * shared_mem_size

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


class MockNicInterface(ABC):
    """
    Abstract base class for a mock NIC interface.
    """

    @abstractmethod
    def run(self):
        """
        Start the mock NIC interface.
        """

        pass

    @abstractmethod
    def stop(self):
        """
        Stop the mock NIC interface.
        """

        pass

    @abstractmethod
    def get_net_ifc_tx_data_from_nic_blocking(self):
        """
        Get the transmitted data from the mock NIC interface in a blocking manner.
        """

        pass

    @abstractmethod
    def put_net_ifc_rx_data_to_nic(self, data):
        """
        Put the received data into the mock NIC interface.
        """

        pass

    @abstractmethod
    def is_contain_host(self) -> bool:
        """
        Check if the mock NIC interface contains a host.

        Returns:
            bool: True if the mock NIC interface contains a host, False otherwise.
        """

        return False


class NicManager:
    def __init__(self):
        pass

    @classmethod
    def do_self_loopback(cls, nic: MockNicInterface):
        def _self_loopback_thread():
            while True:
                data = nic.get_net_ifc_tx_data_from_nic_blocking()
                nic.put_net_ifc_rx_data_to_nic(data)
        loopback_thread = threading.Thread(target=_self_loopback_thread)
        loopback_thread.start()

    @classmethod
    def connect_two_card(cls, nic_a: MockNicInterface,
                         nic_b: MockNicInterface):
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


class EmulatorMockNicAndHost(MockNicInterface):
    """
    rx_packet_wait_time can be used to mimic line-rate receive, the MockHost will block simulator and wait
    up to rx_packet_wait_time seconds. This is because tx simulator may run slower than rx simulator, so from
    rx simulator's point of view, the packet received from network interface is non-continous. if we try to
    block rx simulator, then it may see continous input packet data at every
    clock-cycle
    """

    def __init__(self, main_memory: MockHostMem, host=HOST,
                 port=PORT, rx_packet_wait_time=0) -> None:
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
                # print("write_mem ", hex(host_mem_start_addr + idx),
                #       int(req.payload.data[idx]))
            byte_en >>= 1
        # this Op doesn't send response

    def rpc_handler_pcie_mem_read_req(self, client_socket, raw_req_buf):
        req = CStructRpcPcieMemoryAccessMessage.from_buffer_copy(raw_req_buf)
        byte_cnt_per_word = req.payload.word_width >> 3
        host_mem_start_addr = req.payload.word_addr * byte_cnt_per_word
        req.payload.data[0:byte_cnt_per_word] = self.main_memory.buf[host_mem_start_addr:
                                                                     host_mem_start_addr + byte_cnt_per_word]
        client_socket.send(bytes(req))

    def rpc_handler_net_ifc_get_rx_req(self, client_socket, raw_req_buf):
        req = CStructRpcNetIfcRxTxMessage.from_buffer_copy(raw_req_buf)
        # print("rx Q len=", len(self.pending_network_packet_rx))

        if self.rx_packet_wait_time > 0:
            start_time = time.time()
            while (not self.pending_network_packet_rx and time.time() -
                   start_time < self.rx_packet_wait_time):
                pass

        if self.pending_network_packet_rx:
            req_payload = self.pending_network_packet_rx.pop(0)
            req.payload = req_payload
        else:
            req.payload.is_valid = 0

        client_socket.send(bytes(req))

    def rpc_handler_net_ifc_put_tx_req(self, client_socket, raw_req_buf):
        req = CStructRpcNetIfcRxTxMessage.from_buffer_copy(raw_req_buf)
        self.pending_network_packet_tx.append(req)
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
        data = self.pending_network_packet_tx.pop(0)
        return data

    def put_net_ifc_rx_data_to_nic(self, frag):
        return self.pending_network_packet_rx.append(frag.payload)


# A mock nic that can send and receive packets
class RawsocketMockNicAndHost(MockNicInterface):
    """
    A class representing a mock NIC and host using raw sockets.

    Args:
        addr (str): The IP address to bind the socket to.
        port (int): The port number to bind the socket to.
        local_mac (str): The MAC address of the local host.
        remote_mac (str): The MAC address of the remote host.
    """

    def __init__(self, addr: str, port: int, local_mac: str, remote_mac: str):
        self.pending_network_packet_tx = deque()
        self.pending_network_packet_tx_sema = threading.Semaphore(0)
        self.pending_network_packet_rx = deque()
        self.pending_network_packet_rx_sema = threading.Semaphore(0)
        self.stop_flag = False
        self._receive_proxy_thread = threading.Thread(
            target=self._receive_proxy, args=(
                addr, port, local_mac, remote_mac))
        self._send_proxy_thread = threading.Thread(
            target=self._send_proxy, args=())

    def __del__(self):
        self.stop_flag = True

    def stop(self):
        self.pending_network_packet_tx_sema.release()
        self.stop_flag = True

    def get_net_ifc_tx_data_from_nic_blocking(self):
        self.pending_network_packet_rx_sema.acquire()
        return self.pending_network_packet_rx.popleft()

    def put_net_ifc_rx_data_to_nic(self, data):
        self.pending_network_packet_tx.append(data)
        self.pending_network_packet_tx_sema.release()

    def run(self):
        self._receive_proxy_thread.start()
        self._send_proxy_thread.start()

    def _receive_proxy(self, addr, port, local_mac, remote_mac):
        try:
            s = socket.socket(
                socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_UDP)
            s.setsockopt(socket.IPPROTO_IP, socket.IP_HDRINCL, 1)
        except socket.error as msg:
            import sys
            sys.stderr.write('Socket could not be created. Error Code : ' +
                             str(msg) + ' Message ')
        s.bind((addr, port))
        while not self.stop_flag:
            packet, (_ip, _port) = s.recvfrom(5120)
            eth_frame = Ether(src=remote_mac, dst=local_mac, type=0x0800)
            packet = bytes(eth_frame) + packet
            for frag in self.split_packet_into_fragments(packet):
                self.pending_network_packet_rx.append(frag)
                self.pending_network_packet_rx_sema.release()
        s.close()

    def _send_proxy(self):
        try:
            s = socket.socket(
                socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_UDP)
            s.setsockopt(socket.IPPROTO_IP, socket.IP_HDRINCL, 1)
        except socket.error as msg:
            import sys
            sys.stderr.write('Socket could not be created. Error Code : ' +
                             str(msg) + ' Message ')
        data_buf = dict()
        while not self.stop_flag:
            self.pending_network_packet_tx_sema.acquire()
            if self.stop_flag:
                break
            data = self.pending_network_packet_tx.popleft()
            bitmask = struct.unpack('<Q', bytes(data.payload.byte_en))[0]
            payload = self.get_data_with_bitmap(
                bytes(data.payload.data), bitmask)
            if data.header.tag not in data_buf:
                data_buf[data.header.tag] = payload
            else:
                data_buf[data.header.tag] += payload
            if data.payload.is_last:
                packet = data_buf[data.header.tag]
                dst_ip = socket.inet_ntoa(packet[30:34])
                s.sendto(data_buf[data.header.tag][14:], (dst_ip, 0))
                del data_buf[data.header.tag]
        s.close()

    @classmethod
    def get_data_with_bitmap(cls,data: bytearray, bitmask: int):
        count = bin(bitmask).count('1')
        return data[:count]

    CHUNK_SIZE = 64
    CUbyteArray64 = c_ubyte * 64
    CUbyteArray8 = c_ubyte * 8

    @classmethod
    def split_packet_into_fragments(cls, packet: bytearray):
        for i in range(0, len(packet), cls.CHUNK_SIZE):
            chunk = packet[i:i + cls.CHUNK_SIZE]
            if len(chunk) == cls.CHUNK_SIZE:
                bitmap = cls.CUbyteArray8(
                    *struct.pack("<Q", 0xFFFFFFFFFFFFFFFF))
                is_last = False
            else:
                bitmap = cls.CUbyteArray8(
                    *struct.pack("<Q", (1 << len(chunk)) - 1))
                is_last = True
            array = cls.CUbyteArray64(*chunk)
            yield CStructRpcNetIfcRxTxMessage(
                header=CStructRpcHeader(
                    opcode=CEnumRpcOpcode.RpcOpcodeNetIfcGetRxData,
                ),
                payload=CStructRpcNetIfcRxTxPayload(
                    data=array,
                    byte_en=bitmap,
                    is_last=is_last,
                    is_valid=True
                )
            )

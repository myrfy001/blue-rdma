# coding:utf-8
import socket
from ctypes import *
from enum import IntEnum


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


class CStructRpcHeader(Structure):
    _fields_ = [
        ("opcode", c_int),
        ("client_id", c_longlong),
    ]


class CStructBarIoInfo(Structure):
    _fields_ = [
        ("value", c_longlong),
        ("addr", c_longlong),
        ("valid", c_longlong),
    ]


class CStructBarAccessMessage(Structure):
    _fields_ = [
        ("header", CStructRpcHeader),
        ("payload", CStructBarIoInfo),
    ]


RPC_OPCODE_2_SIZE_MAP = {
    CEnumRpcOpcode.RpcOpcodePcieBarGetReadReq: sizeof(CStructBarIoInfo),
    CEnumRpcOpcode.RpcOpcodePcieBarPutReadResp: sizeof(CStructBarIoInfo),
    CEnumRpcOpcode.RpcOpcodePcieBarGetWriteReq: sizeof(CStructBarIoInfo),
    CEnumRpcOpcode.RpcOpcodePcieBarPutWriteResp: sizeof(CStructBarIoInfo),
}


def rpc_handler_pcie_bar_get_read_req(client_socket, raw_req_buf):
    req = CStructBarAccessMessage.from_buffer(raw_req_buf)
    req.payload.valid = 0
    client_socket.send(bytes(req))


def rpc_handler_pcie_bar_put_read_resp(client_socket, raw_req_buf):
    req = CStructBarAccessMessage.from_buffer(raw_req_buf)
    req.payload.valid = 0
    # this Op doesn't send response


def rpc_handler_pcie_bar_get_write_req(client_socket, raw_req_buf):
    req = CStructBarAccessMessage.from_buffer(raw_req_buf)
    req.payload.valid = 0
    client_socket.send(bytes(req))


def rpc_handler_pcie_bar_put_write_resp(client_socket, raw_req_buf):
    req = CStructBarAccessMessage.from_buffer(raw_req_buf)
    req.payload.valid = 0
    # this Op doesn't send response


RPC_OPCODE_2_HANDLER_MAP = {
    CEnumRpcOpcode.RpcOpcodePcieBarGetReadReq: rpc_handler_pcie_bar_get_read_req,
    CEnumRpcOpcode.RpcOpcodePcieBarPutReadResp: rpc_handler_pcie_bar_put_read_resp,
    CEnumRpcOpcode.RpcOpcodePcieBarGetWriteReq: rpc_handler_pcie_bar_get_write_req,
    CEnumRpcOpcode.RpcOpcodePcieBarPutWriteResp: rpc_handler_pcie_bar_put_write_resp,
}

# 定义服务器地址和端口号
HOST = '127.0.0.1'
PORT = 9876


server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
server_socket.bind((HOST, PORT))
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
        remain_size = RPC_OPCODE_2_SIZE_MAP[rpc_header.opcode]
        recv_pointer = recv_cnt

        while remain_size > 0:
            recv_cnt = client_socket.recv_into(
                raw_req_buf[recv_pointer:], remain_size)
            if recv_cnt == 0:
                raise Exception("bluesim exited, connection broken.")
            remain_size -= remain_size
            recv_pointer += remain_size

        RPC_OPCODE_2_HANDLER_MAP[rpc_header.opcode](client_socket, raw_req_buf)

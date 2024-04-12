from mock_host import RawsocketMockNicAndHost, CStructRpcNetIfcRxTxMessage, CStructRpcHeader, CStructRpcNetIfcRxTxPayload
import socket
from ctypes import c_ubyte
import struct
from scapy.all import IP, UDP, Raw, Ether
nica = RawsocketMockNicAndHost(
    "127.0.0.2",
    4791,
    "aa:bb:cc:dd:ee:ff",
    "bb:cc:dd:ee:ff:aa")
nica.run()


def raw_socket_sending(data: bytes):
    try:
        s = socket.socket(
            socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_UDP)
        s.setsockopt(socket.IPPROTO_IP, socket.IP_HDRINCL, 1)
    except socket.error as msg:
        print('Socket could not be created. Error Code : ' +
              str(msg) + ' Message ')
        import sys
        sys.exit()
    ip = IP(version=4, id=1, src="127.0.0.3", dst="127.0.0.2")
    udp = UDP(sport=4791, dport=4791)
    data = Raw(data)
    packet = ip / udp / data
    s.sendto(bytes(packet), ("127.0.0.2", 4791))


def raw_socket_receive_one():
    try:
        s = socket.socket(
            socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_UDP)
        s.setsockopt(socket.IPPROTO_IP, socket.IP_HDRINCL, 1)
    except socket.error as msg:
        print('Socket could not be created. Error Code : ' +
              str(msg) + ' Message ')
        import sys
        sys.exit()
    s.bind(("127.0.0.3", 4791))
    p = s.recvfrom(1024)
    return p[0]


TEST_PAYLOAD = b"Hello World"
assert len(TEST_PAYLOAD) < 22

raw_socket_sending(TEST_PAYLOAD)
data = bytes(nica.get_net_ifc_tx_data_from_nic_blocking().payload.data)
assert data[14 + 20 + 8:14 + 20 + 8 + len(TEST_PAYLOAD)], TEST_PAYLOAD
mac = Ether(src="bb:cc:dd:ee:ff:aa", dst="aa:bb:cc:dd:ee:ff", type=0x0800)
ip = IP(version=4, id=1, src="127.0.0.3", dst="127.0.0.3")
udp = UDP(sport=4791, dport=4791)
data = Raw(TEST_PAYLOAD)
packet = mac / ip / udp / data

msg = CStructRpcNetIfcRxTxMessage(
    header=CStructRpcHeader(),
    payload=CStructRpcNetIfcRxTxPayload(
        data=(c_ubyte * 64)(*bytes(packet)),
        byte_en=(c_ubyte * 8)(*struct.pack("<Q", (1 << len(packet)) - 1)),
        is_last=1,
        is_valid=1
    )
)
nica.put_net_ifc_rx_data_to_nic(msg)
payload = raw_socket_receive_one()
assert payload[20 + 8:] == TEST_PAYLOAD, payload[14 + 20 + 8:]
nica.stop()
print("test pass")

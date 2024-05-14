from scapy.compat import raw, Tuple
from scapy.error import warning
from scapy.fields import (
    BitEnumField,
    ByteEnumField,
    ByteField,
    XByteField,
    ShortField,
    XShortField,
    XIntField,
    XLongField,
    BitField,
    XBitField,
    FCSField,
)
from scapy.layers.inet import IP, UDP
from scapy.layers.l2 import Ether
from scapy.packet import Packet, bind_layers, Raw
from zlib import crc32

from mock_host import *
from test_case_common import *

PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_4096

RECV_SIDE_IP = NIC_CONFIG_IPADDR
RECE_SIDE_MAC = NIC_CONFIG_MACADDR
RECV_SIDE_QPN = 0x6611
SEND_SIDE_PSN = 0x22

SEND_BYTE_COUNT = 1024*16


def test_case(host_mem):
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

    cmd_req_queue.put_desc_set_udp_param(
        NIC_CONFIG_GATEWAY, NIC_CONFIG_NETMASK, NIC_CONFIG_IPADDR, NIC_CONFIG_MACADDR)

    cmd_req_queue.put_desc_set_raw_packet_receive_meta(
        RESP_SIDE_VA_ADDR, RECV_SIDE_KEY)

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
        peer_qpn=RECV_SIDE_QPN,
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

    for test_idx in range(32):
        # generate raw packet data
        eth_layer = Ether(dst="AA:BB:CC:DD:EE", src="AA:BB:CC:DD:FF")
        ip_layer = IP(dst="17.34.51.68")
        udp_layer = UDP(dport=1111, sport=2222)

        payload_to_send = "0123456789abcdef" * test_idx
        bytes_to_send = bytes(eth_layer/ip_layer/udp_layer/payload_to_send)

        # #########################################

        eth_header = Ether(bytes_to_send)
        print(eth_header)

        # Handle IP header
        ip_header = IP(bytes(eth_header.payload))
        print(ip_header)
        print("IP Header:")
        print("Source IP:", ip_header.src)
        print("Destination IP:", ip_header.dst)
        print("Protocol:", ip_header.proto)

        # Handle UDP header
        udp_header = UDP(bytes(ip_header.payload))
        print("UDP Header:")
        print("Source Port:", udp_header.sport)
        print("Destination Port:", udp_header.dport)
        print("Length:", udp_header.len)
        # #####################################################

        print("bytes_to_send=", bytes_to_send)
        host_mem.buf[REQ_SIDE_VA_ADDR:REQ_SIDE_VA_ADDR +
                     len(bytes_to_send)] = bytes_to_send
        print("src_mem=", bytes(host_mem.buf[REQ_SIDE_VA_ADDR:REQ_SIDE_VA_ADDR +
                                             len(bytes_to_send)]))

        # move send queue head to send WQE
        sgl = [
            SendQueueReqDescFragSGE(
                F_LKEY=SEND_SIDE_KEY, F_LEN=len(bytes_to_send), F_LADDR=REQ_SIDE_VA_ADDR),
        ]
        send_queue.put_work_request(
            opcode=WorkReqOpCode.IBV_WR_RDMA_WRITE_WITH_IMM,
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
            qp_type=TypeQP.IBV_QPT_RAW_PACKET,
        )

        send_queue.sync_pointers()

        report_meta = MeatReportQueueDescBthReth.from_buffer_copy(
            meta_report_queue.deq_blocking())
        print("receive meta report: ", report_meta)

        # discard unused Imm descriptor
        meta_report_queue.deq_blocking()

        expected_receive_len = len(bytes_to_send)
        received_len = report_meta.F_RETH.F_DLEN
        if received_len != expected_receive_len:
            print(
                f"Error: Raw Packet Meta Length Not Match, recv={received_len}, expected={expected_receive_len}")
            raise SystemError

        dst_mem = mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR + 4096 * test_idx:
                                           RESP_SIDE_VA_ADDR + 4096 * test_idx + received_len]

        print("dst_mem=", bytes(dst_mem))

        eth_header = Ether(dst_mem)
        print(eth_header)

        # Handle IP header
        ip_header = IP(bytes(eth_header.payload))
        print(ip_header)
        print("IP Header:")
        print("Source IP:", ip_header.src)
        print("Destination IP:", ip_header.dst)
        print("Protocol:", ip_header.proto)

        # Handle UDP header
        udp_header = UDP(bytes(ip_header.payload))
        print("UDP Header:")
        print("Source Port:", udp_header.sport)
        print("Destination Port:", udp_header.dport)
        print("Length:", udp_header.len)

        udp_payload = bytes(udp_header.payload)
        print("UDP Payload:", udp_payload)

        if udp_payload != payload_to_send.encode("utf-8"):
            print(
                f"Error: recv payload not match, send={payload_to_send}, recv={udp_payload}")
            raise SystemExit

        else:
            print("PASS-", test_idx)


if __name__ == "__main__":
    # must wrap test case in a function, so when the function returned, the memory view will be cleaned
    # otherwise, there will be an warning at program exit.
    run_test_case(test_case)

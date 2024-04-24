from mock_host import *
from test_case_common import *
from utils import print_mem_diff, assert_descriptor_bth_reth

PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_256

RECV_SIDE_IP = NIC_CONFIG_IPADDR
RECE_SIDE_MAC = NIC_CONFIG_MACADDR
RECV_SIDE_QPN = 0x6611
SEND_SIDE_PSN_INIT_VAL = 0x0

PMTU_VAL = 256 * (2 ** (PMTU_VALUE_FOR_TEST-1))
PACKET_CNT = 32
SEND_BYTE_COUNT = PMTU_VAL * PACKET_CNT


def test_case():
    send_psn = SEND_SIDE_PSN_INIT_VAL
    host_mem = MockHostMem("/bluesim1", TOTAL_MEMORY_SIZE)
    mock_nic = MockNicAndHost(host_mem)
    pkt_agent = NetworkDataAgent(mock_nic)
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

    # prepare send data
    for i in range(SEND_BYTE_COUNT):
        mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR+i] = (0xBB + i) & 0xFF
        mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR+i] = 0

    src_mem = mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR:
                                       REQ_SIDE_VA_ADDR+SEND_BYTE_COUNT]
    dst_mem = mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR:
                                       RESP_SIDE_VA_ADDR+SEND_BYTE_COUNT]
    # ================================
    # first case, write single byte
    # ================================
    sgl = [
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=SEND_BYTE_COUNT, F_LADDR=REQ_SIDE_VA_ADDR),
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
        psn=send_psn,
        pmtu=PMTU_VALUE_FOR_TEST,
        send_flag=WorkReqSendFlag.IBV_SEND_SIGNALED,
    )
    send_psn += 1

    send_queue.sync_pointers()

    received_packets = []
    while True:
        frag = mock_nic.get_net_ifc_tx_data_from_nic_blocking()
        pkt_agent.put_tx_frag(frag)
        pkt = pkt_agent.get_full_tx_packet()
        print("received_packets frag")
        if pkt is not None:
            received_packets.append(pkt)
        if len(received_packets) == PACKET_CNT:
            break
        print("received_packets len = ", len(received_packets))

    # drop some packet
    received_packets[3] = None
    received_packets[6] = None
    received_packets[7] = None
    received_packets[20] = None
    received_packets[22] = None

    for packet in received_packets:
        if packet is not None:
            pkt_agent.put_full_rx_data(packet)

    print("all packet sent")

    expected_metas = []
    # first 3 packet are normal, so only receive first packet
    expected_metas.append({"psn": 0, "expected_psn": 0,
                          "opcode": RdmaOpCode.RDMA_WRITE_FIRST,
                           "can_auto_ack": 1})

    # packte 3 lost, so packet 4 trigger report
    expected_metas.append({"psn": 4, "expected_psn": 3,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0})

    # since packet 4 and 5 are continous, packet 5 should not report

    # packet 6,7 is lost, so packet 8 trigger report
    expected_metas.append({"psn": 8, "expected_psn": 6,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0})

    # packet 9~19 is continous, should not report
    # packte 20 lost, so packet 21 trigger report
    expected_metas.append({"psn": 21, "expected_psn": 20,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0})
    # packte 22 lost, so packet 23 trigger report
    expected_metas.append({"psn": 23, "expected_psn": 22,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0})

    # packte 31 is last, need report
    expected_metas.append({"psn": 31, "expected_psn": 31,
                          "opcode": RdmaOpCode.RDMA_WRITE_LAST,
                           "can_auto_ack": 0})

    for expected_meta in expected_metas:
        report = meta_report_queue.deq_blocking()
        desc = MeatReportQueueDescBthReth.from_buffer(report)
        assert_descriptor_bth_reth(desc, **expected_meta)

    mock_nic.stop()


if __name__ == "__main__":
    # must wrap test case in a function, so when the function returned, the memory view will be cleaned
    # otherwise, there will be an warning at program exit.
    test_case()

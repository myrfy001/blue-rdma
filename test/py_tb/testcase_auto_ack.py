from mock_host import *
from test_case_common import *
from utils import print_mem_diff, assert_descriptor_bth_reth, assert_descriptor_bth_aeth

PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_256

RECV_SIDE_IP = NIC_CONFIG_IPADDR
RECE_SIDE_MAC = NIC_CONFIG_MACADDR
RECV_SIDE_QPN = SEND_SIDE_QPN + 1
SEND_SIDE_PSN_INIT_VAL = 0x0
SEND_SIDE_MSN_INIT_VAL = 0x2

PMTU_VAL = 256 * (2 ** (PMTU_VALUE_FOR_TEST-1))
PACKET_CNT = 32
SEND_BYTE_COUNT = PMTU_VAL * PACKET_CNT


def test_case(host_mem):
    send_psn = SEND_SIDE_PSN_INIT_VAL
    send_msn = SEND_SIDE_MSN_INIT_VAL
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

    # we need a couple of QP
    cmd_req_queue.put_desc_update_qp(
        qpn=SEND_SIDE_QPN,
        peer_qpn=RECV_SIDE_QPN,
        pd_handler=SEND_SIDE_PD_HANDLER,
        qp_type=TypeQP.IBV_QPT_RC,
        acc_flag=MemAccessTypeFlag.IBV_ACCESS_LOCAL_WRITE | MemAccessTypeFlag.IBV_ACCESS_REMOTE_READ | MemAccessTypeFlag.IBV_ACCESS_REMOTE_WRITE,
        pmtu=PMTU_VALUE_FOR_TEST,
    )

    cmd_req_queue.put_desc_update_qp(
        qpn=RECV_SIDE_QPN,
        peer_qpn=SEND_SIDE_QPN,
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
    for _ in range(4):
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
    # first case, have some packet lost, check psn and don't send auto ack
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
        msn=send_msn,
    )
    send_psn += PACKET_CNT
    send_msn += 1

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

    # swap some packet
    received_packets[12], received_packets[15] = received_packets[15], received_packets[12]

    for packet in received_packets:
        if packet is not None:
            pkt_agent.put_full_rx_data(packet)

    expected_metas = []
    # first 3 packet are normal, so only receive first packet
    expected_metas.append({"psn": 0, "expected_psn": 0,
                          "opcode": RdmaOpCode.RDMA_WRITE_FIRST,
                           "can_auto_ack": 1, "msn": send_msn-1})

    # packte 3 lost, so packet 4 trigger report
    expected_metas.append({"psn": 4, "expected_psn": 3,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # since packet 4 and 5 are continous, packet 5 should not report

    # packet 6,7 is lost, so packet 8 trigger report
    expected_metas.append({"psn": 8, "expected_psn": 6,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # packet 9~11 is continous, should not report

    # packet 12 and 15 are out of order, receive 15 first, so report
    expected_metas.append({"psn": 15, "expected_psn": 12,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # packet 13 14 is continous, but since expected psn is 16 now and  can't go back, so 13 and 14 won't match expected_psn,
    # so they will both be reported
    expected_metas.append({"psn": 13, "expected_psn": 16,
                           "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})
    expected_metas.append({"psn": 14, "expected_psn": 16,
                           "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # out of order packet 12 arrive now, of course need report
    expected_metas.append({"psn": 12, "expected_psn": 16,
                           "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # packet 16~19 is continous, should not report

    # packte 20 lost, so packet 21 trigger report
    expected_metas.append({"psn": 21, "expected_psn": 20,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # packte 22 lost, so packet 23 trigger report
    expected_metas.append({"psn": 23, "expected_psn": 22,
                          "opcode": RdmaOpCode.RDMA_WRITE_MIDDLE,
                           "can_auto_ack": 0, "msn": send_msn-1})

    # packet 24~30 should not be reported

    # packte 31 is last, need report
    expected_metas.append({"psn": 31, "expected_psn": 31,
                          "opcode": RdmaOpCode.RDMA_WRITE_LAST,
                           "can_auto_ack": 0, "msn": send_msn-1})

    for expected_meta in expected_metas:
        report = meta_report_queue.deq_blocking()
        desc = MeatReportQueueDescBthReth.from_buffer(report)
        assert_descriptor_bth_reth(desc, **expected_meta)

    print("Pass-1")

    # ================================
    # second case, send qp recovery point update cmd
    # ================================

    # for the previous test, the QP Contex should hold recovery_point psn = 23
    cmd_req_queue.put_desc_update_err_psn_recover_point(RECV_SIDE_QPN, 22)
    cmd_req_queue.sync_pointers()

    # send a simple req, and should see that QP is still in non-auto-ack mode
    sgl = [
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR),
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
        msn=send_msn,
    )
    send_psn += 1
    send_msn += 1

    send_queue.sync_pointers()

    received_packets = []
    while True:
        frag = mock_nic.get_net_ifc_tx_data_from_nic_blocking()
        pkt_agent.put_tx_frag(frag)
        pkt = pkt_agent.get_full_tx_packet()
        print("received_packets frag")
        if pkt is not None:
            received_packets.append(pkt)
        if len(received_packets) == 1:
            break
        print("received_packets len = ", len(received_packets))

    for packet in received_packets:
        if packet is not None:
            pkt_agent.put_full_rx_data(packet)

    print("all packet sent")

    expected_metas = []
    # psn should be continous with previous request, so it should be 32
    expected_metas.append({"psn": 32, "expected_psn": 32,
                          "opcode": RdmaOpCode.RDMA_WRITE_ONLY,
                           "can_auto_ack": 0, "msn": send_msn-1})

    for expected_meta in expected_metas:
        report = meta_report_queue.deq_blocking()
        desc = MeatReportQueueDescBthReth.from_buffer(report)
        assert_descriptor_bth_reth(desc, **expected_meta)

    print("Pass-2")

    # then, we send a right psn recover point
    cmd_req_queue.put_desc_update_err_psn_recover_point(RECV_SIDE_QPN, 23)
    cmd_req_queue.sync_pointers()

    # send a simple req, and should see that QP is will go back to auto-ack mode
    sgl = [
        SendQueueReqDescFragSGE(
            F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR),
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
        msn=send_msn,
    )
    send_psn += 1
    send_msn += 1

    send_queue.sync_pointers()

    received_packets = []
    while True:
        frag = mock_nic.get_net_ifc_tx_data_from_nic_blocking()
        pkt_agent.put_tx_frag(frag)
        pkt = pkt_agent.get_full_tx_packet()
        print("received_packets frag")
        if pkt is not None:
            received_packets.append(pkt)
        if len(received_packets) == 1:
            break
        print("received_packets len = ", len(received_packets))

    for packet in received_packets:
        if packet is not None:
            pkt_agent.put_full_rx_data(packet)

    print("all packet sent")

    expected_metas = []
    # psn should be continous with previous request, so it should be 33, and can_auto_ack should be 1
    expected_metas.append({"psn": 33, "expected_psn": 33,
                          "opcode": RdmaOpCode.RDMA_WRITE_ONLY,
                           "can_auto_ack": 1, "msn": send_msn-1})

    for expected_meta in expected_metas:
        report = meta_report_queue.deq_blocking()
        desc = MeatReportQueueDescBthReth.from_buffer(report)
        assert_descriptor_bth_reth(desc, **expected_meta)

    print("Pass-3")

    received_packets = []
    while True:
        frag = mock_nic.get_net_ifc_tx_data_from_nic_blocking()
        pkt_agent.put_tx_frag(frag)
        pkt = pkt_agent.get_full_tx_packet()
        print("received_packets frag")
        if pkt is not None:
            received_packets.append(pkt)
        if len(received_packets) == 1:
            break
        print("received_packets len = ", len(received_packets))

    for packet in received_packets:
        if packet is not None:
            pkt_agent.put_full_rx_data(packet)

    expected_metas = []
    # since can_auto_ack is recovered now, an ack should be generated.
    # since its an ACK, last_retry_psn should be 0
    expected_metas.append({"psn": 33, "opcode": RdmaOpCode.ACKNOWLEDGE,
                           "dqpn": SEND_SIDE_QPN,
                           "msn": send_msn-1, "last_retry_psn": 0,
                           "aeth_code": AethCode.AETH_CODE_ACK,
                           "aeth_value": AethAckValueCreditCnt.AETH_ACK_VALUE_INVALID_CREDIT_CNT})

    for expected_meta in expected_metas:
        report = meta_report_queue.deq_blocking()
        desc = MeatReportQueueDescBthAeth.from_buffer(report)
        assert_descriptor_bth_aeth(desc, **expected_meta)

    print("Pass-4")


if __name__ == "__main__":
    # must wrap test case in a function, so when the function returned, the memory view will be cleaned
    # otherwise, there will be an warning at program exit.
    run_test_case(test_case)

from mock_host import *
from test_case_common import *


PMTU_VALUE_FOR_TEST = PMTU.IBV_MTU_4096

RECV_SIDE_IP = NIC_CONFIG_IPADDR
RECE_SIDE_MAC = NIC_CONFIG_MACADDR
RECV_SIDE_QPN = 0x6611
SEND_SIDE_PSN = 0x22

SEND_BYTE_COUNT = 1024*16


def test_case():
    host_mem = MockHostMem("/bluesim1", TOTAL_MEMORY_SIZE)
    mock_nic = MockNicAndHost(host_mem, tx_packet_accumulate_cnt=230)
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

    # move send queue head to send WQE
    # sgl = [
    #     SendQueueReqDescFragSGE(
    #         F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR),
    #     SendQueueReqDescFragSGE(
    #         F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+1),
    #     SendQueueReqDescFragSGE(
    #         F_LKEY=SEND_SIDE_KEY, F_LEN=1, F_LADDR=REQ_SIDE_VA_ADDR+2),
    #     SendQueueReqDescFragSGE(
    #         F_LKEY=SEND_SIDE_KEY, F_LEN=SEND_BYTE_COUNT-3, F_LADDR=REQ_SIDE_VA_ADDR+3),
    # ]
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
        psn=SEND_SIDE_PSN,
        pmtu=PMTU_VALUE_FOR_TEST,
    )

    # prepare send data
    for i in range(SEND_BYTE_COUNT):
        mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR+i] = (0xBB + i) & 0xFF
        mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR+i] = 0

    send_queue.sync_pointers()
    for report_idx in range(2):
        meta_report_queue.deq_blocking()
        print("receive meta report: ", report_idx)

    src_mem = mock_nic.main_memory.buf[REQ_SIDE_VA_ADDR:
                                       REQ_SIDE_VA_ADDR+SEND_BYTE_COUNT]
    dst_mem = mock_nic.main_memory.buf[RESP_SIDE_VA_ADDR:
                                       RESP_SIDE_VA_ADDR+SEND_BYTE_COUNT]

    if src_mem != dst_mem:
        print("Error: DMA Target mem is not the same as source mem")
        for idx in range(len(src_mem)):
            if src_mem[idx] != dst_mem[idx]:
                print("id:", idx,
                      "src: ", hex(src_mem[idx]),
                      "dst: ", hex(dst_mem[idx])
                      )
    else:
        print("PASS")

    mock_nic.stop()


if __name__ == "__main__":
    # must wrap test case in a function, so when the function returned, the memory view will be cleaned
    # otherwise, there will be an warning at program exit.
    test_case()

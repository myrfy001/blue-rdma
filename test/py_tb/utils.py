from desccriptors import *


def print_mem_diff(real, expected):
    for idx in range(len(real)):
        if real[idx] != expected[idx]:
            print("id:", idx,
                  "expected: ", hex(expected[idx]),
                  "real: ", hex(real[idx])
                  )


def check_single_descriptor_field(name, expected, got):
    if expected is not None:
        if got != expected:
            print(
                f"Error: desc {name} should be {expected}, but got {got}")
            return False
    return True


def assert_descriptor_bth_reth(desc, dqpn=None, psn=None, expected_psn=None, msn=None, opcode=None, trans=None, req_status=RdmaReqStatus.RDMA_REQ_ST_NORMAL, can_auto_ack=None):
    if not isinstance(desc, MeatReportQueueDescBthReth):
        print("Error: desc should be MeatReportQueueDescBthReth, got ", type(desc))
        raise SystemExit

    if not check_single_descriptor_field("dqpn", dqpn, desc.F_BTH.F_DQPN):
        raise SystemExit

    if not check_single_descriptor_field("expected_psn", expected_psn, desc.F_EXPECTED_PSN):
        raise SystemExit

    if not check_single_descriptor_field("psn", psn, desc.F_BTH.F_PSN):
        raise SystemExit

    if not check_single_descriptor_field("msn(pkey)", msn, desc.F_MSN):
        raise SystemExit

    if not check_single_descriptor_field("opcode", opcode, desc.F_BTH.F_OPCODE):
        raise SystemExit

    if not check_single_descriptor_field("trans", trans, desc.F_BTH.F_TRANS):
        raise SystemExit

    if not check_single_descriptor_field("req_status", req_status, desc.F_REQ_STATUS):
        raise SystemExit

    if not check_single_descriptor_field("can_auto_ack", can_auto_ack, desc.F_CAN_AUTO_ACK):
        raise SystemExit


def assert_descriptor_bth_aeth(desc, dqpn=None, psn=None, msn=None, opcode=None, trans=None, req_status=RdmaReqStatus.RDMA_REQ_ST_NORMAL, last_retry_psn=None, aeth_code=None, aeth_value=None):
    if not isinstance(desc, MeatReportQueueDescBthAeth):
        print("Error: desc should be MeatReportQueueDescBthAeth, got ", type(desc))
        raise SystemExit

    if not check_single_descriptor_field("dqpn", dqpn, desc.F_BTH.F_DQPN):
        raise SystemExit

    if not check_single_descriptor_field("psn", psn, desc.F_BTH.F_PSN):
        raise SystemExit

    if not check_single_descriptor_field("msn(pkey)", msn, desc.F_AETH.F_MSN):
        raise SystemExit

    if not check_single_descriptor_field("opcode", opcode, desc.F_BTH.F_OPCODE):
        raise SystemExit

    if not check_single_descriptor_field("trans", trans, desc.F_BTH.F_TRANS):
        raise SystemExit

    if not check_single_descriptor_field("req_status", req_status, desc.F_REQ_STATUS):
        raise SystemExit

    if not check_single_descriptor_field("last_retry_psn", last_retry_psn, desc.F_AETH.F_LAST_RETRY_PSN):
        raise SystemExit

    if not check_single_descriptor_field("aeth_code", aeth_code, desc.F_AETH.F_AETH_CODE):
        raise SystemExit

    if not check_single_descriptor_field("aeth_value", aeth_value, desc.F_AETH.F_AETH_VALUE):
        raise SystemExit


def assert_descriptor_reth(buffer, opcode):
    desc = MeatReportQueueDescBthReth.from_buffer(buffer)
    assert_descriptor_bth_reth(desc, opcode=opcode)


def assert_descriptor_ack(buffer):
    desc = MeatReportQueueDescBthAeth.from_buffer(buffer)
    assert_descriptor_bth_aeth(desc, aeth_code=AethCode.AETH_CODE_ACK)

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


def assert_descriptor_bth_reth(desc, psn=None, expected_psn=None, msn=None, opcode=None, trans=None, can_auto_ack=None):
    if not isinstance(desc, MeatReportQueueDescBthReth):
        print("Error: desc should be MeatReportQueueDescBthReth, got ", type(desc))
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

    if not check_single_descriptor_field("can_auto_ack", can_auto_ack, desc.F_CAN_AUTO_ACK):
        raise SystemExit


def meta_report_descriptor_from_buffer(buffer):
    pass

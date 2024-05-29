# coding: utf-8

import os
import glob
import subprocess
import time
import sys

for testcase_fn in glob.glob("testcase_*.py"):
    # if "testcase_single_card_loopback_multi_case.py" not in testcase_fn:
    #     continue
    print("=-=-=-=-=-=--=-=-=-=-Begin Test-=-=-=-=-=-=-=-=")
    print(testcase_fn)
    print("=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")
    sys.stdout.flush()

    proc_tb = subprocess.Popen(["python3", testcase_fn], bufsize=0)
    time.sleep(1)
    proc_simulator = subprocess.Popen(
        ["../build/mkTestTop.sh"], cwd="../build")

    ret_code = 0
    try:
        ret_code = proc_tb.wait(60)
    except:
        proc_simulator.terminate()
        proc_tb.terminate()
        time.sleep(0.5)
        print("testcase %s execute timeout, killed" % testcase_fn)
        sys.stdout.flush()
        os._exit(1)

    if ret_code:
        print("Batch py_tb error at %s" % testcase_fn)
        sys.stdout.flush()
        os._exit(1)

print("All testcase passed")

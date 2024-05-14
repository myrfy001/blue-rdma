# coding: utf-8

import os
import glob
import subprocess
import time
import signal

for testcase_fn in glob.glob("testcase_*.py"):
    # print("=-=-=-=-=-=--=-=-=-=-")
    # print(testcase_fn)
    proc_tb = subprocess.Popen(["python3", testcase_fn], bufsize=0)
    time.sleep(1)
    proc_simulator = subprocess.Popen(
        ["../build/mkTestTop.sh"], cwd="../build")

    ret = proc_tb.wait(10)

    # # proc_simulator.terminate()
    # proc_tb.terminate()

    print("aaaaaaaaaaaaa")

    if ret:
        for _ in range(100):
            print("Batch py_tb error at %s" % testcase_fn)
        time.sleep(1)
        os._exit(1)

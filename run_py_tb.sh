#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

BASH_PROFILE=$HOME/.bash_profile
if [ -f "$BASH_PROFILE" ]; then
    source $BASH_PROFILE
fi

TEST_LOG=run.log
TEST_DIR=test

PY_TEST_DIR=`realpath ./test/py_tb`
PY_LOG_DIR=`realpath ./tmp`
PY_ALL_LOG=$PY_TEST_DIR/run_py_tb.log

FILE="TestTop.bsv"
TESTCASE="mkTestTop"

# Compile test top module
# cd $TEST_DIR
# truncate -s 0 $TEST_LOG
# make -j8 TESTFILE=$FILE TOPMODULE=$TESTCASE 2>&1 | tee -a $TEST_LOG

# Run tests
mkdir -p $PY_LOG_DIR
cd ${PY_TEST_DIR}
python3 ${PY_TEST_DIR}/batch_run.py 2>&1 | tee -a ${PY_ALL_LOG}
exit ${PIPESTATUS[0]}
#! /usr/bin/env bash

set -o errexit
set -o nounset
set -o xtrace

BASH_PROFILE=$HOME/.bash_profile
if [ -f "$BASH_PROFILE" ]; then
    source $BASH_PROFILE
fi

TEST_LOG_MOCK_HOST=system_test_mock_host.log
TEST_LOG_SIMULATOR_A=system_test_simu_a.log
TEST_LOG_SIMULATOR_B=system_test_simu_b.log

MOCK_HOST_PYTHON_SERVER=py_tb/raw_socket_server.py
COMPILED_SIMULATOR_BIN=mkTestTop.sh

TEST_DIR=test

pkill -ef $MOCK_HOST_PYTHON_SERVER || true
pkill -ef $COMPILED_SIMULATOR_BIN || true

if [[ " $@ " =~ " --kill " ]]; then
    exit
fi


cd $TEST_DIR
truncate -s 0 $TEST_LOG_MOCK_HOST
truncate -s 0 $TEST_LOG_SIMULATOR_A
truncate -s 0 $TEST_LOG_SIMULATOR_B

if [[ " $@ " =~ " --make " ]]; then
    FILE=`ls TestTop.bsv`
    make link -j8 TESTFILE=$FILE TOPMODULE=mkTestTop 2>&1 | tee -a $TEST_LOG_MOCK_HOST
fi

python3 $MOCK_HOST_PYTHON_SERVER >> $TEST_LOG_MOCK_HOST 2>&1 &
# make sure python server is up
sleep 0.5

cd build
export MOCK_HOST_SERVER_ADDR=0.0.0.0

# run first simulator
export MOCK_HOST_SERVER_PORT=9874
./$COMPILED_SIMULATOR_BIN > ../$TEST_LOG_SIMULATOR_A 2>&1 &
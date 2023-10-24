
DIR_OUTPUT ?= output
DIR_BUILD = build
DIR_RTL = src/verilog
DIR_BSV_SRC = src/bsv
DIR_XDC = xdc
DIR_IPS = ips
PART = xcvu13p-fhgb2104-2-i
DIR_GENERATE = generate
DIR_BSV_GENERATED = $(DIR_GENERATE)/bsv
DIR_IP_GENERATED = $(DIR_GENERATE)/ip

TARGETFILE ?= $(DIR_BSV_SRC)/user_logic/Top.bsv
BSV_TOPMODULE ?= mkBsvTop
SIM_TOPMODULE ?= mkTB
VERILOG_TOPMODULE ?= top
TARGET_CLOCKS ?= user_clk
MAX_NET_PATH_NUM ?= 1000

export DIR_OUTPUT
export DIR_RTL
export DIR_XDC
export DIR_IPS
export PART
export DIR_BSV_GENERATED
export DIR_IP_GENERATED
export VERILOG_TOPMODULE
export TARGET_CLOCKS
export MAX_NET_PATH_NUM

TRANSFLAGS = -aggressive-conditions # -lift -split-if
RECOMPILEFLAGS = -u -show-compiles
SCHEDFLAGS = -show-schedule -sched-dot # -show-rule-rel dMemInit_request_put doExecute
#	-show-elab-progress
DEBUGFLAGS = -check-assert \
	-continue-after-errors \
	-keep-fires \
	-keep-inlined-boundaries \
	-show-method-bvi \
	-show-method-conf \
	-show-module-use \
	-show-range-conflict \
	-show-stats \
	-warn-action-shadowing \
	-warn-method-urgency \
	-promote-warnings ALL
VERILOGFLAGS = -verilog -remove-dollar -remove-unused-modules # -use-dpi -verilog-filter cmd
BLUESIMFLAGS = -parallel-sim-link 16 # -systemc
DIR_FLAG_OUTDIR = -bdir $(DIR_BUILD) -info-dir $(DIR_BUILD) -simdir $(DIR_BUILD) -vdir $(DIR_BUILD)
DIR_FLAG_WORKDIR = -fdir $(abspath .)
DIR_FLAG_BSV_SRC = -p +:$(abspath $(DIR_BSV_SRC)) \
	-p +:$(abspath $(DIR_BSV_SRC))/blue-wrapper/src \
	-p +:$(abspath $(DIR_BSV_SRC))/user_logic
DIR_FLAGS = $(DIR_FLAG_BSV_SRC) $(DIR_FLAG_OUTDIR) $(DIR_FLAG_WORKDIR)
MISCFLAGS = -print-flags -show-timestamps -show-version # -steps 1000000000000000 -D macro
RUNTIMEFLAGS = +RTS -K256M -RTS
SIMEXE = $(DIR_BUILD)/out


compile:
	mkdir -p $(DIR_BUILD)
	bsc -elab -sim -verbose $(BLUESIMFLAGS) $(DEBUGFLAGS) $(DIR_FLAGS) $(MISCFLAGS) $(RECOMPILEFLAGS) $(RUNTIMEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -g $(BSV_TOPMODULE) $(TARGETFILE)

link: compile
	bsc -elab -sim -verbose $(BLUESIMFLAGS) $(DEBUGFLAGS) $(DIR_FLAGS) $(MISCFLAGS) $(RECOMPILEFLAGS) $(RUNTIMEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -g $(SIM_TOPMODULE) $(TARGETFILE)
	bsc -sim $(BLUESIMFLAGS) $(DIR_FLAGS) $(RECOMPILEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -e $(SIM_TOPMODULE) -o $(SIMEXE)


verilog: compile
	bsc $(VERILOGFLAGS) $(DIR_FLAGS) $(RECOMPILEFLAGS) $(TRANSFLAGS) -g $(BSV_TOPMODULE) $(TARGETFILE)
	mkdir -p $(DIR_BSV_GENERATED)
	bluetcl listVlogFiles.tcl -bdir $(DIR_BUILD) -vdir $(DIR_BUILD) $(BSV_TOPMODULE) $(BSV_TOPMODULE) | grep -i '\.v' | xargs -I {} cp {} $(DIR_BSV_GENERATED)

vivado:
	vivado -mode batch -source build_top.tcl 2>&1 | tee ./run.log

vcheck:
	iverilog -l $(DIR_IP_GENERATED)/xdma_0/xdma_0_stub.v -y $(DIR_RTL) -y $(DIR_BSV_GENERATED) $(DIR_RTL)/top.v

clean:
	rm -rf $(DIR_OUTPUT) $(DIR_GENERATE) $(DIR_BUILD) .Xil .gen .srcs *.jou *.log

PHONY: vivado
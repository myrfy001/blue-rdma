include ../Makefile.base

TESTFILE ?= TestExtractAndPrependPipeOut.bsv
TOPMODULE ?= mkTestHeaderAndDataStreamConversion
DIR_BSV_GENERATED ?= $(BUILDDIR)

SIMSCRIPT_NAME = ./$(TOPMODULE).sh

gen_udp_crc_tab:
	python3 $(BLUE_CRC_DIR)/scripts/gen_crc_tab.py \
			$(BLUE_UDP_CRC_TABLE_GEN_INFO_DIR)/crc_ieee_32_256.json \
			$(DIR_BSV_GENERATED)

compile:
	mkdir -p $(BUILDDIR)
	bsc -elab -sim -verbose $(BLUESIMFLAGS) $(DEBUGFLAGS) $(DIRFLAGS) $(MISCFLAGS) $(RECOMPILEFLAGS) $(RUNTIMEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -g $(TOPMODULE) $(TESTFILE)

link: compile
	bsc -sim $(BLUESIMFLAGS) $(DIRFLAGS) $(RECOMPILEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -e $(TOPMODULE) -o $(BUILDDIR)/$(SIMSCRIPT_NAME) $(BSV_C_EXT_FILES)

simulate: link gen_udp_crc_tab
	cd $(BUILDDIR) && $(SIMSCRIPT_NAME)

clean:
	rm -rf $(BUILDDIR)

.PHONY: compile link simulate clean
.DEFAULT_GOAL := simulate

include ../Makefile.base

TESTFILE ?= TestExtractAndPrependPipeOut.bsv
TOPMODULE ?= mkTestHeaderAndDataStreamConversion

SIMSCRIPT = $(BUILDDIR)/$(TOPMODULE).sh

test_host_memory.hex:
	python3 generate_test_host_memory.py

compile:
	mkdir -p $(BUILDDIR)
	bsc -elab -sim -verbose $(BLUESIMFLAGS) $(DEBUGFLAGS) $(DIRFLAGS) $(MISCFLAGS) $(RECOMPILEFLAGS) $(RUNTIMEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -g $(TOPMODULE) $(TESTFILE)

link: compile
	bsc -sim $(BLUESIMFLAGS) $(DIRFLAGS) $(RECOMPILEFLAGS) $(SCHEDFLAGS) $(TRANSFLAGS) -e $(TOPMODULE) -o $(SIMSCRIPT)

simulate: test_host_memory.hex link
	$(SIMSCRIPT)

clean:
	rm -rf $(BUILDDIR)

.PHONY: compile link simulate clean
.DEFAULT_GOAL := simulate

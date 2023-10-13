all:
	bsc -sim \
		-p +:%/Libraries/AMBA_TLM3/Axi4 \
		-p +:%/Libraries/AMBA_TLM3/TLM3 \
		-p +:%/Libraries/AMBA_TLM3/Axi \
		-p +:%/Libraries/Bus \
		a.bsv
	
	bsc -systemc -e mkTop mkTop.ba
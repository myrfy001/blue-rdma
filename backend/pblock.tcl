proc place_new_pblock {name pos cells contain_routing} {
    create_pblock ${name}
    resize_pblock [get_pblocks ${name}] -add ${pos}
    add_cells_to_pblock [get_pblocks ${name}] ${cells}
    set_property CONTAIN_ROUTING ${contain_routing} [get_pblocks ${name}]
}

proc append_to_pblock {name cells} {
    add_cells_to_pblock [get_pblocks ${name}] ${cells}
}
    

# place_new_pblock pblock_mmcm {CLOCKREGION_X4Y8:CLOCKREGION_X4Y8} [get_cells -regex -hierarchical  mmcm_250_to_500.*] true


# place_new_pblock pblock_udp {CLOCKREGION_X1Y11:CLOCKREGION_X5Y8} [get_cells bsv_top/udpAndRdma/udp] true

place_new_pblock pblock_bsv_cmac_wrapper {SLICE_X1Y669:SLICE_X90Y482} [get_cells {bsv_top/axiStream512SyncFifoForCMAC* bsv_top/xilinxCmacCtrl*}] true

# place_new_pblock pblock_wrapper {CLOCKREGION_X6Y9:CLOCKREGION_X6Y8}  [get_cells {bsv_top/xdmaWrap}] true
# place_new_pblock pblock_xdma_bridge {CLOCKREGION_X6Y10:CLOCKREGION_X7Y8}  [get_cells {bsv_top/xdmaAxiLiteWrap* bsv_top/bsvTopCore/xdma*}] true

# place_new_pblock pblock_sq {CLOCKREGION_X1Y12:CLOCKREGION_X4Y15}  [get_cells bsv_top/udpAndRdma/rdma/qp/sq] true
# place_new_pblock pblock_rq {CLOCKREGION_X5Y12:CLOCKREGION_X7Y15}  [get_cells bsv_top/udpAndRdma/rdma/qp/rq] true

# place_new_pblock pblock_tlb {SLICE_X108Y717:SLICE_X143Y542}  [get_cells bsv_top/bsvTopCore/tlb] true
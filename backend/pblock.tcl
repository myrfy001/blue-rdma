proc place_new_pblock {name pos cells contain_routing} {
    create_pblock ${name}
    resize_pblock [get_pblocks ${name}] -add ${pos}
    add_cells_to_pblock [get_pblocks ${name}] ${cells}
    set_property CONTAIN_ROUTING ${contain_routing} [get_pblocks ${name}]
}

proc append_to_pblock {name cells} {
    add_cells_to_pblock [get_pblocks ${name}] ${cells}
}
    

place_new_pblock pblock_mmcm {CLOCKREGION_X4Y8:CLOCKREGION_X4Y8} [get_cells -regex -hierarchical  mmcm_250_to_500.*] true
place_new_pblock pblock_udp {CLOCKREGION_X2Y10:CLOCKREGION_X3Y8} [get_cells bsv_userlogic_top_inst/bsvTopCore/udp] true

place_new_pblock pblock_bsv_cmac_wrapper {SLICE_X1Y669:SLICE_X59Y482} [get_cells {bsv_userlogic_top_inst/axiStream512SyncFifoForCMAC* bsv_userlogic_top_inst/xilinxCmacCtrl*}] true

place_new_pblock pblock_wrapper {CLOCKREGION_X6Y9:CLOCKREGION_X6Y8}  [get_cells {bsv_userlogic_top_inst/xdmaWrap}] true
place_new_pblock pblock_xdma_bridge {CLOCKREGION_X6Y10:CLOCKREGION_X7Y8}  [get_cells {bsv_userlogic_top_inst/xdmaAxiLiteWrap* bsv_userlogic_top_inst/bsvTopCore/xdma*}] true

place_new_pblock pblock_sq {CLOCKREGION_X4Y10:CLOCKREGION_X5Y8}  [get_cells bsv_userlogic_top_inst/bsvTopCore/sq] true
place_new_pblock pblock_rq {CLOCKREGION_X5Y11:CLOCKREGION_X6Y11}  [get_cells bsv_userlogic_top_inst/bsvTopCore/rq] true

# place_new_pblock pblock_tlb {SLICE_X108Y717:SLICE_X143Y542}  [get_cells bsv_userlogic_top_inst/bsvTopCore/tlb] true
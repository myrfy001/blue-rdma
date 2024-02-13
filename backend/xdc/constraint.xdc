# reset and clock
set_property LOC [get_package_pins -filter {PIN_FUNC =~ *_PERSTN0_65}] [get_ports sys_rst_n]
set_property LOC [get_package_pins -of_objects [get_bels [get_sites -filter {NAME =~ *COMMON*} -of_objects [get_iobanks -of_objects [get_sites GTYE4_CHANNEL_X1Y23]]]/REFCLK0P]] [get_ports sys_clk_p]
set_property LOC [get_package_pins -of_objects [get_bels [get_sites -filter {NAME =~ *COMMON*} -of_objects [get_iobanks -of_objects [get_sites GTYE4_CHANNEL_X1Y23]]]/REFCLK0N]] [get_ports sys_clk_n]

# bitstream
set_property BITSTREAM.CONFIG.EXTMASTERCCLK_EN div-1 [current_design]
set_property BITSTREAM.CONFIG.BPI_SYNC_MODE Type1 [current_design]
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property BITSTREAM.CONFIG.UNUSEDPIN Pulldown [current_design]

# global voltage level
set_property PULLUP true [get_ports sys_rst_n]
set_property CONFIG_MODE BPI16 [current_design]
set_property CONFIG_VOLTAGE 1.8 [current_design]
set_property IOSTANDARD LVCMOS18 [get_ports sys_rst_n]

# clock
create_clock -name sys_clk -period 10 [get_ports sys_clk_p]

# SLR Partition
set_property USER_SLR_ASSIGNMENT SLR1 [get_cells {mmcm_250_to_500 xdma_0_i refclk_ibuf sys_reset_n_ibuf}]
set_property USER_SLR_ASSIGNMENT SLR2 [get_cells {cmac_inst} ]
# set_property USER_SLR_ASSIGNMENT SLR3 [get_cells {udp_cmac_inst1 } ]
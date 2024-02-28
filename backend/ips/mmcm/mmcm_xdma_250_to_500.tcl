set dir_gen $::env(DIR_IP_GENERATED)

file mkdir $dir_gen
create_ip -name clk_wiz -vendor xilinx.com -library ip -version 6.0 -module_name clk_wiz_xdma_250_to_500 -dir $dir_ip_gen -force

set_property -dict [list \
  CONFIG.CLKIN1_JITTER_PS {40.0} \
  CONFIG.CLKOUT1_DRIVES {BUFGCE} \
  CONFIG.CLKOUT1_JITTER {68.149} \
  CONFIG.CLKOUT1_PHASE_ERROR {71.064} \
  CONFIG.CLKOUT1_REQUESTED_OUT_FREQ {500.000} \
  CONFIG.CLKOUT2_DRIVES {BUFGCE} \
  CONFIG.CLKOUT3_DRIVES {BUFGCE} \
  CONFIG.CLKOUT4_DRIVES {BUFGCE} \
  CONFIG.CLKOUT5_DRIVES {BUFGCE} \
  CONFIG.CLKOUT6_DRIVES {BUFGCE} \
  CONFIG.CLKOUT7_DRIVES {BUFGCE} \
  CONFIG.FEEDBACK_SOURCE {FDBK_AUTO} \
  CONFIG.JITTER_SEL {Min_O_Jitter} \
  CONFIG.MMCM_BANDWIDTH {HIGH} \
  CONFIG.MMCM_CLKFBOUT_MULT_F {6.250} \
  CONFIG.MMCM_CLKIN1_PERIOD {4.000} \
  CONFIG.MMCM_CLKIN2_PERIOD {10.0} \
  CONFIG.MMCM_CLKOUT0_DIVIDE_F {3.125} \
  CONFIG.OPTIMIZE_CLOCKING_STRUCTURE_EN {true} \
  CONFIG.PRIM_IN_FREQ {250.000} \
  CONFIG.SECONDARY_SOURCE {Single_ended_clock_capable_pin} \
  CONFIG.USE_PHASE_ALIGNMENT {true} \
  CONFIG.USE_RESET {false} \
  CONFIG.USE_SAFE_CLOCK_STARTUP {false} \
] [get_ips clk_wiz_xdma_250_to_500]




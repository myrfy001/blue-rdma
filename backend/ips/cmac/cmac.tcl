# 100G CMAC IP
create_ip -name cmac_usplus -vendor xilinx.com -library ip -version 3.1 \
          -module_name cmac_usplus_0 -dir $dir_ip_gen -force
set_property -dict [list \
    CONFIG.CMAC_CAUI4_MODE {1} \
    CONFIG.USER_INTERFACE {AXIS} \
    CONFIG.CMAC_CORE_SELECT {CMACE4_X0Y6} \
    CONFIG.GT_GROUP_SELECT {X0Y32~X0Y35} \
    CONFIG.GT_DRP_CLK {250.00} \
    CONFIG.INCLUDE_RS_FEC {1} \
] [get_ips cmac_usplus_0]
set dir_output $::env(DIR_OUTPUT)
set dir_rtl $::env(DIR_RTL)
set dir_xdc $::env(DIR_XDC)
set dir_ooc_scripts $::env(DIR_OOC_SCRIPTS)
set dir_ips $::env(DIR_IPS)
set dir_ip_gen $::env(DIR_IP_GENERATED)
set dir_bsv_gen $::env(DIR_BSV_GENERATED)
set part $::env(PART)
set top_module $::env(VERILOG_TOPMODULE)
set target_clks $::env(TARGET_CLOCKS)
set max_net_path_num $::env(MAX_NET_PATH_NUM)

set_param general.maxthreads 24
set device [get_parts $part]; # xcvu13p-fhgb2104-2-i; #
set_part $device

proc runGenerateIP {args} {
    global dir_output part device dir_ips device

    file mkdir $dir_output

    foreach file [ glob $dir_ips/**/*.tcl ] {
        source $file
    }

    report_property $device -file $dir_output/pre_synth_dev_prop.rpt
    reset_target all [ get_ips * ]
    generate_target all [ get_ips * ]

}

proc runSynthIP {args} {
    global dir_output top_module dir_ip_gen dir_xdc

    read_xdc [ glob $dir_xdc/*.xdc ]
    
    read_ip [glob $dir_ip_gen/**/*.xci]
    # The following line will generate a .dcp checkpoint file, so no need to create by ourselves
    synth_ip [ get_ips * ] -quiet
}


proc runSynthOOC {args} {
    global dir_output part dir_bsv_gen dir_ooc_scripts dir_ooc_scripts max_net_path_num

    set ooc_module_names { \
        mkTLB \
        mkMemRegionTable \
        mkDmaReadReqAddrTranslator \
        mkExtractHeaderFromRdmaPktPipeOut \
        mkMrAndPgtManager \
        mkQPContext \
        mkRQ \
        mkXdmaGearbox \
    }

    foreach ooc_top $ooc_module_names {
        source ooc_tcl_and_xdc/bsv_ooc_module_common.tcl
    }
}


proc addExtFiles {args} {
    global dir_output part device dir_rtl dir_xdc dir_ip_gen dir_bsv_gen
    read_ip [glob $dir_ip_gen/**/*.xci]
    read_verilog [ glob $dir_rtl/*.v ]
    read_verilog [ glob $dir_bsv_gen/*.v ]
    remove_files {mkTLB.v mkMemRegionTable.v}
    read_checkpoint ${dir_output}/ooc/mkTLB/mkTLB.dcp
    read_checkpoint ${dir_output}/ooc/mkMemRegionTable/mkMemRegionTable.dcp
    read_xdc [ glob $dir_xdc/*.xdc ]
}


proc runSynthDesign {args} {
    global dir_output top_module max_net_path_num

    synth_design -top $top_module -retiming
    write_checkpoint -force $dir_output/post_synth_design.dcp
    write_xdc -force -exclude_physical $dir_output/post_synth.xdc

    # source batch_insert_ila.tcl
    # batch_insert_ila 8192
}


proc runPostSynthReport {args} {
    global dir_output target_clks max_net_path_num

    if {[dict get $args -open_checkpoint] == true} {
        open_checkpoint $dir_output/post_synth_design.dcp
    }

    # Check 1) slack, 2) requirement, 3) src and dst clocks, 4) datapath delay, 5) logic level, 6) skew and uncertainty.
    report_timing_summary -report_unconstrained -warn_on_violation -file $dir_output/post_synth_timing_summary.rpt
######report_timing -of_objects [get_timing_paths -setup -to [get_clocks $target_clks] -max_paths $max_net_path_num -filter { LOGIC_LEVELS >= 4 && LOGIC_LEVELS <= 40 }] -file $dir_output/post_synth_long_paths.rpt
    # Check 1) endpoints without clock, 2) combo loop and 3) latch.
    check_timing -override_defaults no_clock -file $dir_output/post_synth_check_timing.rpt
    report_clock_networks -file $dir_output/post_synth_clock_networks.rpt; # Show unconstrained clocks
    report_clock_interaction -delay_type min_max -significant_digits 3 -file $dir_output/post_synth_clock_interaction.rpt; # Pay attention to Clock pair Classification, Inter-CLock Constraints, Path Requirement (WNS)
    report_high_fanout_nets -timing -load_type -max_nets $max_net_path_num -file $dir_output/post_synth_fanout.rpt
    report_exceptions -ignored -file $dir_output/post_synth_exceptions.rpt; # -ignored -ignored_objects -write_valid_exceptions -write_merged_exceptions

    # 1 LUT + 1 net have delay 0.5ns, if cycle period is Tns, logic level is 2T at most
    # report_design_analysis -timing -max_paths $max_net_path_num -file $dir_output/post_synth_design_timing.rpt
    report_design_analysis -setup -max_paths $max_net_path_num -file $dir_output/post_synth_design_setup_timing.rpt
    # report_design_analysis -logic_level_dist_paths $max_net_path_num -min_level $MIN_LOGIC_LEVEL -max_level $MAX_LOGIC_LEVEL -file $dir_output/post_synth_design_logic_level.rpt
    report_design_analysis -logic_level_dist_paths $max_net_path_num -logic_level_distribution -file $dir_output/post_synth_design_logic_level_dist.rpt

    report_datasheet -file $dir_output/post_synth_datasheet.rpt
    xilinx::designutils::report_failfast -detailed_reports synth -file $dir_output/post_synth_failfast.rpt

    report_drc -file $dir_output/post_synth_drc.rpt
    report_drc -ruledeck methodology_checks -file $dir_output/post_synth_drc_methodology.rpt
    report_drc -ruledeck timing_checks -file $dir_output/post_synth_drc_timing.rpt

    # intra-clock skew < 300ps, inter-clock skew < 500ps

    # Check 1) LUT on clock tree (TIMING-14), 2) hold constraints for multicycle path constraints (XDCH-1).
    report_methodology -file $dir_output/post_synth_methodology.rpt
    report_timing -max $max_net_path_num -slack_less_than 0 -file $dir_output/post_synth_timing.rpt

    report_compile_order -constraints -file $dir_output/post_synth_constraints.rpt; # Verify IP constraints included
    report_utilization -file $dir_output/post_synth_util.rpt; # -cells -pblocks
    report_cdc -file $dir_output/post_synth_cdc.rpt
    report_clocks -file $dir_output/post_synth_clocks.rpt; # Verify clock settings

    # Use IS_SEQUENTIAL for -from/-to
    # Instantiate XPM_CDC modules
    # write_xdc -force -exclude_physical -exclude_timing -constraints INVALID

    report_qor_assessment -report_all_suggestions -csv_output_dir $dir_output -file $dir_output/post_synth_qor_assess.rpt
}


proc runPlacement {args} {
    global dir_output top_module

    if {[dict get $args -open_checkpoint] == true} {
        open_checkpoint $dir_output/post_synth_design.dcp
    }

    opt_design -remap -verbose
    power_opt_design
    place_design
    # Optionally run optimization if there are timing violations after placement
    if {[get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]] < 0} {
        puts "Found setup timing violations => running physical optimization"
        phys_opt_design
    }
    write_checkpoint -force $dir_output/post_place.dcp
    write_xdc -force -exclude_physical $dir_output/post_place.xdc
}


proc runRoute {args} {
    global dir_output top_module

    if {[dict get $args -open_checkpoint] == true} {
        open_checkpoint $dir_output/post_place.dcp
    }

    route_design

    proc runPPO { {num_iters 1} {enable_phys_opt 1} } {
        for {set idx 0} {$idx < $num_iters} {incr idx} {
            place_design -post_place_opt; # Better to run after route
            if {$enable_phys_opt != 0} {
                phys_opt_design
            }
            route_design
            if {[get_property SLACK [get_timing_paths ]] >= 0} {
                break; # Stop if timing closure
            }
        }
    }

    # runPPO 4 1; # num_iters=4, enable_phys_opt=1

    write_checkpoint -force $dir_output/post_route.dcp
    write_xdc -force -exclude_physical $dir_output/post_route.xdc

    write_verilog -force $dir_output/post_impl_netlist.v -mode timesim -sdf_anno true

}


proc runPostRouteReport {args} {
    global dir_output target_clks max_net_path_num

    if {[dict get $args -open_checkpoint] == true} {
        open_checkpoint $dir_output/post_route.dcp
    }

    report_timing_summary -report_unconstrained -warn_on_violation -file $dir_output/post_route_timing_summary.rpt
######report_timing -of_objects [get_timing_paths -hold -to [get_clocks $target_clks] -max_paths $max_net_path_num -filter { LOGIC_LEVELS >= 4 && LOGIC_LEVELS <= 40 }] -file $dir_output/post_route_long_paths.rpt
    report_methodology -file $dir_output/post_route_methodology.rpt
    report_timing -max $max_net_path_num -slack_less_than 0 -file $dir_output/post_route_timing.rpt

    report_route_status -file $dir_output/post_route_status.rpt
    report_drc -file $dir_output/post_route_drc.rpt
    report_drc -ruledeck methodology_checks -file $dir_output/post_route_drc_methodology.rpt
    report_drc -ruledeck timing_checks -file $dir_output/post_route_drc_timing.rpt
    # Check unique control sets < 7.5% of total slices, at most 15%
    report_control_sets -verbose -file $dir_output/post_route_control_sets.rpt

    report_power -file $dir_output/post_route_power.rpt
    report_power_opt -file $dir_output/post_route_power_opt.rpt
    report_utilization -file $dir_output/post_route_util.rpt
    report_ram_utilization -detail -file $dir_output/post_route_ram_utils.rpt
    # Check fanout < 25K
    report_high_fanout_nets -file $dir_output/post_route_fanout.rpt

    report_design_analysis -hold -max_paths $max_net_path_num -file $dir_output/post_route_design_hold_timing.rpt
    # Check initial estimated router congestion level no more than 5, type (global, long, short) and top cells
    report_design_analysis -congestion -file $dir_output/post_route_congestion.rpt
    # Check difficult modules (>15K cells) with high Rent Exponent (complex logic cone) >= 0.65 and/or Avg. Fanout >= 4
    report_design_analysis -complexity -file $dir_output/post_route_complexity.rpt; # -hierarchical_depth
    # If congested, check problematic cells using report_utilization -cells
    # If congested, try NetDelay* for UltraScale+, or try SpredLogic* for UltraScale in implementation strategy

    xilinx::designutils::report_failfast -detailed_reports impl -file $dir_output/post_route_failfast.rpt
    # xilinx::ultrafast::report_io_reg -file $dir_output/post_route_io_reg.rpt
    report_io -file $dir_output/post_route_io.rpt
    report_pipeline_analysis -file $dir_output/post_route_pipeline.rpt
    report_qor_assessment -report_all_suggestions -csv_output_dir $dir_output -file $dir_output/post_route_qor_assess.rpt
    report_qor_suggestions -report_all_suggestions -csv_output_dir $dir_output -file $dir_output/post_route_qor_suggest.rpt
}

proc runWriteBitStream {args} {
    global dir_output top_module

    if {[dict get $args -open_checkpoint] == true} {
        open_checkpoint $dir_output/post_route.dcp
    }

    write_bitstream -force $dir_output/top.bit
}

proc runProgramDevice {args} {
    global dir_output top_module

    open_hw_manager
    connect_hw_server -allow_non_jtag
    open_hw_target
    current_hw_device [get_hw_devices xcvu13p_0]
    refresh_hw_device -update_hw_probes false [lindex [get_hw_devices xcvu13p_0] 0]
    
    # set_property PROBES.FILE {/home/mingheng/xdma_0_ex/xdma_0_ex.runs/impl_1/top.ltx} [get_hw_devices xcvu13p_0]
    # set_property FULL_PROBES.FILE {/home/mingheng/xdma_0_ex/xdma_0_ex.runs/impl_1/top.ltx} [get_hw_devices xcvu13p_0]

    set_property PROGRAM.FILE $dir_output/top.bit [get_hw_devices xcvu13p_0]
    program_hw_devices [get_hw_devices xcvu13p_0]
}

# runGenerateIP -open_checkpoint false
# runSynthIP -open_checkpoint false
# runSynthOOC
# addExtFiles -open_checkpoint false
# runSynthDesign -open_checkpoint false
# runPostSynthReport -open_checkpoint false
runPlacement -open_checkpoint true
runRoute -open_checkpoint false
runPostRouteReport -open_checkpoint false
# runWriteBitStream -open_checkpoint false
# runProgramDevice -open_checkpoint false
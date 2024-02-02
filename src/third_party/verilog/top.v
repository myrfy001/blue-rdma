`timescale 1ps / 1ps

module top#(
   parameter PL_LINK_CAP_MAX_LINK_WIDTH          = 16,            // 1- X1; 2 - X2; 4 - X4; 8 - X8
   parameter C_DATA_WIDTH                        = 512

)(
    // PCIe and XDMA
    output [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0] pci_exp_txp,
    output [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0] pci_exp_txn,
    input [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0]  pci_exp_rxp,
    input [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0]  pci_exp_rxn,

    input 					 sys_clk_p,
    input 					 sys_clk_n,
    input 					 sys_rst_n

    // CMAC
    input qsfp1_ref_clk_p,
    input qsfp1_ref_clk_n,

    input qsfp2_ref_clk_p,
    input qsfp2_ref_clk_n,

    input  [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp1_rxn_in,
    input  [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp1_rxp_in,
    output [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp1_txn_out,
    output [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp1_txp_out,

    input  [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp2_rxn_in,
    input  [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp2_rxp_in,
    output [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp2_txn_out,
    output [CMAC_GT_LANE_WIDTH - 1 : 0] qsfp2_txp_out
);

   //-----------------------------------------------------------------------------------------------------------------------



   
   wire 					   user_lnk_up;
   
   //----------------------------------------------------------------------------------------------------------------//
   //  AXI Interface                                                                                                 //
   //----------------------------------------------------------------------------------------------------------------//
   
   wire 					   user_clk_250;
   wire              user_clk_500;
   wire 					   user_resetn;



  //----------------------------------------------------------------------------------------------------------------//
  //    System(SYS) Interface                                                                                       //
  //----------------------------------------------------------------------------------------------------------------//

    wire                                    sys_clk;
    wire                                    sys_clk_gt;
    wire                                    sys_rst_n_c;




//////////////////////////////////////////////////  LITE
   //-- AXI Master Write Address Channel
    wire [31:0] m_axil_awaddr;
    wire [2:0]  m_axil_awprot;
    wire 	m_axil_awvalid;
    wire 	m_axil_awready;

    //-- AXI Master Write Data Channel
    wire [31:0] m_axil_wdata;
    wire [3:0]  m_axil_wstrb;
    wire 	m_axil_wvalid;
    wire 	m_axil_wready;
    //-- AXI Master Write Response Channel
    wire 	m_axil_bvalid;
    wire 	m_axil_bready;
    //-- AXI Master Read Address Channel
    wire [31:0] m_axil_araddr;
    wire [2:0]  m_axil_arprot;
    wire 	m_axil_arvalid;
    wire 	m_axil_arready;
    //-- AXI Master Read Data Channel
    wire [31:0] m_axil_rdata;
    wire [1:0]  m_axil_rresp;
    wire 	m_axil_rvalid;
    wire 	m_axil_rready;
    wire [1:0]  m_axil_bresp;

    // wire [2:0]    msi_vector_width;
    // wire          msi_enable;

      // AXI streaming ports
    wire [C_DATA_WIDTH-1:0]	m_axis_h2c_tdata_0;
    wire 			m_axis_h2c_tlast_0;
    wire 			m_axis_h2c_tvalid_0;
    wire 			m_axis_h2c_tready_0;
    wire [C_DATA_WIDTH/8-1:0]	m_axis_h2c_tkeep_0;
    
    wire [C_DATA_WIDTH-1:0] s_axis_c2h_tdata_0; 
    wire s_axis_c2h_tlast_0;
    wire s_axis_c2h_tvalid_0;
    wire s_axis_c2h_tready_0;
    wire [C_DATA_WIDTH/8-1:0] s_axis_c2h_tkeep_0; 

    wire [7:0] c2h_sts_0;
    wire [7:0] h2c_sts_0;


  // Ref clock buffer
  IBUFDS_GTE4 # (.REFCLK_HROW_CK_SEL(2'b00)) refclk_ibuf (.O(sys_clk_gt), .ODIV2(sys_clk), .I(sys_clk_p), .CEB(1'b0), .IB(sys_clk_n));
  // Reset buffer
  IBUF   sys_reset_n_ibuf (.O(sys_rst_n_c), .I(sys_rst_n));

// Descriptor Bypass Control Logic
  wire c2h_dsc_byp_ready_0;
  wire [63 : 0] c2h_dsc_byp_src_addr_0;
  wire [63 : 0] c2h_dsc_byp_dst_addr_0;
  wire [27 : 0] c2h_dsc_byp_len_0;
  wire [4 : 0] c2h_dsc_byp_ctl_0;
  wire c2h_dsc_byp_load_0;
  wire h2c_dsc_byp_ready_0;
  wire [63 : 0] h2c_dsc_byp_src_addr_0;
  wire [63 : 0] h2c_dsc_byp_dst_addr_0;
  wire [27 : 0] h2c_dsc_byp_len_0;
  wire [4 : 0] h2c_dsc_byp_ctl_0;
  wire h2c_dsc_byp_load_0;


  clk_wiz_xdma_250_to_500 mmcm_250_to_500
   (
      // Clock out ports
      .clk_out1(user_clk_500),     // output clk_out1
      // Status and control signals
      .locked(locked),       // output locked
      // Clock in ports
      .clk_in1(user_clk_250)      // input clk_in1
  );


  xdma_0 xdma_0_i
     (
      .sys_rst_n       ( sys_rst_n_c ),

      .sys_clk         ( sys_clk ),
      .sys_clk_gt      ( sys_clk_gt),
      
      // Tx
      .pci_exp_txn     ( pci_exp_txn ),
      .pci_exp_txp     ( pci_exp_txp ),
      
      // Rx
      .pci_exp_rxn     ( pci_exp_rxn ),
      .pci_exp_rxp     ( pci_exp_rxp ),
      


      // AXI streaming ports
      .s_axis_c2h_tdata_0(s_axis_c2h_tdata_0),  
      .s_axis_c2h_tlast_0(s_axis_c2h_tlast_0),
      .s_axis_c2h_tvalid_0(s_axis_c2h_tvalid_0), 
      .s_axis_c2h_tready_0(s_axis_c2h_tready_0), 
      .s_axis_c2h_tkeep_0(s_axis_c2h_tkeep_0),



      .m_axis_h2c_tdata_0(m_axis_h2c_tdata_0),
      .m_axis_h2c_tlast_0(m_axis_h2c_tlast_0),
      .m_axis_h2c_tvalid_0(m_axis_h2c_tvalid_0),
      .m_axis_h2c_tready_0(m_axis_h2c_tready_0),
      .m_axis_h2c_tkeep_0(m_axis_h2c_tkeep_0),

      .c2h_sts_0(c2h_sts_0),                            // output wire [7 : 0] c2h_sts_0
      .h2c_sts_0(h2c_sts_0),                            // output wire [7 : 0] h2c_sts_0

      // LITE interface   
      //-- AXI Master Write Address Channel
      .m_axil_awaddr    (m_axil_awaddr),
      .m_axil_awprot    (m_axil_awprot),
      .m_axil_awvalid   (m_axil_awvalid),
      .m_axil_awready   (m_axil_awready),
      //-- AXI Master Write Data Channel
      .m_axil_wdata     (m_axil_wdata),
      .m_axil_wstrb     (m_axil_wstrb),
      .m_axil_wvalid    (m_axil_wvalid),
      .m_axil_wready    (m_axil_wready),
      //-- AXI Master Write Response Channel
      .m_axil_bvalid    (m_axil_bvalid),
      .m_axil_bresp     (m_axil_bresp),
      .m_axil_bready    (m_axil_bready),
      //-- AXI Master Read Address Channel
      .m_axil_araddr    (m_axil_araddr),
      .m_axil_arprot    (m_axil_arprot),
      .m_axil_arvalid   (m_axil_arvalid),
      .m_axil_arready   (m_axil_arready),
      //-- AXI Master Read Data Channel
      .m_axil_rdata     (m_axil_rdata),
      .m_axil_rresp     (m_axil_rresp),
      .m_axil_rvalid    (m_axil_rvalid),
      .m_axil_rready    (m_axil_rready),


      // Descriptor Bypass
      .c2h_dsc_byp_ready_0    (c2h_dsc_byp_ready_0),
      .c2h_dsc_byp_src_addr_0 (c2h_dsc_byp_src_addr_0),
      .c2h_dsc_byp_dst_addr_0 (c2h_dsc_byp_dst_addr_0),
      .c2h_dsc_byp_len_0      (c2h_dsc_byp_len_0),
      .c2h_dsc_byp_ctl_0      (c2h_dsc_byp_ctl_0),
      .c2h_dsc_byp_load_0     (c2h_dsc_byp_load_0),

      .h2c_dsc_byp_ready_0    (h2c_dsc_byp_ready_0),
      .h2c_dsc_byp_src_addr_0 (h2c_dsc_byp_src_addr_0),
      .h2c_dsc_byp_dst_addr_0 (h2c_dsc_byp_dst_addr_0),
      .h2c_dsc_byp_len_0      (h2c_dsc_byp_len_0),
      .h2c_dsc_byp_ctl_0      (h2c_dsc_byp_ctl_0),
      .h2c_dsc_byp_load_0     (h2c_dsc_byp_load_0),


      // .usr_irq_req  (usr_irq_req),
      // .usr_irq_ack  (usr_irq_ack),

      //-- AXI Global
      .axi_aclk        ( user_clk_250),
      .axi_aresetn     ( user_resetn ),
      .user_lnk_up     ( user_lnk_up )
    );


    mkBsvTop bsv_userlogic_top_inst(
      .CLK_slowClock(user_clk_250),
      .RST_N_slowReset(user_resetn),
      .CLK(user_clk_500),
      .RST_N(user_resetn),
      .xdmaChannel_rawH2cAxiStream_tvalid(m_axis_h2c_tvalid_0),
      .xdmaChannel_rawH2cAxiStream_tdata(m_axis_h2c_tdata_0),
      .xdmaChannel_rawH2cAxiStream_tkeep(m_axis_h2c_tkeep_0),
      .xdmaChannel_rawH2cAxiStream_tlast(m_axis_h2c_tlast_0),
      .xdmaChannel_rawH2cAxiStream_tready(m_axis_h2c_tready_0),

      .xdmaChannel_rawC2hAxiStream_tvalid(s_axis_c2h_tvalid_0),
      .xdmaChannel_rawC2hAxiStream_tdata(s_axis_c2h_tdata_0),
      .xdmaChannel_rawC2hAxiStream_tkeep(s_axis_c2h_tkeep_0),
      .xdmaChannel_rawC2hAxiStream_tlast(s_axis_c2h_tlast_0),
      .xdmaChannel_rawC2hAxiStream_tready(s_axis_c2h_tready_0),

      .xdmaChannel_h2cDescByp_ready(h2c_dsc_byp_ready_0),

      .xdmaChannel_h2cDescByp_load(h2c_dsc_byp_load_0),

      .xdmaChannel_h2cDescByp_src_addr(h2c_dsc_byp_src_addr_0),

      .xdmaChannel_h2cDescByp_dst_addr(h2c_dsc_byp_dst_addr_0),

      .xdmaChannel_h2cDescByp_len(h2c_dsc_byp_len_0),

      .xdmaChannel_h2cDescByp_ctl(h2c_dsc_byp_ctl_0),

      .xdmaChannel_h2cDescByp_desc_done(h2c_sts_0[3]),

      .xdmaChannel_c2hDescByp_ready(c2h_dsc_byp_ready_0),

      .xdmaChannel_c2hDescByp_load(c2h_dsc_byp_load_0),

      .xdmaChannel_c2hDescByp_src_addr(c2h_dsc_byp_src_addr_0),

      .xdmaChannel_c2hDescByp_dst_addr(c2h_dsc_byp_dst_addr_0),

      .xdmaChannel_c2hDescByp_len(c2h_dsc_byp_len_0),

      .xdmaChannel_c2hDescByp_ctl(c2h_dsc_byp_ctl_0),

      .xdmaChannel_c2hDescByp_desc_done(c2h_sts_0[3]),

      .axilRegBlock_awvalid(m_axil_awvalid),
      .axilRegBlock_awaddr(m_axil_awaddr),
      .axilRegBlock_awprot(m_axil_awprot),

      .axilRegBlock_awready(m_axil_awready),

      .axilRegBlock_wvalid(m_axil_wvalid),
      .axilRegBlock_wdata(m_axil_wdata),
      .axilRegBlock_wstrb(m_axil_wstrb),

      .axilRegBlock_wready(m_axil_wready),

      .axilRegBlock_bvalid(m_axil_bvalid),

      .axilRegBlock_bresp(m_axil_bresp),

      .axilRegBlock_bready(m_axil_bready),

      .axilRegBlock_arvalid(m_axil_arvalid),
      .axilRegBlock_araddr(m_axil_araddr),
      .axilRegBlock_arprot(m_axil_arprot),

      .axilRegBlock_arready(m_axil_arready),

      .axilRegBlock_rvalid(m_axil_rvalid),

      .axilRegBlock_rresp(m_axil_rresp),

      .axilRegBlock_rdata(m_axil_rdata),

      .axilRegBlock_rready(m_axil_rready)
  );


  cmac_usplus_0 cmac_inst(
        .gt_rxp_in                            (gt_rxp_in     ),
        .gt_rxn_in                            (gt_rxn_in     ),
        .gt_txp_out                           (gt_txp_out    ),
        .gt_txn_out                           (gt_txn_out    ),
        .gt_loopback_in                       (gt_loopback_in),
        
        .gtwiz_reset_tx_datapath              (gtwiz_reset_tx_datapath),
        .gtwiz_reset_rx_datapath              (gtwiz_reset_rx_datapath),
        .sys_reset                            (gt_sys_reset),
        .gt_ref_clk_p                         (gt_ref_clk_p),
        .gt_ref_clk_n                         (gt_ref_clk_n),
        .init_clk                             (gt_init_clk),

        .gt_txusrclk2                         (gt_txusrclk2),
        .usr_rx_reset                         (gt_usr_rx_reset),
        .usr_tx_reset                         (gt_usr_tx_reset),

        // RX
        .rx_axis_tvalid                       (gt_rx_axis_tvalid),
        .rx_axis_tdata                        (gt_rx_axis_tdata ),
        .rx_axis_tkeep                        (gt_rx_axis_tkeep ),
        .rx_axis_tlast                        (gt_rx_axis_tlast ),
        .rx_axis_tuser                        (gt_rx_axis_tuser ),
        
        .stat_rx_bad_fcs                      (gt_stat_rx_bad_fcs),
        .stat_rx_stomped_fcs                  (gt_stat_rx_stomped_fcs),
        .stat_rx_aligned                      (gt_stat_rx_aligned),
        .stat_rx_pause_req                    (gt_stat_rx_pause_req),
        .ctl_rx_enable                        (gt_ctl_rx_enable),
        .ctl_rx_force_resync                  (gt_ctl_rx_force_resync),
        .ctl_rx_test_pattern                  (gt_ctl_rx_test_pattern),
        .ctl_rx_check_etype_gcp               (gt_ctl_rx_check_etype_gcp),
        .ctl_rx_check_etype_gpp               (gt_ctl_rx_check_etype_gpp),
        .ctl_rx_check_etype_pcp               (gt_ctl_rx_check_etype_pcp),
        .ctl_rx_check_etype_ppp               (gt_ctl_rx_check_etype_ppp),
        .ctl_rx_check_mcast_gcp               (gt_ctl_rx_check_mcast_gcp),
        .ctl_rx_check_mcast_gpp               (gt_ctl_rx_check_mcast_gpp),
        .ctl_rx_check_mcast_pcp               (gt_ctl_rx_check_mcast_pcp),
        .ctl_rx_check_mcast_ppp               (gt_ctl_rx_check_mcast_ppp),
        .ctl_rx_check_opcode_gcp              (gt_ctl_rx_check_opcode_gcp),
        .ctl_rx_check_opcode_gpp              (gt_ctl_rx_check_opcode_gpp),
        .ctl_rx_check_opcode_pcp              (gt_ctl_rx_check_opcode_pcp),
        .ctl_rx_check_opcode_ppp              (gt_ctl_rx_check_opcode_ppp),
        .ctl_rx_check_sa_gcp                  (gt_ctl_rx_check_sa_gcp),
        .ctl_rx_check_sa_gpp                  (gt_ctl_rx_check_sa_gpp),
        .ctl_rx_check_sa_pcp                  (gt_ctl_rx_check_sa_pcp),
        .ctl_rx_check_sa_ppp                  (gt_ctl_rx_check_sa_ppp),
        .ctl_rx_check_ucast_gcp               (gt_ctl_rx_check_ucast_gcp),
        .ctl_rx_check_ucast_gpp               (gt_ctl_rx_check_ucast_gpp),
        .ctl_rx_check_ucast_pcp               (gt_ctl_rx_check_ucast_pcp),
        .ctl_rx_check_ucast_ppp               (gt_ctl_rx_check_ucast_ppp),
        .ctl_rx_enable_gcp                    (gt_ctl_rx_enable_gcp),
        .ctl_rx_enable_gpp                    (gt_ctl_rx_enable_gpp),
        .ctl_rx_enable_pcp                    (gt_ctl_rx_enable_pcp),
        .ctl_rx_enable_ppp                    (gt_ctl_rx_enable_ppp),
        .ctl_rx_pause_ack                     (gt_ctl_rx_pause_ack),
        .ctl_rx_pause_enable                  (gt_ctl_rx_pause_enable),
    

        // TX
        .tx_axis_tready                       (gt_tx_axis_tready),
        .tx_axis_tvalid                       (gt_tx_axis_tvalid),
        .tx_axis_tdata                        (gt_tx_axis_tdata),
        .tx_axis_tkeep                        (gt_tx_axis_tkeep),
        .tx_axis_tlast                        (gt_tx_axis_tlast),
        .tx_axis_tuser                        (gt_tx_axis_tuser),
        
        .tx_ovfout                            (gt_tx_ovfout),
        .tx_unfout                            (gt_tx_unfout),
        .ctl_tx_enable                        (gt_ctl_tx_enable),
        .ctl_tx_test_pattern                  (gt_ctl_tx_test_pattern),
        .ctl_tx_send_idle                     (gt_ctl_tx_send_idle),
        .ctl_tx_send_rfi                      (gt_ctl_tx_send_rfi),
        .ctl_tx_send_lfi                      (gt_ctl_tx_send_lfi),
        .ctl_tx_pause_enable                  (gt_ctl_tx_pause_enable),
        .ctl_tx_pause_req                     (gt_ctl_tx_pause_req),
        .ctl_tx_pause_quanta0                 (gt_ctl_tx_pause_quanta0),
        .ctl_tx_pause_quanta1                 (gt_ctl_tx_pause_quanta1),
        .ctl_tx_pause_quanta2                 (gt_ctl_tx_pause_quanta2),
        .ctl_tx_pause_quanta3                 (gt_ctl_tx_pause_quanta3),
        .ctl_tx_pause_quanta4                 (gt_ctl_tx_pause_quanta4),
        .ctl_tx_pause_quanta5                 (gt_ctl_tx_pause_quanta5),
        .ctl_tx_pause_quanta6                 (gt_ctl_tx_pause_quanta6),
        .ctl_tx_pause_quanta7                 (gt_ctl_tx_pause_quanta7),
        .ctl_tx_pause_quanta8                 (gt_ctl_tx_pause_quanta8),

        .ctl_tx_pause_refresh_timer0          (16'd0),
        .ctl_tx_pause_refresh_timer1          (16'd0),
        .ctl_tx_pause_refresh_timer2          (16'd0),
        .ctl_tx_pause_refresh_timer3          (16'd0),
        .ctl_tx_pause_refresh_timer4          (16'd0),
        .ctl_tx_pause_refresh_timer5          (16'd0),
        .ctl_tx_pause_refresh_timer6          (16'd0),
        .ctl_tx_pause_refresh_timer7          (16'd0),
        .ctl_tx_pause_refresh_timer8          (16'd0),
        .ctl_tx_resend_pause                  (1'b0 ),
        .tx_preamblein                        (56'd0),
        .core_rx_reset                        (1'b0 ),
        .core_tx_reset                        (1'b0 ),
        .rx_clk                               (gt_txusrclk2),
        .core_drp_reset                       (1'b0 ),
        .drp_clk                              (1'b0 ),
        .drp_addr                             (10'b0),
        .drp_di                               (16'b0),
        .drp_en                               (1'b0 ),
        .drp_do                               (),
        .drp_rdy                              (),
        .drp_we                               (1'b0 )
    );
endmodule
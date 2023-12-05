`timescale 1ps / 1ps

module top #
  (
   parameter PL_LINK_CAP_MAX_LINK_WIDTH          = 16,            // 1- X1; 2 - X2; 4 - X4; 8 - X8
   parameter PL_SIM_FAST_LINK_TRAINING           = "FALSE",      // Simulation Speedup
   parameter PL_LINK_CAP_MAX_LINK_SPEED          = 4,             // 1- GEN1; 2 - GEN2; 4 - GEN3
   parameter C_DATA_WIDTH                        = 512 ,
   parameter EXT_PIPE_SIM                        = "FALSE",  // This Parameter has effect on selecting Enable External PIPE Interface in GUI.
   parameter C_ROOT_PORT                         = "FALSE",      // PCIe block is in root port mode
   parameter C_DEVICE_NUMBER                     = 0,            // Device number for Root Port configurations only
   parameter AXIS_CCIX_RX_TDATA_WIDTH     = 512, 
   parameter AXIS_CCIX_TX_TDATA_WIDTH     = 512,
   parameter AXIS_CCIX_RX_TUSER_WIDTH     = 46,
   parameter AXIS_CCIX_TX_TUSER_WIDTH     = 46
   )
   (
    output [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0] pci_exp_txp,
    output [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0] pci_exp_txn,
    input [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0]  pci_exp_rxp,
    input [(PL_LINK_CAP_MAX_LINK_WIDTH - 1) : 0]  pci_exp_rxn,



    input 					 sys_clk_p,
    input 					 sys_clk_n,
    input 					 sys_rst_n
   );

   //-----------------------------------------------------------------------------------------------------------------------

   
   // Local Parameters derived from user selection
   localparam integer 				   USER_CLK_FREQ         = ((PL_LINK_CAP_MAX_LINK_SPEED == 3'h4) ? 5 : 4);
   localparam TCQ = 1;
   localparam C_S_AXI_ID_WIDTH = 4; 
   localparam C_M_AXI_ID_WIDTH = 4; 
   localparam C_S_AXI_DATA_WIDTH = C_DATA_WIDTH;
   localparam C_M_AXI_DATA_WIDTH = C_DATA_WIDTH;
   localparam C_S_AXI_ADDR_WIDTH = 64;
   localparam C_M_AXI_ADDR_WIDTH = 64;

   localparam C_NUM_USR_IRQ	 = 1;

   
   wire 					   user_lnk_up;
   
   //----------------------------------------------------------------------------------------------------------------//
   //  AXI Interface                                                                                                 //
   //----------------------------------------------------------------------------------------------------------------//
   
   wire 					   user_clk_250;
   wire              user_clk_500;
   wire 					   user_resetn;
   
  // Wires for Avery HOT/WARM and COLD RESET
   wire 					   avy_sys_rst_n_c;
   wire 					   avy_cfg_hot_reset_out;
   reg 						   avy_sys_rst_n_g;
   reg 						   avy_cfg_hot_reset_out_g;
   assign avy_sys_rst_n_c = avy_sys_rst_n_g;
   assign avy_cfg_hot_reset_out = avy_cfg_hot_reset_out_g;
   initial begin 
      avy_sys_rst_n_g = 1;
      avy_cfg_hot_reset_out_g =0;
   end
   


  //----------------------------------------------------------------------------------------------------------------//
  //    System(SYS) Interface                                                                                       //
  //----------------------------------------------------------------------------------------------------------------//

    wire                                    sys_clk;
    wire                                    sys_clk_gt;
    wire                                    sys_rst_n_c;


    reg [C_NUM_USR_IRQ-1:0] 		     usr_irq_req = 0;
    wire [C_NUM_USR_IRQ-1:0] 		     usr_irq_ack;


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



  // Core Top Level Wrapper
  xdma_0 xdma_0_i
     (
      //---------------------------------------------------------------------------------------//
      //  PCI Express (pci_exp) Interface                                                      //
      //---------------------------------------------------------------------------------------//
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


      .usr_irq_req  (usr_irq_req),
      .usr_irq_ack  (usr_irq_ack),

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

endmodule
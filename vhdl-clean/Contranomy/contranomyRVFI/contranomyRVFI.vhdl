-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomyrvfi_types.all;

entity contranomyRVFI is
  port(-- clock
       clk                   : in contranomyrvfi_types.clk_core;
       -- reset
       reset                 : in contranomyrvfi_types.rst_core;
       iBusWishbone_DAT_MISO : in std_logic_vector(31 downto 0);
       iBusWishbone_ACK      : in boolean;
       iBusWishbone_ERR      : in boolean;
       dBusWishbone_DAT_MISO : in std_logic_vector(31 downto 0);
       dBusWishbone_ACK      : in boolean;
       dBusWishbone_ERR      : in boolean;
       timerInterrupt        : in boolean;
       softwareInterrupt     : in boolean;
       externalInterrupt     : in std_logic_vector(31 downto 0);
       iBusWishbone_ADR      : out std_logic_vector(29 downto 0);
       iBusWishbone_DAT_MOSI : out std_logic_vector(31 downto 0);
       iBusWishbone_SEL      : out std_logic_vector(3 downto 0);
       iBusWishbone_CYC      : out boolean;
       iBusWishbone_STB      : out boolean;
       iBusWishbone_WE       : out boolean;
       iBusWishbone_CTI      : out std_logic_vector(2 downto 0);
       iBusWishbone_BTE      : out contranomyrvfi_types.bursttypeextension;
       dBusWishbone_ADR      : out std_logic_vector(29 downto 0);
       dBusWishbone_DAT_MOSI : out std_logic_vector(31 downto 0);
       dBusWishbone_SEL      : out std_logic_vector(3 downto 0);
       dBusWishbone_CYC      : out boolean;
       dBusWishbone_STB      : out boolean;
       dBusWishbone_WE       : out boolean;
       dBusWishbone_CTI      : out std_logic_vector(2 downto 0);
       dBusWishbone_BTE      : out contranomyrvfi_types.bursttypeextension;
       rvfi_valid            : out boolean;
       rvfi_order            : out unsigned(63 downto 0);
       rvfi_insn             : out std_logic_vector(31 downto 0);
       rvfi_trap             : out boolean;
       rvfi_halt             : out boolean;
       rvfi_intr             : out boolean;
       rvfi_mode             : out std_logic_vector(1 downto 0);
       rvfi_ixl              : out std_logic_vector(1 downto 0);
       rvfi_rs1_addr         : out contranomyrvfi_types.register_r;
       rvfi_rs2_addr         : out contranomyrvfi_types.register_r;
       rvfi_rs1_rdata        : out std_logic_vector(31 downto 0);
       rvfi_rs2_rdata        : out std_logic_vector(31 downto 0);
       rvfi_rd_addr          : out contranomyrvfi_types.register_r;
       rvfi_rd_wdata         : out std_logic_vector(31 downto 0);
       rvfi_pc_rdata         : out std_logic_vector(31 downto 0);
       rvfi_pc_wdata         : out std_logic_vector(31 downto 0);
       rvfi_mem_addr         : out std_logic_vector(31 downto 0);
       rvfi_mem_rmask        : out std_logic_vector(3 downto 0);
       rvfi_mem_wmask        : out std_logic_vector(3 downto 0);
       rvfi_mem_rdata        : out std_logic_vector(31 downto 0);
       rvfi_mem_wdata        : out std_logic_vector(31 downto 0);
       rvfi_csr_misa_rmask   : out std_logic_vector(31 downto 0);
       rvfi_csr_misa_wmask   : out std_logic_vector(31 downto 0);
       rvfi_csr_misa_rdata   : out std_logic_vector(31 downto 0);
       rvfi_csr_misa_wdata   : out std_logic_vector(31 downto 0));
end;

architecture structural of contranomyRVFI is
  signal result           : contranomyrvfi_types.tup2_6;
  signal result_0         : contranomyrvfi_types.tup3;
  signal \coreIn\         : contranomyrvfi_types.corein;
  signal result_fun_arg   : contranomyrvfi_types.tup3_0;
  signal result_0_fun_arg : contranomyrvfi_types.tup2;
  signal clash_internal   : contranomyrvfi_types.tup2_8;
  signal clash_internal_0 : contranomyrvfi_types.coreout;
  signal ibuswishbone_0   : contranomyrvfi_types.wishbonem2s;
  signal dbuswishbone_0   : contranomyrvfi_types.wishbonem2s;
  signal clash_internal_1 : contranomyrvfi_types.rvfi;
  signal rvfi_csr_misa    : contranomyrvfi_types.rvficsr;

begin
  \coreIn\ <= ( corein_sel0_ibuss2m => ( wishbones2m_sel0_readdata => iBusWishbone_DAT_MISO
              , wishbones2m_sel1_acknowledge => iBusWishbone_ACK
              , wishbones2m_sel2_err => iBusWishbone_ERR )
              , corein_sel1_dbuss2m => ( wishbones2m_sel0_readdata => dBusWishbone_DAT_MISO
              , wishbones2m_sel1_acknowledge => dBusWishbone_ACK
              , wishbones2m_sel2_err => dBusWishbone_ERR )
              , corein_sel2_timerinterrupt => timerInterrupt
              , corein_sel3_softwareinterrupt => softwareInterrupt
              , corein_sel4_externalinterrupt => externalInterrupt );

  result_fun_arg <= result_0.tup3_sel1_tup3_0;

  registerfile_0_result : entity registerfile_0
    port map
      ( result           => result
      , \c$ds_bindCsr\   => clk
      , \c$ds_bindCsr_0\ => reset
      , ds               => result_fun_arg );

  result_0_fun_arg <= ( tup2_sel0_corein => \coreIn\
                      , tup2_sel1_tup2_6 => result );

  core_0_result_0 : entity core_0
    port map
      ( result           => result_0
      , \c$ds_bindCsr\   => clk
      , \c$ds_bindCsr_0\ => reset
      , eta              => result_0_fun_arg );

  -- src/Contranomy.hs:(39,1)-(42,26)
  clash_internal <= ( tup2_8_sel0_coreout => result_0.tup3_sel0_coreout
                    , tup2_8_sel1_rvfi => result_0.tup3_sel2_rvfi );

  clash_internal_0 <= clash_internal.tup2_8_sel0_coreout;

  clash_internal_1 <= clash_internal.tup2_8_sel1_rvfi;

  ibuswishbone_0 <= clash_internal_0.coreout_sel0_ibusm2s;

  dbuswishbone_0 <= clash_internal_0.coreout_sel1_dbusm2s;

  iBusWishbone_ADR <= ibuswishbone_0.wishbonem2s_sel0_addr;

  iBusWishbone_DAT_MOSI <= ibuswishbone_0.wishbonem2s_sel1_writedata;

  iBusWishbone_SEL <= ibuswishbone_0.wishbonem2s_sel2_busselect;

  iBusWishbone_CYC <= ibuswishbone_0.wishbonem2s_sel3_buscycle;

  iBusWishbone_STB <= ibuswishbone_0.wishbonem2s_sel4_strobe;

  iBusWishbone_WE <= ibuswishbone_0.wishbonem2s_sel5_writeenable;

  iBusWishbone_CTI <= ibuswishbone_0.wishbonem2s_sel6_cycletypeidentifier;

  iBusWishbone_BTE <= ibuswishbone_0.wishbonem2s_sel7_bursttypeextension;

  dBusWishbone_ADR <= dbuswishbone_0.wishbonem2s_sel0_addr;

  dBusWishbone_DAT_MOSI <= dbuswishbone_0.wishbonem2s_sel1_writedata;

  dBusWishbone_SEL <= dbuswishbone_0.wishbonem2s_sel2_busselect;

  dBusWishbone_CYC <= dbuswishbone_0.wishbonem2s_sel3_buscycle;

  dBusWishbone_STB <= dbuswishbone_0.wishbonem2s_sel4_strobe;

  dBusWishbone_WE <= dbuswishbone_0.wishbonem2s_sel5_writeenable;

  dBusWishbone_CTI <= dbuswishbone_0.wishbonem2s_sel6_cycletypeidentifier;

  dBusWishbone_BTE <= dbuswishbone_0.wishbonem2s_sel7_bursttypeextension;

  rvfi_valid <= clash_internal_1.rvfi_sel0_valid;

  rvfi_order <= clash_internal_1.rvfi_sel1_order;

  rvfi_insn <= clash_internal_1.rvfi_sel2_insn;

  rvfi_trap <= clash_internal_1.rvfi_sel3_trap;

  rvfi_halt <= clash_internal_1.rvfi_sel4_halt;

  rvfi_intr <= clash_internal_1.rvfi_sel5_intr;

  rvfi_mode <= clash_internal_1.rvfi_sel6_mode;

  rvfi_ixl <= clash_internal_1.rvfi_sel7_ixl;

  rvfi_rs1_addr <= clash_internal_1.rvfi_sel8_rs1addr;

  rvfi_rs2_addr <= clash_internal_1.rvfi_sel9_rs2addr;

  rvfi_rs1_rdata <= clash_internal_1.rvfi_sel10_rs1rdata;

  rvfi_rs2_rdata <= clash_internal_1.rvfi_sel11_rs2rdata;

  rvfi_rd_addr <= clash_internal_1.rvfi_sel12_rdaddr;

  rvfi_rd_wdata <= clash_internal_1.rvfi_sel13_rdwdata;

  rvfi_pc_rdata <= clash_internal_1.rvfi_sel14_pcrdata;

  rvfi_pc_wdata <= clash_internal_1.rvfi_sel15_pcwdata;

  rvfi_mem_addr <= clash_internal_1.rvfi_sel16_memaddr;

  rvfi_mem_rmask <= clash_internal_1.rvfi_sel17_memrmask;

  rvfi_mem_wmask <= clash_internal_1.rvfi_sel18_memwmask;

  rvfi_mem_rdata <= clash_internal_1.rvfi_sel19_memrdata;

  rvfi_mem_wdata <= clash_internal_1.rvfi_sel20_memwdata;

  rvfi_csr_misa <= clash_internal_1.rvfi_sel21_misacsr;

  rvfi_csr_misa_rmask <= rvfi_csr_misa.rvficsr_sel0_rmask;

  rvfi_csr_misa_wmask <= rvfi_csr_misa.rvficsr_sel1_wmask;

  rvfi_csr_misa_rdata <= rvfi_csr_misa.rvficsr_sel2_rdata;

  rvfi_csr_misa_wdata <= rvfi_csr_misa.rvficsr_sel3_wdata;


end;


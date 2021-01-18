-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;
use work.constants.all;

entity contranomy is
  port(-- clock
       clk                   : in contranomy_types.clk_core;
       -- reset
       reset                 : in contranomy_types.rst_core;
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
       iBusWishbone_BTE      : out contranomy_types.bursttypeextension;
       dBusWishbone_ADR      : out std_logic_vector(29 downto 0);
       dBusWishbone_DAT_MOSI : out std_logic_vector(31 downto 0);
       dBusWishbone_SEL      : out std_logic_vector(3 downto 0);
       dBusWishbone_CYC      : out boolean;
       dBusWishbone_STB      : out boolean;
       dBusWishbone_WE       : out boolean;
       dBusWishbone_CTI      : out std_logic_vector(2 downto 0);
       dBusWishbone_BTE      : out contranomy_types.bursttypeextension);
end;

architecture structural of contranomy is
  signal result           : contranomy_types.tup2_6;
  signal result_0         : contranomy_types.tup3;
  signal \coreIn\         : contranomy_types.corein;
  signal result_fun_arg   : contranomy_types.tup3_0;
  signal result_0_fun_arg : contranomy_types.tup2;
  signal clash_internal   : contranomy_types.coreout;
  signal ibuswishbone_0   : contranomy_types.wishbonem2s;
  signal dbuswishbone_0   : contranomy_types.wishbonem2s;

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

  registerfile_result : entity registerfile
    port map
      ( result           => result
      , \c$ds_bindCsr\   => clk
      , \c$ds_bindCsr_0\ => reset
      , ds               => result_fun_arg );

  result_0_fun_arg <= ( tup2_sel0_corein => \coreIn\
                      , tup2_sel1_tup2_6 => result );

  core_result_0 : entity core
    port map
      ( result           => result_0
      , \c$ds_bindCsr\   => clk
      , \c$ds_bindCsr_0\ => reset
      , eta              => result_0_fun_arg );

  -- src/Contranomy.hs:(25,1)-(28,16)
  clash_internal <= result_0.tup3_sel0_coreout;

  ibuswishbone_0 <= clash_internal.coreout_sel0_ibusm2s;

  dbuswishbone_0 <= clash_internal.coreout_sel1_dbusm2s;

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


end;


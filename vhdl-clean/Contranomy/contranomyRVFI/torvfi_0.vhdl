-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomyrvfi_types.all;

entity torvfi_0 is
  port(\loadStoreFinished\ : in boolean;
       \rvfiOrder\         : in unsigned(63 downto 0);
       instruction         : in std_logic_vector(31 downto 0);
       trap                : in boolean;
       \rs1Val\            : in std_logic_vector(31 downto 0);
       \rs2Val\            : in std_logic_vector(31 downto 0);
       \rdVal\             : in contranomyrvfi_types.maybe;
       pc                  : in std_logic_vector(29 downto 0);
       \pcN\               : in std_logic_vector(29 downto 0);
       \dBusM2S\           : in contranomyrvfi_types.wishbonem2s;
       \dBusS2M\           : in contranomyrvfi_types.wishbones2m;
       \csrVal\            : in contranomyrvfi_types.tup2_1;
       result              : out contranomyrvfi_types.rvfi);
end;

architecture structural of torvfi_0 is
  signal \c$case_alt\             : contranomyrvfi_types.rvficsr;
  signal \c$case_alt_0\           : contranomyrvfi_types.rvficsr;
  -- src/Contranomy/Instruction.hs:275:22-38
  signal old                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:275:22-38
  signal \newVal\                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Wishbone.hs:26:5-10
  signal \c$case_alt_1\           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/RVFI.hs:73:37-55
  signal \c$case_alt_2\           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Wishbone.hs:26:5-10
  signal \c$case_alt_3\           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/RVFI.hs:69:37-61
  signal \c$case_alt_4\           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Wishbone.hs:26:5-10
  signal \c$case_alt_5\           : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/RVFI.hs:65:37-55
  signal \c$case_alt_6\           : std_logic_vector(3 downto 0);
  -- src/Contranomy/Wishbone.hs:26:5-10
  signal \c$case_alt_7\           : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/RVFI.hs:61:37-61
  signal \c$case_alt_8\           : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/RVFI.hs:60:16-53
  signal \c$app_arg\              : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/RVFI.hs:57:16-32
  signal \c$app_arg_0\            : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/RVFI.hs:56:16-33
  signal \c$app_arg_1\            : contranomyrvfi_types.register_r;
  -- src/Contranomy/Core/RVFI.hs:56:25-27
  signal x                        : contranomyrvfi_types.tup2_7;
  -- src/Contranomy/Core/RVFI.hs:88:49-77
  signal ds                       : contranomyrvfi_types.decodedinstruction;
  signal \c$case_alt_selection\   : contranomyrvfi_types.maybe_0;
  signal \c$case_alt_0_selection\ : std_logic_vector(11 downto 0);
  signal \c$case_alt_1_selection\ : boolean;
  signal \c$case_alt_2_selection\ : boolean;
  signal \c$case_alt_3_selection\ : boolean;
  signal \c$case_alt_4_selection\ : boolean;
  signal \c$case_alt_5_selection\ : boolean;
  signal \c$case_alt_6_selection\ : boolean;
  signal \c$case_alt_7_selection\ : boolean;
  signal \c$case_alt_8_selection\ : boolean;

begin
  -- src/Contranomy/Core/RVFI.hs:(46,1)-(88,77)
  result <= ( rvfi_sel0_valid => \loadStoreFinished\
            , rvfi_sel1_order => \rvfiOrder\
            , rvfi_sel2_insn => instruction
            , rvfi_sel3_trap => trap
            , rvfi_sel4_halt => false
            , rvfi_sel5_intr => false
            , rvfi_sel6_mode => std_logic_vector'("11")
            , rvfi_sel7_ixl => std_logic_vector'("01")
            , rvfi_sel8_rs1addr => ds.decodedinstruction_sel2_rs1
            , rvfi_sel9_rs2addr => ds.decodedinstruction_sel3_rs2
            , rvfi_sel10_rs1rdata => \rs1Val\
            , rvfi_sel11_rs2rdata => \rs2Val\
            , rvfi_sel12_rdaddr => \c$app_arg_1\
            , rvfi_sel13_rdwdata => \c$app_arg_0\
            , rvfi_sel14_pcrdata => std_logic_vector'(std_logic_vector'(pc) & std_logic_vector'(std_logic_vector'("00")))
            , rvfi_sel15_pcwdata => std_logic_vector'(std_logic_vector'(\pcN\) & std_logic_vector'(std_logic_vector'("00")))
            , rvfi_sel16_memaddr => \c$app_arg\
            , rvfi_sel17_memrmask => \c$case_alt_7\
            , rvfi_sel18_memwmask => \c$case_alt_5\
            , rvfi_sel19_memrdata => \c$case_alt_3\
            , rvfi_sel20_memwdata => \c$case_alt_1\
            , rvfi_sel21_misacsr => \c$case_alt\ );

  \c$case_alt_selection\ <= \csrVal\.tup2_1_sel0_maybe_0;

  with (\c$case_alt_selection\(32 downto 32)) select
    \c$case_alt\ <= ( rvficsr_sel0_rmask => std_logic_vector'(x"00000004")
                    , rvficsr_sel1_wmask => std_logic_vector'(x"00000000")
                    , rvficsr_sel2_rdata => std_logic_vector'(x"00000000")
                    , rvficsr_sel3_wdata => std_logic_vector'(x"00000000") ) when "0",
                    \c$case_alt_0\ when others;

  \c$case_alt_0_selection\ <= ds.decodedinstruction_sel12_imm12i;

  with (\c$case_alt_0_selection\) select
    \c$case_alt_0\ <= ( rvficsr_sel0_rmask => std_logic_vector'(x"FFFFFFFF")
                      , rvficsr_sel1_wmask => std_logic_vector'(x"FFFFFFFF")
                      , rvficsr_sel2_rdata => old
                      , rvficsr_sel3_wdata => \newVal\ ) when x"301",
                      ( rvficsr_sel0_rmask => std_logic_vector'(x"00000004")
                      , rvficsr_sel1_wmask => std_logic_vector'(x"00000000")
                      , rvficsr_sel2_rdata => std_logic_vector'(x"00000000")
                      , rvficsr_sel3_wdata => std_logic_vector'(x"00000000") ) when others;

  old <= \csrVal\.tup2_1_sel0_maybe_0(31 downto 0);

  \newVal\ <= \csrVal\.tup2_1_sel1_std_logic_vector;

  \c$case_alt_1_selection\ <= \dBusM2S\.wishbonem2s_sel4_strobe;

  -- src/Contranomy/Wishbone.hs:26:5-10
  \c$case_alt_1\ <= \c$case_alt_2\ when \c$case_alt_1_selection\ else
                    std_logic_vector'(x"00000000");

  \c$case_alt_2_selection\ <= \dBusM2S\.wishbonem2s_sel5_writeenable;

  -- src/Contranomy/Core/RVFI.hs:73:37-55
  -- src/Contranomy/Wishbone.hs:28:5-15
  \c$case_alt_2\ <= \dBusM2S\.wishbonem2s_sel1_writedata when \c$case_alt_2_selection\ else
                    std_logic_vector'(x"00000000");

  \c$case_alt_3_selection\ <= \dBusM2S\.wishbonem2s_sel4_strobe;

  -- src/Contranomy/Wishbone.hs:26:5-10
  \c$case_alt_3\ <= \c$case_alt_4\ when \c$case_alt_3_selection\ else
                    std_logic_vector'(x"00000000");

  \c$case_alt_4_selection\ <= \dBusM2S\.wishbonem2s_sel5_writeenable;

  -- src/Contranomy/Core/RVFI.hs:69:37-61
  -- src/Contranomy/Wishbone.hs:28:5-15
  \c$case_alt_4\ <= std_logic_vector'(x"00000000") when \c$case_alt_4_selection\ else
                    \dBusS2M\.wishbones2m_sel0_readdata;

  \c$case_alt_5_selection\ <= \dBusM2S\.wishbonem2s_sel4_strobe;

  -- src/Contranomy/Wishbone.hs:26:5-10
  \c$case_alt_5\ <= \c$case_alt_6\ when \c$case_alt_5_selection\ else
                    std_logic_vector'(x"0");

  \c$case_alt_6_selection\ <= \dBusM2S\.wishbonem2s_sel5_writeenable;

  -- src/Contranomy/Core/RVFI.hs:65:37-55
  -- src/Contranomy/Wishbone.hs:28:5-15
  \c$case_alt_6\ <= \dBusM2S\.wishbonem2s_sel2_busselect when \c$case_alt_6_selection\ else
                    std_logic_vector'(x"0");

  \c$case_alt_7_selection\ <= \dBusM2S\.wishbonem2s_sel4_strobe;

  -- src/Contranomy/Wishbone.hs:26:5-10
  \c$case_alt_7\ <= \c$case_alt_8\ when \c$case_alt_7_selection\ else
                    std_logic_vector'(x"0");

  \c$case_alt_8_selection\ <= \dBusM2S\.wishbonem2s_sel5_writeenable;

  -- src/Contranomy/Core/RVFI.hs:61:37-61
  -- src/Contranomy/Wishbone.hs:28:5-15
  \c$case_alt_8\ <= std_logic_vector'(x"0") when \c$case_alt_8_selection\ else
                    \dBusM2S\.wishbonem2s_sel2_busselect;

  -- src/Contranomy/Core/RVFI.hs:60:16-53
  \c$app_arg\ <= std_logic_vector'(x"00000000") when trap else
                 std_logic_vector'(std_logic_vector'(\dBusM2S\.wishbonem2s_sel0_addr) & std_logic_vector'(std_logic_vector'("00")));

  -- src/Contranomy/Core/RVFI.hs:57:16-32
  with (\rdVal\(37 downto 37)) select
    \c$app_arg_0\ <= std_logic_vector'(x"00000000") when "0",
                     x.tup2_7_sel1_std_logic_vector when others;

  -- src/Contranomy/Core/RVFI.hs:56:16-33
  with (\rdVal\(37 downto 37)) select
    \c$app_arg_1\ <= std_logic_vector(to_unsigned(0,5)) when "0",
                     x.tup2_7_sel0_register_r when others;

  x <= contranomyrvfi_types.tup2_7'(contranomyrvfi_types.fromSLV(\rdVal\(36 downto 0)));

  -- src/Contranomy/Core/RVFI.hs:88:49-77
  decodeinstruction_0_ds : entity decodeinstruction_0
    port map
      (result => ds, w      => instruction);


end;


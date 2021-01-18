-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;

entity core is
  port(-- clock
       \c$ds_bindCsr\   : in contranomy_types.clk_core;
       -- reset
       \c$ds_bindCsr_0\ : in contranomy_types.rst_core;
       eta              : in contranomy_types.tup2;
       result           : out contranomy_types.tup3);
end;

architecture structural of core is
  signal result_0                                  : contranomy_types.tup2_0;
  signal result_1                                  : contranomy_types.corestate;
  signal \b'1\                                     : contranomy_types.clash_internal;
  signal \c$b'1_0\                                 : contranomy_types.clash_internal_0;
  signal b2                                        : unsigned(63 downto 0);
  signal b1                                        : unsigned(63 downto 0);
  signal a1                                        : contranomy_types.machinestate;
  signal b1_0                                      : contranomy_types.clash_internal_0;
  signal a1_0                                      : std_logic_vector(31 downto 0);
  signal b1_1                                      : contranomy_types.clash_internal;
  signal a1_1                                      : contranomy_types.clash_internal_1;
  -- src/Contranomy/Core.hs:168:12-15
  signal g3                                        : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:168:12-15
  signal g5                                        : unsigned(63 downto 0);
  -- src/Contranomy/Core.hs:168:12-15
  signal g4                                        : contranomy_types.machinestate;
  -- src/Contranomy/Core.hs:168:12-15
  signal g1                                        : contranomy_types.corestage;
  signal \c$b'1_1\                                 : contranomy_types.clash_internal;
  signal \c$b'1_2\                                 : contranomy_types.clash_internal_0;
  -- src/Contranomy/Core.hs:(175,3)-(180,10)
  signal a3                                        : contranomy_types.machinestate;
  signal \c$app_arg\                               : contranomy_types.rvfi;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal \pcN1\                                    : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core.hs:(162,7)-(165,28)
  signal \registerWrite\                           : contranomy_types.maybe;
  -- src/Contranomy/Core.hs:152:70-86
  signal \dBusM2S1\                                : contranomy_types.wishbonem2s;
  -- src/Contranomy/Core.hs:163:20-27
  signal result_2                                  : contranomy_types.maybe;
  -- src/Contranomy/Core.hs:165:14-28
  signal \c$registerWrite_case_alt\                : contranomy_types.maybe;
  -- src/Contranomy/Instruction.hs:187:20-35
  signal \c$registerWrite_case_alt_0\              : contranomy_types.maybe;
  -- src/Contranomy/Instruction.hs:184:17-32
  signal \c$registerWrite_case_alt_1\              : contranomy_types.maybe;
  -- src/Contranomy/Instruction.hs:188:18-33
  signal \c$registerWrite_case_alt_2\              : contranomy_types.maybe;
  -- src/Contranomy/Core.hs:158:21-26
  signal \c$registerWrite_case_alt_3\              : contranomy_types.maybe;
  -- src/Contranomy/Instruction.hs:183:16-31
  signal \c$registerWrite_case_alt_4\              : contranomy_types.maybe;
  -- src/Contranomy/Core.hs:165:14-18
  signal a4                                        : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:(175,3)-(180,10)
  signal \csrOld\                                  : contranomy_types.maybe_0;
  -- src/Contranomy/Core.hs:(175,3)-(180,10)
  signal s1                                        : contranomy_types.tup2_1;
  signal \c$case_scrut\                            : contranomy_types.tup2_2;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal trap                                      : boolean;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal a1_2                                      : contranomy_types.tup2_3;
  -- src/Contranomy/Core.hs:(162,7)-(165,28)
  signal \c$registerWrite_case_alt_5\              : contranomy_types.maybe;
  -- src/Contranomy/Core.hs:165:14-18
  signal a4_0                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:165:14-18
  signal \ldVal\                                   : contranomy_types.maybe_0;
  -- src/Contranomy/Core.hs:(163,9)-(165,28)
  signal rd                                        : contranomy_types.register_r;
  signal b1_2                                      : unsigned(63 downto 0);
  signal a1_3                                      : contranomy_types.machinestate;
  signal b1_3                                      : contranomy_types.clash_internal_0;
  signal a1_4                                      : std_logic_vector(31 downto 0);
  signal b1_4                                      : contranomy_types.clash_internal;
  signal a1_5                                      : contranomy_types.clash_internal_1;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal g3_0                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal g5_0                                      : unsigned(63 downto 0);
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal g4_0                                      : contranomy_types.machinestate;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal g2                                        : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal g1_0                                      : contranomy_types.corestage;
  -- src/Contranomy/Core.hs:(151,24)-(152,86)
  signal \s'\                                      : contranomy_types.corestate;
  signal \c$case_scrut_0\                          : contranomy_types.tup2_4;
  -- src/Contranomy/Core.hs:(96,1)-(180,10)
  signal \loadStoreFinished\                       : boolean;
  -- src/Contranomy/Core.hs:134:9-76
  signal ds14                                      : contranomy_types.tup5;
  -- src/Contranomy/Core.hs:129:7-51
  signal \aluIResult\                              : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:141:44
  signal y                                         : std_logic_vector(1 downto 0);
  -- src/Contranomy/Core.hs:136:50-51
  signal \pcN\                                     : contranomy_types.tup2_5;
  -- src/Contranomy/Core.hs:136:13-51
  signal \rs2Val\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:136:13-51
  signal \rs1Val\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core.hs:134:35-50
  signal \c$ds14_app_arg\                          : boolean;
  -- src/Contranomy/Core.hs:136:13-51
  signal ipv                                       : boolean;
  -- src/Contranomy/Core.hs:134:9-76
  signal \c$ds14_case_alt\                         : boolean;
  -- src/Contranomy/Wishbone.hs:42:5-7
  signal \c$case_alt\                              : contranomy_types.corestage;
  -- src/Contranomy/Core.hs:(103,18)-(106,30)
  signal \c$case_alt_0\                            : contranomy_types.corestage;
  signal \c$decodeInstructionOut\                  : contranomy_types.decodedinstruction;
  signal \c$decodeInstructionOut_app_arg\          : std_logic_vector(31 downto 0);
  signal result_3                                  : unsigned(63 downto 0) := to_unsigned(0,64);
  -- src/Contranomy/Core/CoreState.hs:30:1-25
  signal x                                         : unsigned(63 downto 0);
  signal \c$app_arg_machineState_mstatus_mie\      : boolean := false;
  signal \c$app_arg_machineState_mstatus_mpie\     : boolean := false;
  -- src/Contranomy/Core/MachineState.hs:36:1-23
  signal x_0                                       : boolean;
  -- src/Contranomy/Core/MachineState.hs:36:1-23
  signal x_1                                       : boolean;
  signal result_4                                  : contranomy_types.mstatus;
  signal \c$app_arg_machineState_mcause_interrupt\ : boolean := false;
  signal \c$app_arg_machineState_mcause_code\      : std_logic_vector(3 downto 0) := std_logic_vector'(x"0");
  -- src/Contranomy/Instruction.hs:155:1-22
  signal x_2                                       : boolean;
  -- src/Contranomy/Instruction.hs:155:1-22
  signal x_3                                       : std_logic_vector(3 downto 0);
  signal result_5                                  : contranomy_types.mcause;
  signal \c$app_arg_machineState_mtvec\            : contranomy_types.interruptmode := std_logic_vector'(std_logic_vector(resize(unsigned(std_logic_vector'(std_logic_vector'("000000000000000000000000000000"))),30)) & "00");
  signal \c$app_arg_machineState_mie_meie\         : boolean := false;
  signal \c$app_arg_machineState_mie_mtie\         : boolean := false;
  signal \c$app_arg_machineState_mie_msie\         : boolean := false;
  -- src/Contranomy/Core/MachineState.hs:64:1-19
  signal x_4                                       : boolean;
  -- src/Contranomy/Core/MachineState.hs:64:1-19
  signal x_5                                       : boolean;
  -- src/Contranomy/Core/MachineState.hs:64:1-19
  signal x_6                                       : boolean;
  signal result_6                                  : contranomy_types.mie;
  signal \c$app_arg_machineState_mscratch\         : std_logic_vector(31 downto 0) := std_logic_vector'(x"00000000");
  signal \c$app_arg_machineState_mepc\             : std_logic_vector(29 downto 0) := std_logic_vector'("000000000000000000000000000000");
  signal \c$app_arg_machineState_mtval\            : std_logic_vector(31 downto 0) := std_logic_vector'(x"00000000");
  signal \c$app_arg_machineState_irqmask\          : std_logic_vector(31 downto 0) := std_logic_vector'(x"00000000");
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_7                                       : contranomy_types.mstatus;
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_8                                       : contranomy_types.mcause;
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_9                                       : contranomy_types.interruptmode;
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_10                                      : contranomy_types.mie;
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_11                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_12                                      : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_13                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/MachineState.hs:79:1-28
  signal x_14                                      : std_logic_vector(31 downto 0);
  signal result_7                                  : contranomy_types.machinestate;
  -- src/Contranomy/Core/CoreState.hs:30:1-25
  signal x_15                                      : contranomy_types.machinestate;
  signal result_8                                  : std_logic_vector(31 downto 0) := std_logic_vector'(x"00000000");
  -- src/Contranomy/Core/CoreState.hs:30:1-25
  signal x_16                                      : std_logic_vector(31 downto 0);
  signal result_9                                  : std_logic_vector(29 downto 0) := std_logic_vector'("000000000000000000000000000000");
  -- src/Contranomy/Core/CoreState.hs:30:1-25
  signal x_17                                      : std_logic_vector(29 downto 0);
  signal result_10                                 : contranomy_types.corestage := std_logic_vector'("0" & "-");
  -- src/Contranomy/Core/CoreState.hs:30:1-25
  signal x_18                                      : contranomy_types.corestage;
  signal \c$app_arg_fun_arg\                       : contranomy_types.wishbones2m;
  signal result_2_selection_res                    : boolean;
  signal \c$registerWrite_case_alt_selection\      : std_logic_vector(6 downto 0);
  signal \c$registerWrite_case_alt_0_selection\    : std_logic_vector(6 downto 0);
  signal \c$registerWrite_case_alt_1_selection\    : std_logic_vector(6 downto 0);
  signal \c$registerWrite_case_alt_2_selection\    : std_logic_vector(6 downto 0);
  signal \c$registerWrite_case_alt_4_selection\    : std_logic_vector(6 downto 0);
  signal \c$case_scrut_fun_arg\                    : boolean;
  signal \c$case_scrut_fun_arg_0\                  : boolean;
  signal \c$case_scrut_fun_arg_1\                  : std_logic_vector(31 downto 0);
  signal \c$case_scrut_0_fun_arg\                  : contranomy_types.corestate;
  signal \c$case_scrut_0_fun_arg_0\                : contranomy_types.exception_in;
  signal \c$case_scrut_0_fun_arg_1\                : contranomy_types.corestate;
  signal ds14_fun_arg                              : contranomy_types.wishbones2m;
  signal \c$ds14_case_alt_selection\               : boolean;
  signal \c$case_alt_selection\                    : boolean;
  signal \c$case_alt_0_selection\                  : boolean;

begin
  -- src/Contranomy/Core.hs:(67,1)-(88,5)
  -- src/Contranomy/Clash/Extra.hs:(27,1)-(30,22)
  -- src/Contranomy/Clash/Extra.hs:30:22
  result <= result_0.tup2_0_sel0_tup3;

  with (result_10(1 downto 1)) select
    result_0 <= ( tup2_0_sel0_tup3 => ( tup3_sel0_coreout => ( coreout_sel0_ibusm2s => ( wishbonem2s_sel0_addr => result_9
                , wishbonem2s_sel1_writedata => std_logic_vector'(0 to 31 => '-')
                , wishbonem2s_sel2_busselect => std_logic_vector'(x"F")
                , wishbonem2s_sel3_buscycle => true
                , wishbonem2s_sel4_strobe => true
                , wishbonem2s_sel5_writeenable => false
                , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
                , wishbonem2s_sel7_bursttypeextension => "00" )
                , coreout_sel1_dbusm2s => ( wishbonem2s_sel0_addr => std_logic_vector'(0 to 29 => '-')
                , wishbonem2s_sel1_writedata => std_logic_vector'(0 to 31 => '-')
                , wishbonem2s_sel2_busselect => std_logic_vector'(0 to 3 => '-')
                , wishbonem2s_sel3_buscycle => false
                , wishbonem2s_sel4_strobe => false
                , wishbonem2s_sel5_writeenable => false
                , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
                , wishbonem2s_sel7_bursttypeextension => "00" ) )
                , tup3_sel1_tup3_0 => ( tup3_0_sel0_maybe_1_0 => std_logic_vector'("1" & (std_logic_vector(\c$decodeInstructionOut\.decodedinstruction_sel2_rs1)))
                , tup3_0_sel1_maybe_1_1 => std_logic_vector'("1" & (std_logic_vector(\c$decodeInstructionOut\.decodedinstruction_sel3_rs2)))
                , tup3_0_sel2_maybe => std_logic_vector'("0" & "-------------------------------------") )
                , tup3_sel2_rvfi => ( rvfi_sel0_valid => false
                , rvfi_sel1_order => to_unsigned(0,64)
                , rvfi_sel2_insn => std_logic_vector'(x"00000000")
                , rvfi_sel3_trap => false
                , rvfi_sel4_halt => false
                , rvfi_sel5_intr => false
                , rvfi_sel6_mode => std_logic_vector'("11")
                , rvfi_sel7_ixl => std_logic_vector'("01")
                , rvfi_sel8_rs1addr => std_logic_vector(to_unsigned(0,5))
                , rvfi_sel9_rs2addr => std_logic_vector(to_unsigned(0,5))
                , rvfi_sel10_rs1rdata => std_logic_vector'(x"00000000")
                , rvfi_sel11_rs2rdata => std_logic_vector'(x"00000000")
                , rvfi_sel12_rdaddr => std_logic_vector(to_unsigned(0,5))
                , rvfi_sel13_rdwdata => std_logic_vector'(x"00000000")
                , rvfi_sel14_pcrdata => std_logic_vector'(x"00000000")
                , rvfi_sel15_pcwdata => std_logic_vector'(x"00000000")
                , rvfi_sel16_memaddr => std_logic_vector'(x"00000000")
                , rvfi_sel17_memrmask => std_logic_vector'(x"0")
                , rvfi_sel18_memwmask => std_logic_vector'(x"0")
                , rvfi_sel19_memrdata => std_logic_vector'(x"00000000")
                , rvfi_sel20_memwdata => std_logic_vector'(x"00000000")
                , rvfi_sel21_misacsr => ( rvficsr_sel0_rmask => std_logic_vector'(x"00000000")
                , rvficsr_sel1_wmask => std_logic_vector'(x"00000000")
                , rvficsr_sel2_rdata => std_logic_vector'(x"00000000")
                , rvficsr_sel3_wdata => std_logic_vector'(x"00000000") ) ) )
                , tup2_0_sel1_corestate => ( corestate_sel0_stage => \c$case_alt\
                , corestate_sel1_pc => result_9
                , corestate_sel2_instruction => eta.tup2_sel0_corein.corein_sel0_ibuss2m.wishbones2m_sel0_readdata
                , corestate_sel3_machinestate => result_7
                , corestate_sel4_rvfiorder => result_3 ) ) when "0",
                ( tup2_0_sel0_tup3 => ( tup3_sel0_coreout => ( coreout_sel0_ibusm2s => ( wishbonem2s_sel0_addr => std_logic_vector'(0 to 29 => '-')
                , wishbonem2s_sel1_writedata => std_logic_vector'(0 to 31 => '-')
                , wishbonem2s_sel2_busselect => std_logic_vector'(0 to 3 => '-')
                , wishbonem2s_sel3_buscycle => false
                , wishbonem2s_sel4_strobe => false
                , wishbonem2s_sel5_writeenable => false
                , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
                , wishbonem2s_sel7_bursttypeextension => "00" )
                , coreout_sel1_dbusm2s => \dBusM2S1\ )
                , tup3_sel1_tup3_0 => ( tup3_0_sel0_maybe_1_0 => std_logic_vector'("0" & "-----")
                , tup3_0_sel1_maybe_1_1 => std_logic_vector'("0" & "-----")
                , tup3_0_sel2_maybe => \registerWrite\ )
                , tup3_sel2_rvfi => \c$app_arg\ )
                , tup2_0_sel1_corestate => result_1 ) when others;

  result_1 <= ( corestate_sel0_stage => std_logic_vector'("0" & "-")
              , corestate_sel1_pc => a1_1.clash_internal_1_sel1_std_logic_vector
              , corestate_sel2_instruction => \b'1\.clash_internal_sel0_std_logic_vector
              , corestate_sel3_machinestate => \b'1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel0_machinestate
              , corestate_sel4_rvfiorder => \b'1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel1_unsigned ) when \loadStoreFinished\ else
              ( corestate_sel0_stage => a1_5.clash_internal_1_sel0_corestage
              , corestate_sel1_pc => a1_5.clash_internal_1_sel1_std_logic_vector
              , corestate_sel2_instruction => \c$b'1_1\.clash_internal_sel0_std_logic_vector
              , corestate_sel3_machinestate => \c$b'1_1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel0_machinestate
              , corestate_sel4_rvfiorder => \c$b'1_1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel1_unsigned );

  \b'1\ <= ( clash_internal_sel0_std_logic_vector => a1_0
           , clash_internal_sel1_clash_internal_0 => \c$b'1_0\ );

  \c$b'1_0\ <= ( clash_internal_0_sel0_machinestate => a1
               , clash_internal_0_sel1_unsigned => b2 );

  -- src/Contranomy/Core.hs:169:5-14
  b2 <= b1 + to_unsigned(1,64);

  b1 <= b1_0.clash_internal_0_sel1_unsigned;

  a1 <= b1_0.clash_internal_0_sel0_machinestate;

  b1_0 <= b1_1.clash_internal_sel1_clash_internal_0;

  a1_0 <= b1_1.clash_internal_sel0_std_logic_vector;

  -- src/Contranomy/Core.hs:168:5-15
  b1_1 <= ( clash_internal_sel0_std_logic_vector => g3
          , clash_internal_sel1_clash_internal_0 => ( clash_internal_0_sel0_machinestate => g4
          , clash_internal_0_sel1_unsigned => g5 ) );

  -- src/Contranomy/Core.hs:168:5-15
  a1_1 <= ( clash_internal_1_sel0_corestage => g1
          , clash_internal_1_sel1_std_logic_vector => \pcN1\ );

  g3 <= \c$b'1_1\.clash_internal_sel0_std_logic_vector;

  g5 <= \c$b'1_1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel1_unsigned;

  g4 <= \c$b'1_1\.clash_internal_sel1_clash_internal_0.clash_internal_0_sel0_machinestate;

  g1 <= a1_5.clash_internal_1_sel0_corestage;

  \c$b'1_1\ <= ( clash_internal_sel0_std_logic_vector => a1_4
               , clash_internal_sel1_clash_internal_0 => \c$b'1_2\ );

  \c$b'1_2\ <= ( clash_internal_0_sel0_machinestate => a3
               , clash_internal_0_sel1_unsigned => b1_2 );

  a3 <= \c$case_scrut\.tup2_2_sel1_machinestate;

  \c$app_arg_fun_arg\ <= eta.tup2_sel0_corein.corein_sel1_dbuss2m;

  torvfi_capp_arg : entity torvfi
    port map
      ( result              => \c$app_arg\
      , \loadStoreFinished\ => \loadStoreFinished\
      , \rvfiOrder\         => result_3
      , instruction         => result_8
      , trap                => trap
      , \rs1Val\            => \rs1Val\
      , \rs2Val\            => \rs2Val\
      , \rdVal\             => \registerWrite\
      , pc                  => result_9
      , \pcN\               => \pcN1\
      , \dBusM2S\           => \dBusM2S1\
      , \dBusS2M\           => \c$app_arg_fun_arg\
      , \csrVal\            => s1 );

  \pcN1\ <= a1_2.tup2_3_sel1_std_logic_vector;

  -- src/Contranomy/Core.hs:(162,7)-(165,28)
  -- src/Contranomy/Core.hs:(163,9)-(165,28)
  \registerWrite\ <= std_logic_vector'("0" & "-------------------------------------") when trap else
                     result_2;

  \dBusM2S1\ <= ds14.tup5_sel0_wishbonem2s;

  result_2_selection_res <= (rd) = (std_logic_vector(to_unsigned(0,5)));

  -- src/Contranomy/Core.hs:163:20-27
  -- src/Contranomy/Instruction.hs:128:3-23
  -- src/Contranomy/Instruction.hs:123:1-28
  result_2 <= std_logic_vector'("0" & "-------------------------------------") when result_2_selection_res else
              \c$registerWrite_case_alt\;

  \c$registerWrite_case_alt_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Core.hs:165:14-28
  -- src/Contranomy/Core.hs:(154,7)-(160,35)
  -- src/Contranomy/Core.hs:(154,15)-(160,35)
  -- src/Contranomy/Instruction.hs:182:18-33
  with (\c$registerWrite_case_alt_selection\) select
    \c$registerWrite_case_alt\ <= std_logic_vector'("0" & "-------------------------------------") when "1100011",
                                  \c$registerWrite_case_alt_0\ when others;

  \c$registerWrite_case_alt_0_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:187:20-35
  -- src/Contranomy/Core.hs:154:20-25
  with (\c$registerWrite_case_alt_0_selection\) select
    \c$registerWrite_case_alt_0\ <= std_logic_vector'("0" & "-------------------------------------") when "0001111",
                                    \c$registerWrite_case_alt_1\ when others;

  \c$registerWrite_case_alt_1_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:184:17-32
  -- src/Contranomy/Core.hs:154:20-25
  with (\c$registerWrite_case_alt_1_selection\) select
    \c$registerWrite_case_alt_1\ <= std_logic_vector'("0" & "-------------------------------------") when "0100011",
                                    \c$registerWrite_case_alt_2\ when others;

  \c$registerWrite_case_alt_2_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:188:18-33
  -- src/Contranomy/Core.hs:154:20-25
  with (\c$registerWrite_case_alt_2_selection\) select
    \c$registerWrite_case_alt_2\ <= \c$registerWrite_case_alt_3\ when "1110011",
                                    \c$registerWrite_case_alt_4\ when others;

  -- src/Contranomy/Core.hs:158:21-26
  with (\csrOld\(32 downto 32)) select
    \c$registerWrite_case_alt_3\ <= std_logic_vector'("0" & "-------------------------------------") when "0",
                                    std_logic_vector'("1" & ((std_logic_vector(rd)
                                     & a4))) when others;

  \c$registerWrite_case_alt_4_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:183:16-31
  -- src/Contranomy/Core.hs:154:20-25
  with (\c$registerWrite_case_alt_4_selection\) select
    \c$registerWrite_case_alt_4\ <= \c$registerWrite_case_alt_5\ when "0000011",
                                    std_logic_vector'("1" & ((std_logic_vector(rd)
                                     & \aluIResult\))) when others;

  a4 <= \csrOld\(31 downto 0);

  \csrOld\ <= s1.tup2_1_sel0_maybe_0;

  s1 <= \c$case_scrut\.tup2_2_sel0_tup2_1;

  \c$case_scrut_fun_arg\ <= eta.tup2_sel0_corein.corein_sel3_softwareinterrupt;

  \c$case_scrut_fun_arg_0\ <= eta.tup2_sel0_corein.corein_sel2_timerinterrupt;

  \c$case_scrut_fun_arg_1\ <= eta.tup2_sel0_corein.corein_sel4_externalinterrupt;

  csrunit_ccase_scrut : entity csrunit
    port map
      ( result              => \c$case_scrut\
      , trap                => trap
      , instruction         => result_8
      , \rs1Val\            => \rs1Val\
      , \softwareInterrupt\ => \c$case_scrut_fun_arg\
      , \timerInterrupt\    => \c$case_scrut_fun_arg_0\
      , \externalInterrupt\ => \c$case_scrut_fun_arg_1\
      , \c$arg\             => a1_3 );

  trap <= a1_2.tup2_3_sel0_boolean;

  a1_2 <= \c$case_scrut_0\.tup2_4_sel0_tup2_3;

  with (\ldVal\(32 downto 32)) select
    \c$registerWrite_case_alt_5\ <= std_logic_vector'("0" & "-------------------------------------") when "0",
                                    std_logic_vector'("1" & ((std_logic_vector(rd)
                                     & a4_0))) when others;

  a4_0 <= \ldVal\(31 downto 0);

  \ldVal\ <= ds14.tup5_sel1_maybe_0_0;

  rd <= \c$decodeInstructionOut\.decodedinstruction_sel1_rd;

  b1_2 <= b1_3.clash_internal_0_sel1_unsigned;

  a1_3 <= b1_3.clash_internal_0_sel0_machinestate;

  b1_3 <= b1_4.clash_internal_sel1_clash_internal_0;

  a1_4 <= b1_4.clash_internal_sel0_std_logic_vector;

  b1_4 <= ( clash_internal_sel0_std_logic_vector => g3_0
          , clash_internal_sel1_clash_internal_0 => ( clash_internal_0_sel0_machinestate => g4_0
          , clash_internal_0_sel1_unsigned => g5_0 ) );

  a1_5 <= ( clash_internal_1_sel0_corestage => g1_0
          , clash_internal_1_sel1_std_logic_vector => g2 );

  g3_0 <= \s'\.corestate_sel2_instruction;

  g5_0 <= \s'\.corestate_sel4_rvfiorder;

  g4_0 <= \s'\.corestate_sel3_machinestate;

  g2 <= \s'\.corestate_sel1_pc;

  g1_0 <= \s'\.corestate_sel0_stage;

  \s'\ <= \c$case_scrut_0\.tup2_4_sel1_corestate;

  \c$case_scrut_0_fun_arg\ <= ( corestate_sel0_stage => result_10
                              , corestate_sel1_pc => result_9
                              , corestate_sel2_instruction => result_8
                              , corestate_sel3_machinestate => result_7
                              , corestate_sel4_rvfiorder => result_3 );

  \c$case_scrut_0_fun_arg_0\ <= ( ei_instraccessfault => ipv
                                , ei_instr_addr_misaligned => y /= std_logic_vector'("00")
                                , ei_instr_illegal => \c$ds14_case_alt\
                                , ei_data_access_fault => ds14.tup5_sel2_maybe_0_1
                                , ei_data_addr_misaligned => ds14.tup5_sel3_maybe_0_2
                                , ei_timer_interrupt => eta.tup2_sel0_corein.corein_sel2_timerinterrupt
                                , ei_software_interrupt => eta.tup2_sel0_corein.corein_sel3_softwareinterrupt
                                , ei_external_interrupt => eta.tup2_sel0_corein.corein_sel4_externalinterrupt );

  \c$case_scrut_0_fun_arg_1\ <= ( corestate_sel0_stage => result_10
                                , corestate_sel1_pc => result_9
                                , corestate_sel2_instruction => result_8
                                , corestate_sel3_machinestate => result_7
                                , corestate_sel4_rvfiorder => result_3 );

  handleexceptions_ccase_scrut_0 : entity handleexceptions
    port map
      ( result        => \c$case_scrut_0\
      , ds            => \c$case_scrut_0_fun_arg\
      , \exceptionIn\ => \c$case_scrut_0_fun_arg_0\
      , \lsFinished\  => \loadStoreFinished\
      , ds1           => \pcN\
      , \c$arg\       => \c$case_scrut_0_fun_arg_1\ );

  \loadStoreFinished\ <= ds14.tup5_sel4_boolean;

  ds14_fun_arg <= eta.tup2_sel0_corein.corein_sel1_dbuss2m;

  loadstoreunit_ds14 : entity loadstoreunit
    port map
      ( result             => ds14
      , instruction        => result_8
      , \instructionFault\ => \c$ds14_app_arg\
      , addr               => \aluIResult\
      , \toStore\          => \rs2Val\
      , \dBusS2M\          => ds14_fun_arg );

  alu_aluiresult : entity alu
    port map
      ( result      => \aluIResult\
      , instruction => result_8
      , pc          => result_9
      , \rs1Value\  => \rs1Val\
      , \rs2Value\  => \rs2Val\ );

  y <= \pcN\.tup2_5_sel1_std_logic_vector_1;

  branchunit_pcn : entity branchunit
    port map
      ( result      => \pcN\
      , instruction => result_8
      , \rs1Val\    => \rs1Val\
      , \rs2Val\    => \rs2Val\
      , pc          => result_9 );

  \rs2Val\ <= eta.tup2_sel1_tup2_6.tup2_6_sel1_std_logic_vector_1;

  \rs1Val\ <= eta.tup2_sel1_tup2_6.tup2_6_sel0_std_logic_vector_0;

  -- src/Contranomy/Core.hs:134:35-50
  -- src/Contranomy/Core.hs:131:7-49
  -- src/Contranomy/Core.hs:131:26-49
  \c$ds14_app_arg\ <= true when result_10(0) = '1' else
                      \c$ds14_case_alt\;

  \c$ds14_case_alt_selection\ <= \c$decodeInstructionOut\.decodedinstruction_sel16_legal;

  \c$ds14_case_alt\ <= false when \c$ds14_case_alt_selection\ else
                       true;

  \c$case_alt_selection\ <= eta.tup2_sel0_corein.corein_sel0_ibuss2m.wishbones2m_sel2_err;

  -- src/Contranomy/Wishbone.hs:42:5-7
  \c$case_alt\ <= EXECUTE_FAULT when \c$case_alt_selection\ else \c$case_alt_0\;

  \c$case_alt_0_selection\ <= eta.tup2_sel0_corein.corein_sel0_ibuss2m.wishbones2m_sel1_acknowledge;

  -- src/Contranomy/Core.hs:(103,18)-(106,30)
  -- src/Contranomy/Wishbone.hs:40:5-15
  \c$case_alt_0\ <= EXECUTE_NO_FAULT when \c$case_alt_0_selection\ else INSTRUCTION_FETCH;

  decodeinstruction_cdecodeinstructionout : entity decodeinstruction
    port map
      ( result => \c$decodeInstructionOut\
      , w      => \c$decodeInstructionOut_app_arg\ );

  with (result_10(1 downto 1)) select
    \c$decodeInstructionOut_app_arg\ <= eta.tup2_sel0_corein.corein_sel0_ibuss2m.wishbones2m_sel0_readdata when "0",
                                        result_8 when others;

  -- src/Contranomy/Core/CoreState.hs:30:1-25
  -- register begin 
  result_3_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        result_3 <= to_unsigned(0,64);
      else
        result_3 <= x;
      end if;
    end if;
  end process;
  -- register end

  x <= result_0.tup2_0_sel1_corestate.corestate_sel4_rvfiorder;

  -- src/Contranomy/Core/MachineState.hs:36:1-23
  -- register begin 
  capp_arg_machinestate_mstatus_mie_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mstatus_mie\ <= false;
      else
        \c$app_arg_machineState_mstatus_mie\ <= x_0;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:36:1-23
  -- register begin 
  capp_arg_machinestate_mstatus_mpie_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mstatus_mpie\ <= false;
      else
        \c$app_arg_machineState_mstatus_mpie\ <= x_1;
      end if;
    end if;
  end process;
  -- register end

  x_0 <= x_7.mstatus_sel0_mie;

  x_1 <= x_7.mstatus_sel1_mpie;

  -- src/Contranomy/Core/MachineState.hs:36:1-23
  result_4 <= ( mstatus_sel0_mie => \c$app_arg_machineState_mstatus_mie\
              , mstatus_sel1_mpie => \c$app_arg_machineState_mstatus_mpie\ );

  -- src/Contranomy/Instruction.hs:155:1-22
  -- register begin 
  capp_arg_machinestate_mcause_interrupt_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mcause_interrupt\ <= false;
      else
        \c$app_arg_machineState_mcause_interrupt\ <= x_2;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Instruction.hs:155:1-22
  -- register begin 
  capp_arg_machinestate_mcause_code_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mcause_code\ <= std_logic_vector'(x"0");
      else
        \c$app_arg_machineState_mcause_code\ <= x_3;
      end if;
    end if;
  end process;
  -- register end

  x_2 <= x_8.mcause_sel0_interrupt;

  x_3 <= x_8.mcause_sel1_code;

  -- src/Contranomy/Instruction.hs:155:1-22
  result_5 <= ( mcause_sel0_interrupt => \c$app_arg_machineState_mcause_interrupt\
              , mcause_sel1_code => \c$app_arg_machineState_mcause_code\ );

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  -- register begin 
  capp_arg_machinestate_mtvec_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mtvec\ <= std_logic_vector'(std_logic_vector(resize(unsigned(std_logic_vector'(std_logic_vector'("000000000000000000000000000000"))),30)) & "00");
      else
        \c$app_arg_machineState_mtvec\ <= x_9;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:64:1-19
  -- register begin 
  capp_arg_machinestate_mie_meie_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mie_meie\ <= false;
      else
        \c$app_arg_machineState_mie_meie\ <= x_4;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:64:1-19
  -- register begin 
  capp_arg_machinestate_mie_mtie_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mie_mtie\ <= false;
      else
        \c$app_arg_machineState_mie_mtie\ <= x_5;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:64:1-19
  -- register begin 
  capp_arg_machinestate_mie_msie_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mie_msie\ <= false;
      else
        \c$app_arg_machineState_mie_msie\ <= x_6;
      end if;
    end if;
  end process;
  -- register end

  x_4 <= x_10.mie_sel0_meie;

  x_5 <= x_10.mie_sel1_mtie;

  x_6 <= x_10.mie_sel2_msie;

  -- src/Contranomy/Core/MachineState.hs:64:1-19
  result_6 <= ( mie_sel0_meie => \c$app_arg_machineState_mie_meie\
              , mie_sel1_mtie => \c$app_arg_machineState_mie_mtie\
              , mie_sel2_msie => \c$app_arg_machineState_mie_msie\ );

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  -- register begin 
  capp_arg_machinestate_mscratch_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mscratch\ <= std_logic_vector'(x"00000000");
      else
        \c$app_arg_machineState_mscratch\ <= x_11;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  -- register begin 
  capp_arg_machinestate_mepc_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mepc\ <= std_logic_vector'("000000000000000000000000000000");
      else
        \c$app_arg_machineState_mepc\ <= x_12;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  -- register begin 
  capp_arg_machinestate_mtval_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_mtval\ <= std_logic_vector'(x"00000000");
      else
        \c$app_arg_machineState_mtval\ <= x_13;
      end if;
    end if;
  end process;
  -- register end

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  -- register begin 
  capp_arg_machinestate_irqmask_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$app_arg_machineState_irqmask\ <= std_logic_vector'(x"00000000");
      else
        \c$app_arg_machineState_irqmask\ <= x_14;
      end if;
    end if;
  end process;
  -- register end

  x_7 <= x_15.machinestate_sel0_mstatus;

  x_8 <= x_15.machinestate_sel1_mcause;

  x_9 <= x_15.machinestate_sel2_mtvec;

  x_10 <= x_15.machinestate_sel3_mie;

  x_11 <= x_15.machinestate_sel4_mscratch;

  x_12 <= x_15.machinestate_sel5_mepc;

  x_13 <= x_15.machinestate_sel6_mtval;

  x_14 <= x_15.machinestate_sel7_irqmask;

  -- src/Contranomy/Core/MachineState.hs:79:1-28
  result_7 <= ( machinestate_sel0_mstatus => result_4
              , machinestate_sel1_mcause => result_5
              , machinestate_sel2_mtvec => \c$app_arg_machineState_mtvec\
              , machinestate_sel3_mie => result_6
              , machinestate_sel4_mscratch => \c$app_arg_machineState_mscratch\
              , machinestate_sel5_mepc => \c$app_arg_machineState_mepc\
              , machinestate_sel6_mtval => \c$app_arg_machineState_mtval\
              , machinestate_sel7_irqmask => \c$app_arg_machineState_irqmask\ );

  x_15 <= result_0.tup2_0_sel1_corestate.corestate_sel3_machinestate;

  -- src/Contranomy/Core/CoreState.hs:30:1-25
  -- register begin 
  result_8_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        result_8 <= std_logic_vector'(x"00000000");
      else
        result_8 <= x_16;
      end if;
    end if;
  end process;
  -- register end

  x_16 <= result_0.tup2_0_sel1_corestate.corestate_sel2_instruction;

  -- src/Contranomy/Core/CoreState.hs:30:1-25
  -- register begin 
  result_9_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        result_9 <= std_logic_vector'("000000000000000000000000000000");
      else
        result_9 <= x_17;
      end if;
    end if;
  end process;
  -- register end

  x_17 <= result_0.tup2_0_sel1_corestate.corestate_sel1_pc;

  -- src/Contranomy/Core/CoreState.hs:30:1-25
  -- register begin 
  result_10_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        result_10 <= std_logic_vector'("0" & "-");
      else
        result_10 <= x_18;
      end if;
    end if;
  end process;
  -- register end

  x_18 <= result_0.tup2_0_sel1_corestate.corestate_sel0_stage;


end;


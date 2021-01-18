-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;

entity handleexceptions is
  port(ds            : in contranomy_types.corestate;
       \exceptionIn\ : in contranomy_types.exceptionin;
       \lsFinished\  : in boolean;
       ds1           : in contranomy_types.tup2_5;
       \c$arg\       : in contranomy_types.corestate;
       result        : out contranomy_types.tup2_4);
end;

architecture structural of handleexceptions is
  signal result_0                           : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:(92,29)-(93,82)
  signal \c$case_alt\                       : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:(92,52)-(93,82)
  signal \c$case_alt_0\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:93:9-82
  signal \c$case_alt_1\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:93:35-82
  signal \c$case_alt_2\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:93:64-82
  signal \c$case_alt_3\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:93:78-82
  signal \c$case_alt_4\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:105:14-22
  signal \c$case_alt_5\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:(152,8)-(156,22)
  signal \c$case_alt_6\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:87:13-22
  signal \c$case_alt_7\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:88:14-36
  signal \c$case_alt_8\                     : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:154:12-23
  signal eta1                               : contranomy_types.tup2_3;
  -- src/Contranomy/Core/Exception.hs:153:5-68
  signal g5                                 : unsigned(63 downto 0);
  -- src/Contranomy/Core/Exception.hs:153:5-68
  signal g3                                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:153:5-68
  signal g2                                 : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core/Exception.hs:153:5-68
  signal g1                                 : contranomy_types.corestage;
  -- src/Contranomy/Core/Exception.hs:156:12-22
  signal eta                                : contranomy_types.tup2_3;
  -- src/Contranomy/Core/Exception.hs:(105,29)-(151,22)
  signal result_1                           : contranomy_types.tup2_4;
  -- src/Contranomy/Core/Exception.hs:151:12-22
  signal eta1_0                             : contranomy_types.tup2_3;
  -- src/Contranomy/Core/Exception.hs:(106,5)-(149,21)
  signal g5_0                               : unsigned(63 downto 0);
  -- src/Contranomy/Core/Exception.hs:(106,5)-(149,21)
  signal g3_0                               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(106,5)-(149,21)
  signal g2_0                               : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core/Exception.hs:(106,5)-(149,21)
  signal g1_0                               : contranomy_types.corestage;
  -- src/Contranomy/Core/Exception.hs:151:12-22
  signal \c$eta_case_alt\                   : std_logic_vector(29 downto 0);
  -- src/Contranomy/Core/Exception.hs:(136,23)-(148,32)
  signal \c$b1_app_arg\                     : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(138,28)-(148,32)
  signal \c$b1_case_alt\                    : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(140,28)-(148,32)
  signal \c$b1_case_alt_0\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(142,28)-(148,32)
  signal \c$b1_case_alt_1\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(106,22)-(149,21)
  signal \c$b1_case_alt_2\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(144,28)-(148,32)
  signal \c$b1_case_alt_3\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:145:38-41
  signal addr                               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(146,30)-(148,32)
  signal \c$b1_case_alt_4\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:147:40-43
  signal addr_0                             : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Exception.hs:(109,23)-(133,57)
  signal \c$b1_app_arg_0\                   : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(110,25)-(115,52)
  signal \c$b1_case_alt_5\                  : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(117,25)-(133,57)
  signal \c$b1_case_alt_6\                  : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(106,22)-(149,21)
  signal \c$b1_case_alt_7\                  : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(112,30)-(115,52)
  signal \c$b1_case_alt_8\                  : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(106,22)-(149,21)
  signal \c$b1_case_alt_9\                  : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(119,30)-(133,57)
  signal \c$b1_case_alt_10\                 : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(121,30)-(133,57)
  signal \c$b1_case_alt_11\                 : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(123,30)-(133,57)
  signal \c$b1_case_alt_12\                 : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(125,30)-(133,57)
  signal \c$b1_case_alt_13\                 : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(127,30)-(133,57)
  signal \c$b1_case_alt_14\                 : contranomy_types.mcause;
  -- src/Contranomy/Core/Exception.hs:(101,7)-(102,90)
  signal interrupt                          : boolean;
  -- src/Contranomy/Core/Exception.hs:(101,7)-(102,90)
  signal \c$interrupt_case_alt\             : boolean;
  -- src/Contranomy/Core/Exception.hs:102:30-90
  signal result_2                           : boolean;
  -- src/Contranomy/Core/Exception.hs:98:46-49
  signal \c$interrupt_case_alt_0\           : boolean;
  -- src/Contranomy/Core/Exception.hs:102:50-89
  signal result_3                           : boolean;
  -- src/Contranomy/Core/Exception.hs:99:49-52
  signal \c$$j_case_alt\                    : boolean;
  -- src/Contranomy/Core/Exception.hs:102:72-89
  signal \$j1\                              : boolean;
  -- src/Contranomy/Core/Exception.hs:(79,7)-(83,18)
  signal \eCall\                            : boolean;
  -- src/Contranomy/Core/Exception.hs:81:13-22
  signal \c$eCall_case_alt\                 : boolean;
  -- src/Contranomy/Core/Exception.hs:(73,7)-(77,18)
  signal breakpoint                         : boolean;
  -- src/Contranomy/Core/Exception.hs:75:13-22
  signal \c$breakpoint_case_alt\            : boolean;
  -- src/Contranomy/Core/Exception.hs:70:13-23
  signal func3                              : std_logic_vector(2 downto 0);
  -- src/Contranomy/Core/Exception.hs:70:13-23
  signal \imm12I\                           : std_logic_vector(11 downto 0);
  -- src/Contranomy/Core/Exception.hs:70:13-23
  signal opcode                             : std_logic_vector(6 downto 0);
  -- src/Contranomy/Core/Exception.hs:72:49-77
  signal ds15                               : contranomy_types.decodedinstruction;
  -- src/Contranomy/Core/Exception.hs:(60,94)-(156,22)
  signal align                              : std_logic_vector(1 downto 0);
  -- src/Contranomy/Core/Exception.hs:(60,94)-(156,22)
  signal \pcN\                              : std_logic_vector(29 downto 0);
  signal result_0_selection                 : boolean;
  signal \c$case_alt_selection\             : boolean;
  signal \c$case_alt_0_selection\           : boolean;
  signal \c$case_alt_1_selection\           : contranomy_types.maybe_0;
  signal \c$case_alt_2_selection\           : contranomy_types.maybe_0;
  signal \c$eta_case_alt_selection\         : contranomy_types.interruptmode;
  signal \c$b1_app_arg_selection\           : boolean;
  signal \c$b1_case_alt_selection\          : boolean;
  signal \c$b1_case_alt_0_selection\        : boolean;
  signal \c$b1_case_alt_3_selection\        : contranomy_types.maybe_0;
  signal \c$b1_case_alt_4_selection\        : contranomy_types.maybe_0;
  signal \c$b1_case_alt_5_selection\        : boolean;
  signal \c$b1_case_alt_6_selection\        : boolean;
  signal \c$b1_case_alt_7_selection\        : boolean;
  signal \c$b1_case_alt_8_selection\        : boolean;
  signal \c$b1_case_alt_9_selection\        : boolean;
  signal \c$b1_case_alt_10_selection\       : boolean;
  signal \c$b1_case_alt_11_selection\       : boolean;
  signal \c$interrupt_case_alt_selection\   : boolean;
  signal result_2_selection                 : boolean;
  signal \c$interrupt_case_alt_0_selection\ : boolean;
  signal result_3_selection                 : boolean;
  signal \c$$j_case_alt_selection\          : boolean;
  signal \c$$j1_selection_res\              : boolean;
  signal ds15_fun_arg                       : std_logic_vector(31 downto 0);

begin
  result_0_selection <= \exceptionIn\.exceptionin_sel0_instraccessfault;

  result_0 <= result_1 when result_0_selection else
              \c$case_alt\;

  \c$case_alt_selection\ <= \exceptionIn\.exceptionin_sel1_instraddrmisaligned;

  -- src/Contranomy/Core/Exception.hs:(92,29)-(93,82)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$case_alt\ <= result_1 when \c$case_alt_selection\ else
                  \c$case_alt_0\;

  \c$case_alt_0_selection\ <= \exceptionIn\.exceptionin_sel2_instrillegal;

  -- src/Contranomy/Core/Exception.hs:(92,52)-(93,82)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$case_alt_0\ <= result_1 when \c$case_alt_0_selection\ else
                    \c$case_alt_1\;

  \c$case_alt_1_selection\ <= \exceptionIn\.exceptionin_sel3_dataaccessfault;

  -- src/Contranomy/Core/Exception.hs:93:9-82
  -- src/Contranomy/Core/Exception.hs:70:13-23
  with (\c$case_alt_1_selection\(32 downto 32)) select
    \c$case_alt_1\ <= \c$case_alt_2\ when "0",
                      result_1 when others;

  \c$case_alt_2_selection\ <= \exceptionIn\.exceptionin_sel4_dataaddrmisaligned;

  -- src/Contranomy/Core/Exception.hs:93:35-82
  -- src/Contranomy/Core/Exception.hs:70:13-23
  with (\c$case_alt_2_selection\(32 downto 32)) select
    \c$case_alt_2\ <= \c$case_alt_3\ when "0",
                      result_1 when others;

  -- src/Contranomy/Core/Exception.hs:93:64-82
  \c$case_alt_3\ <= result_1 when breakpoint else
                    \c$case_alt_4\;

  -- src/Contranomy/Core/Exception.hs:93:78-82
  \c$case_alt_4\ <= result_1 when \eCall\ else
                    \c$case_alt_5\;

  -- src/Contranomy/Core/Exception.hs:105:14-22
  \c$case_alt_5\ <= result_1 when interrupt else
                    \c$case_alt_6\;

  -- src/Contranomy/Core/Exception.hs:(152,8)-(156,22)
  -- src/Contranomy/Core/Exception.hs:(85,7)-(89,18)
  -- src/Contranomy/Core/Exception.hs:(85,14)-(89,18)
  -- src/Contranomy/Instruction.hs:188:18-33
  with (opcode) select
    \c$case_alt_6\ <= \c$case_alt_7\ when "1110011",
                      ( tup2_4_sel0_tup2_3 => eta
                      , tup2_4_sel1_corestate => \c$arg\ ) when others;

  -- src/Contranomy/Core/Exception.hs:87:13-22
  with (func3) select
    \c$case_alt_7\ <= \c$case_alt_8\ when "000",
                      ( tup2_4_sel0_tup2_3 => eta
                      , tup2_4_sel1_corestate => \c$arg\ ) when others;

  -- src/Contranomy/Core/Exception.hs:88:14-36
  -- src/Contranomy/Instruction.hs:327:20-21
  with (\imm12I\) select
    \c$case_alt_8\ <= ( tup2_4_sel0_tup2_3 => eta1
                      , tup2_4_sel1_corestate => ( corestate_sel0_stage => g1
                      , corestate_sel1_pc => g2
                      , corestate_sel2_instruction => g3
                      , corestate_sel3_machinestate => ( machinestate_sel0_mstatus => ( mstatus_sel0_mie => ds.corestate_sel3_machinestate.machinestate_sel0_mstatus.mstatus_sel1_mpie
                      , mstatus_sel1_mpie => ds.corestate_sel3_machinestate.machinestate_sel0_mstatus.mstatus_sel1_mpie )
                      , machinestate_sel1_mcause => ds.corestate_sel3_machinestate.machinestate_sel1_mcause
                      , machinestate_sel2_mtvec => ds.corestate_sel3_machinestate.machinestate_sel2_mtvec
                      , machinestate_sel3_mie => ds.corestate_sel3_machinestate.machinestate_sel3_mie
                      , machinestate_sel4_mscratch => ds.corestate_sel3_machinestate.machinestate_sel4_mscratch
                      , machinestate_sel5_mepc => ds.corestate_sel3_machinestate.machinestate_sel5_mepc
                      , machinestate_sel6_mtval => ds.corestate_sel3_machinestate.machinestate_sel6_mtval
                      , machinestate_sel7_irqmask => ds.corestate_sel3_machinestate.machinestate_sel7_irqmask )
                      , corestate_sel4_rvfiorder => g5 ) ) when x"302",
                      ( tup2_4_sel0_tup2_3 => eta
                      , tup2_4_sel1_corestate => \c$arg\ ) when others;

  -- src/Contranomy/Core/Exception.hs:154:12-23
  eta1 <= ( tup2_3_sel0_boolean => false
          , tup2_3_sel1_std_logic_vector => ds.corestate_sel3_machinestate.machinestate_sel5_mepc );

  g5 <= \c$arg\.corestate_sel4_rvfiorder;

  g3 <= \c$arg\.corestate_sel2_instruction;

  g2 <= \c$arg\.corestate_sel1_pc;

  g1 <= \c$arg\.corestate_sel0_stage;

  -- src/Contranomy/Core/Exception.hs:156:12-22
  eta <= ( tup2_3_sel0_boolean => false
         , tup2_3_sel1_std_logic_vector => \pcN\ );

  -- src/Contranomy/Core/Exception.hs:(105,29)-(151,22)
  result_1 <= ( tup2_4_sel0_tup2_3 => eta1_0
              , tup2_4_sel1_corestate => ( corestate_sel0_stage => g1_0
              , corestate_sel1_pc => g2_0
              , corestate_sel2_instruction => g3_0
              , corestate_sel3_machinestate => ( machinestate_sel0_mstatus => ( mstatus_sel0_mie => false
              , mstatus_sel1_mpie => ds.corestate_sel3_machinestate.machinestate_sel0_mstatus.mstatus_sel0_mie )
              , machinestate_sel1_mcause => \c$b1_app_arg_0\
              , machinestate_sel2_mtvec => ds.corestate_sel3_machinestate.machinestate_sel2_mtvec
              , machinestate_sel3_mie => ds.corestate_sel3_machinestate.machinestate_sel3_mie
              , machinestate_sel4_mscratch => ds.corestate_sel3_machinestate.machinestate_sel4_mscratch
              , machinestate_sel5_mepc => std_logic_vector'(ds.corestate_sel1_pc)
              , machinestate_sel6_mtval => \c$b1_app_arg\
              , machinestate_sel7_irqmask => ds.corestate_sel3_machinestate.machinestate_sel7_irqmask )
              , corestate_sel4_rvfiorder => g5_0 ) );

  -- src/Contranomy/Core/Exception.hs:151:12-22
  eta1_0 <= ( tup2_3_sel0_boolean => true
            , tup2_3_sel1_std_logic_vector => \c$eta_case_alt\ );

  g5_0 <= \c$arg\.corestate_sel4_rvfiorder;

  g3_0 <= \c$arg\.corestate_sel2_instruction;

  g2_0 <= \c$arg\.corestate_sel1_pc;

  g1_0 <= \c$arg\.corestate_sel0_stage;

  \c$eta_case_alt_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel2_mtvec;

  \c$eta_case_alt\ <= ds.corestate_sel3_machinestate.machinestate_sel2_mtvec(31 downto 2) when std_match("------------------------------00", \c$eta_case_alt_selection\) else
                      ds.corestate_sel3_machinestate.machinestate_sel2_mtvec(31 downto 2) when std_match("------------------------------01", \c$eta_case_alt_selection\) else
                      std_logic_vector'(0 to 29 => '-');

  \c$b1_app_arg_selection\ <= \exceptionIn\.exceptionin_sel1_instraddrmisaligned;

  -- src/Contranomy/Core/Exception.hs:(136,23)-(148,32)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_app_arg\ <= std_logic_vector'(std_logic_vector'(\pcN\) & std_logic_vector'(align)) when \c$b1_app_arg_selection\ else
                    \c$b1_case_alt\;

  \c$b1_case_alt_selection\ <= \exceptionIn\.exceptionin_sel2_instrillegal;

  -- src/Contranomy/Core/Exception.hs:(138,28)-(148,32)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt\ <= ds.corestate_sel2_instruction when \c$b1_case_alt_selection\ else
                     \c$b1_case_alt_0\;

  \c$b1_case_alt_0_selection\ <= \exceptionIn\.exceptionin_sel0_instraccessfault;

  -- src/Contranomy/Core/Exception.hs:(140,28)-(148,32)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_0\ <= \c$b1_case_alt_2\ when \c$b1_case_alt_0_selection\ else
                       \c$b1_case_alt_1\;

  -- src/Contranomy/Core/Exception.hs:(142,28)-(148,32)
  \c$b1_case_alt_1\ <= \c$b1_case_alt_2\ when breakpoint else
                       \c$b1_case_alt_3\;

  -- src/Contranomy/Core/Exception.hs:143:25-32
  \c$b1_case_alt_2\ <= std_logic_vector'(std_logic_vector'(ds.corestate_sel1_pc) & std_logic_vector'(std_logic_vector'("00")));

  \c$b1_case_alt_3_selection\ <= \exceptionIn\.exceptionin_sel4_dataaddrmisaligned;

  -- src/Contranomy/Core/Exception.hs:(144,28)-(148,32)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  with (\c$b1_case_alt_3_selection\(32 downto 32)) select
    \c$b1_case_alt_3\ <= \c$b1_case_alt_4\ when "0",
                         addr when others;

  addr <= \exceptionIn\.exceptionin_sel4_dataaddrmisaligned(31 downto 0);

  \c$b1_case_alt_4_selection\ <= \exceptionIn\.exceptionin_sel3_dataaccessfault;

  -- src/Contranomy/Core/Exception.hs:(146,30)-(148,32)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  with (\c$b1_case_alt_4_selection\(32 downto 32)) select
    \c$b1_case_alt_4\ <= std_logic_vector'(x"00000000") when "0",
                         addr_0 when others;

  addr_0 <= \exceptionIn\.exceptionin_sel3_dataaccessfault(31 downto 0);

  -- src/Contranomy/Core/Exception.hs:(109,23)-(133,57)
  \c$b1_app_arg_0\ <= \c$b1_case_alt_5\ when interrupt else
                      \c$b1_case_alt_6\;

  \c$b1_case_alt_5_selection\ <= \exceptionIn\.exceptionin_sel6_softwareinterrupt;

  -- src/Contranomy/Core/Exception.hs:(110,25)-(115,52)
  -- src/Contranomy/Core/Exception.hs:99:7-52
  -- src/Contranomy/Core/Exception.hs:99:28-52
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_5\ <= \c$b1_case_alt_7\ when \c$b1_case_alt_5_selection\ else
                       \c$b1_case_alt_8\;

  \c$b1_case_alt_6_selection\ <= \exceptionIn\.exceptionin_sel0_instraccessfault;

  -- src/Contranomy/Core/Exception.hs:(117,25)-(133,57)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_6\ <= ( mcause_sel0_interrupt => false
                       , mcause_sel1_code => std_logic_vector'(x"1") ) when \c$b1_case_alt_6_selection\ else
                       \c$b1_case_alt_10\;

  \c$b1_case_alt_7_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel3_mie.mie_sel2_msie;

  \c$b1_case_alt_7\ <= ( mcause_sel0_interrupt => true
                       , mcause_sel1_code => std_logic_vector'(x"3") ) when \c$b1_case_alt_7_selection\ else
                       \c$b1_case_alt_8\;

  \c$b1_case_alt_8_selection\ <= \exceptionIn\.exceptionin_sel5_timerinterrupt;

  -- src/Contranomy/Core/Exception.hs:(112,30)-(115,52)
  -- src/Contranomy/Core/Exception.hs:98:7-49
  -- src/Contranomy/Core/Exception.hs:98:28-49
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_8\ <= \c$b1_case_alt_9\ when \c$b1_case_alt_8_selection\ else
                       ( mcause_sel0_interrupt => true
                       , mcause_sel1_code => std_logic_vector'(x"B") );

  \c$b1_case_alt_9_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel3_mie.mie_sel1_mtie;

  \c$b1_case_alt_9\ <= ( mcause_sel0_interrupt => true
                       , mcause_sel1_code => std_logic_vector'(x"7") ) when \c$b1_case_alt_9_selection\ else
                       ( mcause_sel0_interrupt => true
                       , mcause_sel1_code => std_logic_vector'(x"B") );

  \c$b1_case_alt_10_selection\ <= \exceptionIn\.exceptionin_sel2_instrillegal;

  -- src/Contranomy/Core/Exception.hs:(119,30)-(133,57)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_10\ <= ( mcause_sel0_interrupt => false
                        , mcause_sel1_code => std_logic_vector'(x"2") ) when \c$b1_case_alt_10_selection\ else
                        \c$b1_case_alt_11\;

  \c$b1_case_alt_11_selection\ <= \exceptionIn\.exceptionin_sel1_instraddrmisaligned;

  -- src/Contranomy/Core/Exception.hs:(121,30)-(133,57)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  \c$b1_case_alt_11\ <= ( mcause_sel0_interrupt => false
                        , mcause_sel1_code => std_logic_vector'(x"0") ) when \c$b1_case_alt_11_selection\ else
                        \c$b1_case_alt_12\;

  -- src/Contranomy/Core/Exception.hs:(123,30)-(133,57)
  \c$b1_case_alt_12\ <= ( mcause_sel0_interrupt => false
                        , mcause_sel1_code => std_logic_vector'(x"B") ) when \eCall\ else
                        \c$b1_case_alt_13\;

  -- src/Contranomy/Core/Exception.hs:(125,30)-(133,57)
  \c$b1_case_alt_13\ <= ( mcause_sel0_interrupt => false
                        , mcause_sel1_code => std_logic_vector'(x"3") ) when breakpoint else
                        \c$b1_case_alt_14\;

  -- src/Contranomy/Core/Exception.hs:(127,30)-(133,57)
  -- src/Contranomy/Core/Exception.hs:70:13-23
  -- src/Contranomy/Core/Exception.hs:(131,32)-(133,57)
  -- src/Contranomy/Instruction.hs:183:16-31
  with (opcode) select
    \c$b1_case_alt_14\ <= ( mcause_sel0_interrupt => false
                          , mcause_sel1_code => std_logic_vector'(x"4") ) when "0000011",
                          ( mcause_sel0_interrupt => false
                          , mcause_sel1_code => std_logic_vector'(x"6") ) when others;

  -- src/Contranomy/Core/Exception.hs:(101,7)-(102,90)
  -- src/Contranomy/Core/Exception.hs:102:9-90
  interrupt <= \c$interrupt_case_alt\ when \lsFinished\ else
               false;

  \c$interrupt_case_alt_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel0_mstatus.mstatus_sel0_mie;

  \c$interrupt_case_alt\ <= result_2 when \c$interrupt_case_alt_selection\ else
                            false;

  result_2_selection <= \exceptionIn\.exceptionin_sel5_timerinterrupt;

  -- src/Contranomy/Core/Exception.hs:102:30-90
  -- src/Contranomy/Core/Exception.hs:98:7-49
  -- src/Contranomy/Core/Exception.hs:98:28-49
  -- src/Contranomy/Core/Exception.hs:70:13-23
  result_2 <= \c$interrupt_case_alt_0\ when result_2_selection else
              result_3;

  \c$interrupt_case_alt_0_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel3_mie.mie_sel1_mtie;

  -- src/Contranomy/Core/Exception.hs:98:46-49
  -- src/Contranomy/Core/Exception.hs:95:74-85
  \c$interrupt_case_alt_0\ <= true when \c$interrupt_case_alt_0_selection\ else
                              result_3;

  result_3_selection <= \exceptionIn\.exceptionin_sel6_softwareinterrupt;

  -- src/Contranomy/Core/Exception.hs:102:50-89
  -- src/Contranomy/Core/Exception.hs:99:7-52
  -- src/Contranomy/Core/Exception.hs:99:28-52
  -- src/Contranomy/Core/Exception.hs:70:13-23
  result_3 <= \c$$j_case_alt\ when result_3_selection else
              \$j1\;

  \c$$j_case_alt_selection\ <= ds.corestate_sel3_machinestate.machinestate_sel3_mie.mie_sel2_msie;

  -- src/Contranomy/Core/Exception.hs:99:49-52
  -- src/Contranomy/Core/Exception.hs:95:74-85
  \c$$j_case_alt\ <= true when \c$$j_case_alt_selection\ else
                     \$j1\;

  \c$$j1_selection_res\ <= (\exceptionIn\.exceptionin_sel7_externalinterrupt and ds.corestate_sel3_machinestate.machinestate_sel7_irqmask) /= std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/Exception.hs:102:72-89
  -- src/Contranomy/Core/Exception.hs:100:7-73
  -- src/Contranomy/Core/Exception.hs:100:28-73
  \$j1\ <= ds.corestate_sel3_machinestate.machinestate_sel3_mie.mie_sel0_meie when \c$$j1_selection_res\ else
           false;

  -- src/Contranomy/Core/Exception.hs:(79,7)-(83,18)
  -- src/Contranomy/Core/Exception.hs:(79,15)-(83,18)
  -- src/Contranomy/Instruction.hs:188:18-33
  with (opcode) select
    \eCall\ <= \c$eCall_case_alt\ when "1110011",
               false when others;

  -- src/Contranomy/Core/Exception.hs:81:13-22
  with (func3) select
    \c$eCall_case_alt\ <= \imm12I\ = std_logic_vector'(x"000") when "000",
                          false when others;

  -- src/Contranomy/Core/Exception.hs:(73,7)-(77,18)
  -- src/Contranomy/Core/Exception.hs:(73,20)-(77,18)
  -- src/Contranomy/Instruction.hs:188:18-33
  with (opcode) select
    breakpoint <= \c$breakpoint_case_alt\ when "1110011",
                  false when others;

  -- src/Contranomy/Core/Exception.hs:75:13-22
  with (func3) select
    \c$breakpoint_case_alt\ <= \imm12I\ = std_logic_vector'(x"001") when "000",
                               false when others;

  func3 <= ds15.decodedinstruction_sel15_func3;

  \imm12I\ <= ds15.decodedinstruction_sel12_imm12i;

  opcode <= ds15.decodedinstruction_sel0_opcode;

  ds15_fun_arg <= ds.corestate_sel2_instruction;

  -- src/Contranomy/Core/Exception.hs:72:49-77
  decodeinstruction_ds15 : entity decodeinstruction
    port map
      (result => ds15, w      => ds15_fun_arg);

  align <= ds1.tup2_5_sel1_std_logic_vector_1;

  \pcN\ <= ds1.tup2_5_sel0_std_logic_vector_0;

  result <= result_0;


end;


-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomyrvfi_types.all;

entity csrunit_0 is
  port(trap                : in boolean;
       instruction         : in std_logic_vector(31 downto 0);
       \rs1Val\            : in std_logic_vector(31 downto 0);
       \softwareInterrupt\ : in boolean;
       \timerInterrupt\    : in boolean;
       \externalInterrupt\ : in std_logic_vector(31 downto 0);
       \c$arg\             : in contranomyrvfi_types.machinestate;
       result              : out contranomyrvfi_types.tup2_2);
end;

architecture structural of csrunit_0 is
  signal \c$case_alt\                             : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:64:5-14
  signal \c$case_alt_0\                           : contranomyrvfi_types.tup2_2;
  signal \c$case_alt_1\                           : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:69:5-12
  signal \c$case_alt_2\                           : contranomyrvfi_types.tup2_2;
  signal \c$case_alt_3\                           : contranomyrvfi_types.tup2_2;
  signal \c$case_alt_4\                           : contranomyrvfi_types.tup2_2;
  signal result_0                                 : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Instruction.hs:275:22-38
  signal \c$case_alt_5\                           : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl17                                    : boolean;
  -- src/Contranomy/Core/CSR.hs:104:16-40
  signal lvl1                                     : contranomyrvfi_types.tup2_1;
  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  signal result_1                                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:171:3-56
  signal \c$case_alt_6\                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:3-41
  signal \c$case_alt_7\                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:165:3-29
  signal \c$case_alt_8\                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:18-31
  signal x                                        : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:290:20-36
  signal \c$case_alt_9\                           : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl16                                    : boolean;
  -- src/Contranomy/Core/CSR.hs:69:5-12
  signal lvl4                                     : contranomyrvfi_types.tup2_1;
  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  signal result_2                                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:171:3-56
  signal \c$case_alt_10\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:3-41
  signal \c$case_alt_11\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:165:3-29
  signal \c$case_alt_12\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:18-31
  signal x_0                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:110:16-40
  signal lvl2                                     : contranomyrvfi_types.maybe_0;
  -- src/Contranomy/Core/CSR.hs:(106,13)-(108,47)
  signal \oldValue\                               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:278:22-38
  signal \c$case_alt_13\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Instruction.hs:279:22-38
  signal \c$case_alt_14\                          : contranomyrvfi_types.tup2_2;
  signal \c$app_arg\                              : boolean;
  signal \c$app_arg_0\                            : std_logic;
  signal \c$app_arg_1\                            : boolean;
  signal \c$app_arg_2\                            : std_logic;
  signal \c$app_arg_3\                            : std_logic;
  -- src/Contranomy/Instruction.hs:286:20-36
  signal \c$case_alt_15\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Instruction.hs:287:20-36
  signal \c$case_alt_16\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Instruction.hs:288:20-36
  signal \c$case_alt_17\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Instruction.hs:289:20-36
  signal \c$case_alt_18\                          : contranomyrvfi_types.tup2_2;
  signal \c$app_arg_4\                            : std_logic;
  -- src/Contranomy/Instruction.hs:297:22-38
  signal \c$case_alt_19\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl9                                     : boolean;
  -- src/Contranomy/Instruction.hs:298:22-38
  signal \c$case_alt_20\                          : contranomyrvfi_types.tup2_2;
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl8                                     : boolean;
  -- src/Contranomy/Core/CSR.hs:69:5-12
  signal lvl7                                     : contranomyrvfi_types.tup2_1;
  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  signal result_3                                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:171:3-56
  signal \c$case_alt_21\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:3-41
  signal \c$case_alt_22\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:165:3-29
  signal \c$case_alt_23\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:18-31
  signal x_1                                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:151:16-49
  signal lvl5                                     : contranomyrvfi_types.maybe_0;
  -- src/Contranomy/Core/CSR.hs:108:24-47
  signal \c$oldValue_app_arg\                     : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:107:24-44
  signal \c$oldValue_app_arg_0\                   : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:106:24-55
  signal \c$oldValue_app_arg_1\                   : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  signal result_4                                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:171:3-56
  signal \c$case_alt_24\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:3-41
  signal \c$case_alt_25\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:165:3-29
  signal \c$case_alt_26\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:168:18-31
  signal x_2                                      : std_logic_vector(31 downto 0);
  signal \c$csrWriteOut_app_arg\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:96:13-49
  signal \oldValue1\                              : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:96:40-49
  signal \c$oldValue1_app_arg\                    : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:96:24-34
  signal \c$oldValue1_app_arg_0\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl18                                    : boolean;
  signal \c$csrWriteOut_case_alt\                 : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:112:13-67
  signal \c$oldValue1_0\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:112:57-67
  signal \c$oldValue1_app_arg_1\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:112:41-51
  signal \c$oldValue1_app_arg_2\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:112:24-35
  signal \c$oldValue1_app_arg_3\                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl15                                    : boolean;
  signal \c$csrWriteOut_case_alt_0\               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:120:13-33
  signal \c$oldValue1_1\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl14                                    : boolean;
  signal \c$csrWriteOut_case_alt_1\               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl13                                    : boolean;
  signal \c$csrWriteOut_case_alt_2\               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:130:13-33
  signal \c$oldValue1_2\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl12                                    : boolean;
  signal \c$csrWriteOut_case_alt_3\               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:135:13-52
  signal \c$oldValue1_3\                          : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:135:13-52
  signal \c$oldValue1_app_arg_4\                  : std_logic_vector(0 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl11                                    : boolean;
  signal \c$csrWriteOut_case_alt_4\               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:94:10-28
  signal lvl10                                    : boolean;
  -- src/Contranomy/Core/CSR.hs:(89,9)-(92,43)
  signal \c$writeValue1_case_alt\                 : contranomyrvfi_types.maybe_0;
  -- src/Contranomy/Core/CSR.hs:90:24-39
  signal \csrType1\                               : contranomyrvfi_types.csrtype;
  -- src/Contranomy/Core/CSR.hs:91:15-23
  signal \c$writeValue1_case_alt_0\               : contranomyrvfi_types.maybe_0;
  -- src/Contranomy/Core/CSR.hs:90:24-39
  signal \writeValue0\                            : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/CSR.hs:(85,33)-(87,47)
  signal ds17                                     : contranomyrvfi_types.tup2_9;
  -- src/Contranomy/Core/CSR.hs:87:23-47
  signal t                                        : contranomyrvfi_types.csrtype;
  -- src/Contranomy/Core/CSR.hs:86:23-32
  signal t_0                                      : contranomyrvfi_types.csrtype;
  -- src/Contranomy/Core/CSR.hs:(85,33)-(87,47)
  signal \c$ds17_case_scrut\                      : contranomyrvfi_types.csrop;
  -- src/Contranomy/Core/CSR.hs:(89,9)-(92,43)
  signal \c$writeValue1_app_arg\                  : std_logic_vector(4 downto 0);
  signal \c$case_scrut\                           : contranomyrvfi_types.decodedinstruction;
  signal \c$case_alt_selection\                   : std_logic_vector(6 downto 0);
  signal \c$case_alt_0_selection_res\             : boolean;
  signal \c$oldValue_app_arg_1_selection_res\     : boolean;
  signal \c$oldValue1_app_arg_selection\          : boolean;
  signal \c$oldValue1_app_arg_0_selection\        : boolean;
  signal \c$oldValue1_app_arg_1_selection\        : boolean;
  signal \c$oldValue1_app_arg_2_selection\        : boolean;
  signal \c$oldValue1_app_arg_3_selection\        : boolean;
  signal \c$oldValue1_app_arg_4_selection\        : boolean;
  signal \c$writeValue1_case_alt_0_selection_res\ : boolean;

begin
  \c$case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (\c$case_alt_selection\) select
    \c$case_alt\ <= \c$case_alt_0\ when "1110011",
                    \c$case_alt_1\ when others;

  \c$case_alt_0_selection_res\ <= \c$case_scrut\.decodedinstruction_sel15_func3 /= std_logic_vector'("000");

  -- src/Contranomy/Core/CSR.hs:64:5-14
  \c$case_alt_0\ <= \c$case_alt_2\ when \c$case_alt_0_selection_res\ else
                    \c$case_alt_3\;

  -- src/Contranomy/Core/CSR.hs:154:5-13
  -- src/Contranomy/Core/CSR.hs:155:5-31
  \c$case_alt_1\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("0" & "--------------------------------")
                    , tup2_1_sel1_std_logic_vector => std_logic_vector'(0 to 31 => '-') )
                    , tup2_2_sel1_machinestate => \c$arg\ );

  -- src/Contranomy/Core/CSR.hs:69:5-12
  \c$case_alt_2\ <= \c$case_alt_4\ when trap else
                    result_0;

  -- src/Contranomy/Core/CSR.hs:154:5-13
  -- src/Contranomy/Core/CSR.hs:155:5-31
  \c$case_alt_3\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("0" & "--------------------------------")
                    , tup2_1_sel1_std_logic_vector => std_logic_vector'(0 to 31 => '-') )
                    , tup2_2_sel1_machinestate => \c$arg\ );

  -- src/Contranomy/Core/CSR.hs:154:5-13
  -- src/Contranomy/Core/CSR.hs:155:5-31
  \c$case_alt_4\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("0" & "--------------------------------")
                    , tup2_1_sel1_std_logic_vector => std_logic_vector'(0 to 31 => '-') )
                    , tup2_2_sel1_machinestate => \c$arg\ );

  result_0 <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\oldValue1\))
              , tup2_1_sel1_std_logic_vector => result_4 )
              , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => ( mstatus_sel0_mie => \c$app_arg\
              , mstatus_sel1_mpie => \c$app_arg_1\ )
              , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
              , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
              , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
              , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
              , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
              , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
              , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl18 else
              \c$case_alt_5\;

  -- src/Contranomy/Instruction.hs:275:22-38
  \c$case_alt_5\ <= ( tup2_2_sel0_tup2_1 => lvl1
                    , tup2_2_sel1_machinestate => \c$arg\ ) when lvl17 else
                    \c$case_alt_9\;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl17 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"301");

  lvl1 <= ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (std_logic_vector'(x"40000100")))
          , tup2_1_sel1_std_logic_vector => result_1 );

  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  result_1 <= \c$case_alt_8\ when std_match("01", \csrType1\) else
              \c$case_alt_7\ when std_match("10", \csrType1\) else
              \c$case_alt_6\ when std_match("11", \csrType1\) else
              std_logic_vector'(x"40000100");

  -- src/Contranomy/Core/CSR.hs:171:3-56
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_6\ <= std_logic_vector'(x"40000100") when "0",
                      std_logic_vector'(x"40000100") and (not x) when others;

  -- src/Contranomy/Core/CSR.hs:168:3-41
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_7\ <= std_logic_vector'(x"40000100") when "0",
                      std_logic_vector'(x"40000100") or x when others;

  -- src/Contranomy/Core/CSR.hs:165:3-29
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_8\ <= std_logic_vector'(x"40000100") when "0",
                      x when others;

  x <= \c$writeValue1_case_alt\(31 downto 0);

  -- src/Contranomy/Instruction.hs:290:20-36
  \c$case_alt_9\ <= ( tup2_2_sel0_tup2_1 => lvl4
                    , tup2_2_sel1_machinestate => \c$arg\ ) when lvl16 else
                    \c$case_alt_13\;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl16 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"344");

  lvl4 <= ( tup2_1_sel0_maybe_0 => lvl2
          , tup2_1_sel1_std_logic_vector => result_2 );

  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  result_2 <= \c$case_alt_12\ when std_match("01", \csrType1\) else
              \c$case_alt_11\ when std_match("10", \csrType1\) else
              \c$case_alt_10\ when std_match("11", \csrType1\) else
              \oldValue\;

  -- src/Contranomy/Core/CSR.hs:171:3-56
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_10\ <= \oldValue\ when "0",
                       \oldValue\ and (not x_0) when others;

  -- src/Contranomy/Core/CSR.hs:168:3-41
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_11\ <= \oldValue\ when "0",
                       \oldValue\ or x_0 when others;

  -- src/Contranomy/Core/CSR.hs:165:3-29
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_12\ <= \oldValue\ when "0",
                       x_0 when others;

  x_0 <= \c$writeValue1_case_alt\(31 downto 0);

  -- src/Contranomy/Core/CSR.hs:110:16-40
  lvl2 <= std_logic_vector'("1" & (\oldValue\));

  -- src/Contranomy/Core/CSR.hs:(106,13)-(108,47)
  -- src/Contranomy/Core/CSR.hs:(106,24)-(108,47)
  \oldValue\ <= (\c$oldValue_app_arg_1\ or \c$oldValue_app_arg_0\) or \c$oldValue_app_arg\;

  -- src/Contranomy/Instruction.hs:278:22-38
  \c$case_alt_13\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$oldValue1_0\))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => ( mie_sel0_meie => \c$app_arg_3\ = ('1')
                     , mie_sel1_mtie => \c$app_arg_1\
                     , mie_sel2_msie => \c$app_arg\ )
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl15 else
                     \c$case_alt_14\;

  -- src/Contranomy/Instruction.hs:279:22-38
  \c$case_alt_14\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$oldValue1_1\))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => result_4
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl14 else
                     \c$case_alt_15\;

  -- src/Contranomy/Core/CSR.hs:(114,17)-(117,21)
  \c$app_arg\ <= \c$app_arg_0\ = ('1');

  -- indexBitVector begin 
  indexbitvector : block
    signal vec_index : integer range 0 to 32-1;
  begin
    vec_index <= to_integer(to_signed(3,64))
    -- pragma translate_off
                 mod 32
    -- pragma translate_on
                 ;

    \c$app_arg_0\ <= result_4(vec_index);
  end block;
  -- indexBitVector end

  -- src/Contranomy/Core/CSR.hs:(114,17)-(117,21)
  \c$app_arg_1\ <= \c$app_arg_2\ = ('1');

  -- indexBitVector begin 
  indexbitvector_0 : block
    signal vec_index_0 : integer range 0 to 32-1;
  begin
    vec_index_0 <= to_integer(to_signed(7,64))
    -- pragma translate_off
                 mod 32
    -- pragma translate_on
                 ;

    \c$app_arg_2\ <= result_4(vec_index_0);
  end block;
  -- indexBitVector end

  -- indexBitVector begin 
  indexbitvector_1 : block
    signal vec_index_1 : integer range 0 to 32-1;
  begin
    vec_index_1 <= to_integer(to_signed(11,64))
    -- pragma translate_off
                 mod 32
    -- pragma translate_on
                 ;

    \c$app_arg_3\ <= result_4(vec_index_1);
  end block;
  -- indexBitVector end

  -- src/Contranomy/Instruction.hs:286:20-36
  \c$case_alt_15\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$arg\.machinestate_sel4_mscratch))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => result_4
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl13 else
                     \c$case_alt_16\;

  -- src/Contranomy/Instruction.hs:287:20-36
  \c$case_alt_16\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$oldValue1_2\))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => result_4(31 downto 2)
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl12 else
                     \c$case_alt_17\;

  -- src/Contranomy/Instruction.hs:288:20-36
  \c$case_alt_17\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$oldValue1_3\))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => ( mcause_sel0_interrupt => \c$app_arg_4\ = ('1')
                     , mcause_sel1_code => std_logic_vector(resize(unsigned(result_4),4)) )
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl11 else
                     \c$case_alt_18\;

  -- src/Contranomy/Instruction.hs:289:20-36
  \c$case_alt_18\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$arg\.machinestate_sel6_mtval))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => result_4
                     , machinestate_sel7_irqmask => \c$arg\.machinestate_sel7_irqmask ) ) when lvl10 else
                     \c$case_alt_19\;

  -- indexBitVector begin 
  indexbitvector_2 : block
    signal vec_index_2 : integer range 0 to 32-1;
  begin
    vec_index_2 <= to_integer(to_signed(31,64))
    -- pragma translate_off
                 mod 32
    -- pragma translate_on
                 ;

    \c$app_arg_4\ <= result_4(vec_index_2);
  end block;
  -- indexBitVector end

  -- src/Contranomy/Instruction.hs:297:22-38
  \c$case_alt_19\ <= ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("1" & (\c$arg\.machinestate_sel7_irqmask))
                     , tup2_1_sel1_std_logic_vector => result_4 )
                     , tup2_2_sel1_machinestate => ( machinestate_sel0_mstatus => \c$arg\.machinestate_sel0_mstatus
                     , machinestate_sel1_mcause => \c$arg\.machinestate_sel1_mcause
                     , machinestate_sel2_mtvec => \c$arg\.machinestate_sel2_mtvec
                     , machinestate_sel3_mie => \c$arg\.machinestate_sel3_mie
                     , machinestate_sel4_mscratch => \c$arg\.machinestate_sel4_mscratch
                     , machinestate_sel5_mepc => \c$arg\.machinestate_sel5_mepc
                     , machinestate_sel6_mtval => \c$arg\.machinestate_sel6_mtval
                     , machinestate_sel7_irqmask => result_4 ) ) when lvl9 else
                     \c$case_alt_20\;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl9 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"330");

  -- src/Contranomy/Instruction.hs:298:22-38
  \c$case_alt_20\ <= ( tup2_2_sel0_tup2_1 => lvl7
                     , tup2_2_sel1_machinestate => \c$arg\ ) when lvl8 else
                     ( tup2_2_sel0_tup2_1 => ( tup2_1_sel0_maybe_0 => std_logic_vector'("0" & "--------------------------------")
                     , tup2_1_sel1_std_logic_vector => std_logic_vector'(0 to 31 => '-') )
                     , tup2_2_sel1_machinestate => \c$arg\ );

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl8 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"360");

  lvl7 <= ( tup2_1_sel0_maybe_0 => lvl5
          , tup2_1_sel1_std_logic_vector => result_3 );

  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  result_3 <= \c$case_alt_23\ when std_match("01", \csrType1\) else
              \c$case_alt_22\ when std_match("10", \csrType1\) else
              \c$case_alt_21\ when std_match("11", \csrType1\) else
              \externalInterrupt\;

  -- src/Contranomy/Core/CSR.hs:171:3-56
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_21\ <= \externalInterrupt\ when "0",
                       \externalInterrupt\ and (not x_1) when others;

  -- src/Contranomy/Core/CSR.hs:168:3-41
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_22\ <= \externalInterrupt\ when "0",
                       \externalInterrupt\ or x_1 when others;

  -- src/Contranomy/Core/CSR.hs:165:3-29
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_23\ <= \externalInterrupt\ when "0",
                       x_1 when others;

  x_1 <= \c$writeValue1_case_alt\(31 downto 0);

  -- src/Contranomy/Core/CSR.hs:151:16-49
  lvl5 <= std_logic_vector'("1" & (\externalInterrupt\));

  -- src/Contranomy/Core/CSR.hs:108:24-47
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue_app_arg\ <= std_logic_vector'(x"00000008") when \softwareInterrupt\ else
                          std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/CSR.hs:107:24-44
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue_app_arg_0\ <= std_logic_vector'(x"00000080") when \timerInterrupt\ else
                            std_logic_vector'(x"00000000");

  \c$oldValue_app_arg_1_selection_res\ <= \externalInterrupt\ /= std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/CSR.hs:106:24-55
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue_app_arg_1\ <= std_logic_vector'(x"00000800") when \c$oldValue_app_arg_1_selection_res\ else
                            std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/CSR.hs:(164,1)-(173,32)
  result_4 <= \c$case_alt_26\ when std_match("01", \csrType1\) else
              \c$case_alt_25\ when std_match("10", \csrType1\) else
              \c$case_alt_24\ when std_match("11", \csrType1\) else
              \c$csrWriteOut_app_arg\;

  -- src/Contranomy/Core/CSR.hs:171:3-56
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_24\ <= \c$csrWriteOut_app_arg\ when "0",
                       \c$csrWriteOut_app_arg\ and (not x_2) when others;

  -- src/Contranomy/Core/CSR.hs:168:3-41
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_25\ <= \c$csrWriteOut_app_arg\ when "0",
                       \c$csrWriteOut_app_arg\ or x_2 when others;

  -- src/Contranomy/Core/CSR.hs:165:3-29
  with (\c$writeValue1_case_alt\(32 downto 32)) select
    \c$case_alt_26\ <= \c$csrWriteOut_app_arg\ when "0",
                       x_2 when others;

  x_2 <= \c$writeValue1_case_alt\(31 downto 0);

  \c$csrWriteOut_app_arg\ <= \oldValue1\ when lvl18 else
                             \c$csrWriteOut_case_alt\;

  -- src/Contranomy/Core/CSR.hs:96:13-49
  -- src/Contranomy/Core/CSR.hs:96:24-49
  \oldValue1\ <= \c$oldValue1_app_arg_0\ or \c$oldValue1_app_arg\;

  \c$oldValue1_app_arg_selection\ <= \c$arg\.machinestate_sel0_mstatus.mstatus_sel0_mie;

  -- src/Contranomy/Core/CSR.hs:96:40-49
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue1_app_arg\ <= std_logic_vector'(x"00000008") when \c$oldValue1_app_arg_selection\ else
                           std_logic_vector'(x"00000000");

  \c$oldValue1_app_arg_0_selection\ <= \c$arg\.machinestate_sel0_mstatus.mstatus_sel1_mpie;

  -- src/Contranomy/Core/CSR.hs:96:24-34
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue1_app_arg_0\ <= std_logic_vector'(x"00000080") when \c$oldValue1_app_arg_0_selection\ else
                             std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl18 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"300");

  \c$csrWriteOut_case_alt\ <= \c$oldValue1_0\ when lvl15 else
                              \c$csrWriteOut_case_alt_0\;

  -- src/Contranomy/Core/CSR.hs:112:13-67
  -- src/Contranomy/Core/CSR.hs:112:24-67
  \c$oldValue1_0\ <= (\c$oldValue1_app_arg_3\ or \c$oldValue1_app_arg_2\) or \c$oldValue1_app_arg_1\;

  \c$oldValue1_app_arg_1_selection\ <= \c$arg\.machinestate_sel3_mie.mie_sel2_msie;

  -- src/Contranomy/Core/CSR.hs:112:57-67
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue1_app_arg_1\ <= std_logic_vector'(x"00000008") when \c$oldValue1_app_arg_1_selection\ else
                             std_logic_vector'(x"00000000");

  \c$oldValue1_app_arg_2_selection\ <= \c$arg\.machinestate_sel3_mie.mie_sel1_mtie;

  -- src/Contranomy/Core/CSR.hs:112:41-51
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue1_app_arg_2\ <= std_logic_vector'(x"00000080") when \c$oldValue1_app_arg_2_selection\ else
                             std_logic_vector'(x"00000000");

  \c$oldValue1_app_arg_3_selection\ <= \c$arg\.machinestate_sel3_mie.mie_sel0_meie;

  -- src/Contranomy/Core/CSR.hs:112:24-35
  -- src/Contranomy/Clash/Extra.hs:46:1-33
  \c$oldValue1_app_arg_3\ <= std_logic_vector'(x"00000800") when \c$oldValue1_app_arg_3_selection\ else
                             std_logic_vector'(x"00000000");

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl15 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"304");

  \c$csrWriteOut_case_alt_0\ <= \c$oldValue1_1\ when lvl14 else
                                \c$csrWriteOut_case_alt_1\;

  -- src/Contranomy/Core/CSR.hs:120:13-33
  -- src/Contranomy/Core/CSR.hs:120:24-33
  -- src/Contranomy/Core/MachineState.hs:53:1-34
  \c$oldValue1_1\ <= \c$arg\.machinestate_sel2_mtvec;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl14 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"305");

  \c$csrWriteOut_case_alt_1\ <= \c$arg\.machinestate_sel4_mscratch when lvl13 else
                                \c$csrWriteOut_case_alt_2\;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl13 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"340");

  \c$csrWriteOut_case_alt_2\ <= \c$oldValue1_2\ when lvl12 else
                                \c$csrWriteOut_case_alt_3\;

  -- src/Contranomy/Core/CSR.hs:130:13-33
  -- src/Contranomy/Core/CSR.hs:130:24-33
  \c$oldValue1_2\ <= std_logic_vector'(std_logic_vector'(\c$arg\.machinestate_sel5_mepc) & std_logic_vector'(std_logic_vector'("00")));

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl12 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"341");

  \c$csrWriteOut_case_alt_3\ <= \c$oldValue1_3\ when lvl11 else
                                \c$csrWriteOut_case_alt_4\;

  -- src/Contranomy/Core/CSR.hs:135:13-52
  -- src/Contranomy/Core/CSR.hs:135:24-52
  \c$oldValue1_3\ <= std_logic_vector'(std_logic_vector'((std_logic_vector'(std_logic_vector'(\c$oldValue1_app_arg_4\) & std_logic_vector'(std_logic_vector'("000000000000000000000000000"))))) & std_logic_vector'(\c$arg\.machinestate_sel1_mcause.mcause_sel1_code));

  \c$oldValue1_app_arg_4_selection\ <= \c$arg\.machinestate_sel1_mcause.mcause_sel0_interrupt;

  \c$oldValue1_app_arg_4\ <= std_logic_vector'("1") when \c$oldValue1_app_arg_4_selection\ else
                             std_logic_vector'("0");

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl11 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"342");

  \c$csrWriteOut_case_alt_4\ <= \c$arg\.machinestate_sel6_mtval when lvl10 else
                                \c$arg\.machinestate_sel7_irqmask;

  -- src/Contranomy/Core/CSR.hs:94:10-28
  lvl10 <= \c$case_scrut\.decodedinstruction_sel12_imm12i = std_logic_vector'(x"343");

  \c$writeValue1_case_alt\ <= std_logic_vector'("1" & (\writeValue0\)) when std_match("01", \csrType1\) else
                              \c$writeValue1_case_alt_0\;

  \csrType1\ <= ds17.tup2_9_sel1_csrtype;

  \c$writeValue1_case_alt_0_selection_res\ <= \c$writeValue1_app_arg\ = (std_logic_vector(to_unsigned(0,5)));

  -- src/Contranomy/Core/CSR.hs:91:15-23
  -- src/Contranomy/Instruction.hs:128:3-23
  -- src/Contranomy/Instruction.hs:123:1-28
  \c$writeValue1_case_alt_0\ <= std_logic_vector'("0" & "--------------------------------") when \c$writeValue1_case_alt_0_selection_res\ else
                                std_logic_vector'("1" & (\writeValue0\));

  \writeValue0\ <= ds17.tup2_9_sel0_std_logic_vector;

  -- src/Contranomy/Core/CSR.hs:(85,33)-(87,47)
  -- src/Contranomy/Core/CSR.hs:83:9-39
  -- src/Contranomy/Core/CSR.hs:83:19-39
  -- src/Contranomy/Instruction.hs:324:1-26
  ds17 <= ( tup2_9_sel0_std_logic_vector => \rs1Val\
          , tup2_9_sel1_csrtype => t_0 ) when std_match("0--", \c$ds17_case_scrut\) else
          ( tup2_9_sel0_std_logic_vector => std_logic_vector'(std_logic_vector'(std_logic_vector'("000000000000000000000000000")) & std_logic_vector'(\c$writeValue1_app_arg\))
          , tup2_9_sel1_csrtype => t );

  t <= \c$ds17_case_scrut\(1 downto 0);

  t_0 <= \c$ds17_case_scrut\(1 downto 0);

  \c$ds17_case_scrut\ <= \c$case_scrut\.decodedinstruction_sel15_func3;

  \c$writeValue1_app_arg\ <= \c$case_scrut\.decodedinstruction_sel2_rs1;

  decodeinstruction_0_ccase_scrut : entity decodeinstruction_0
    port map
      ( result => \c$case_scrut\
      , w      => instruction );

  result <= \c$case_alt\;


end;


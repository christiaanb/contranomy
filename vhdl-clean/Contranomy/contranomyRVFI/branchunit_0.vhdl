-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomyrvfi_types.all;

entity branchunit_0 is
  port(instruction : in std_logic_vector(31 downto 0);
       \rs1Val\    : in std_logic_vector(31 downto 0);
       \rs2Val\    : in std_logic_vector(31 downto 0);
       pc          : in std_logic_vector(29 downto 0);
       result      : out contranomyrvfi_types.tup2_5);
end;

architecture structural of branchunit_0 is
  signal result_0                                                : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Instruction.hs:180:15-30
  signal \c$case_alt\                                            : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:(39,5)-(52,21)
  signal result_1                                                : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:45:19-34
  signal \c$case_alt_0\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:43:19-33
  signal \c$case_alt_1\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:44:18-62
  signal \c$case_alt_2\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:42:18-61
  signal \c$case_alt_3\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:41:18-33
  signal \c$case_alt_4\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:40:18-33
  signal \c$case_alt_5\                                          : contranomyrvfi_types.tup2_5;
  signal \c$app_arg\                                             : signed(31 downto 0);
  signal \c$app_arg_0\                                           : signed(31 downto 0);
  -- src/Contranomy/Core/Branch.hs:48:26-76
  signal ds17                                                    : contranomyrvfi_types.tup2_5;
  signal x                                                       : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:181:16-31
  signal \c$case_alt_6\                                          : contranomyrvfi_types.tup2_5;
  -- src/Contranomy/Core/Branch.hs:55:26-76
  signal ds17_0                                                  : contranomyrvfi_types.tup2_5;
  signal x_0                                                     : std_logic_vector(31 downto 0);
  signal \c$x_app_arg\                                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/Branch.hs:59:24-57
  signal ds17_1                                                  : contranomyrvfi_types.tup2_5;
  signal x_1                                                     : std_logic_vector(31 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut\                   : std_logic_vector(31 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut_app_arg\           : std_logic_vector(11 downto 0);
  signal \c$case_scrut\                                          : contranomyrvfi_types.decodedinstruction;
  signal result_0_selection                                      : std_logic_vector(6 downto 0);
  signal \c$case_alt_selection\                                  : std_logic_vector(6 downto 0);
  signal result_1_selection_res                                  : contranomyrvfi_types.branchcondition;
  signal \c$case_alt_0_selection_res\                            : boolean;
  signal \c$case_alt_1_selection_res\                            : boolean;
  signal \c$case_alt_2_selection_res\                            : boolean;
  signal \c$case_alt_3_selection_res\                            : boolean;
  signal \c$case_alt_4_selection_res\                            : boolean;
  signal \c$case_alt_5_selection_res\                            : boolean;
  signal \c$case_alt_6_selection\                                : std_logic_vector(6 downto 0);
  signal \c$x_app_arg_selection_res\                             : boolean;
  signal \c$bv\                                                  : std_logic_vector(19 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut_selection_res\     : boolean;
  signal \c$$fResizeBitVector_$csignExtendOut_app_arg_selection\ : std_logic_vector(6 downto 0);

begin
  result_0_selection <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (result_0_selection) select
    result_0 <= result_1 when "1100011",
                \c$case_alt\ when others;

  \c$case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:180:15-30
  with (\c$case_alt_selection\) select
    \c$case_alt\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17_0.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17_0.tup2_5_sel1_std_logic_vector_1 ) when "1101111",
                    \c$case_alt_6\ when others;

  result_1_selection_res <= \c$case_scrut\.decodedinstruction_sel15_func3;

  -- src/Contranomy/Core/Branch.hs:(39,5)-(52,21)
  -- src/Contranomy/Core/Branch.hs:(39,17)-(46,20)
  -- src/Contranomy/Instruction.hs:266:1-36
  result_1 <= \c$case_alt_5\ when std_match("000", result_1_selection_res) else
              \c$case_alt_4\ when std_match("001", result_1_selection_res) else
              \c$case_alt_3\ when std_match("100", result_1_selection_res) else
              \c$case_alt_2\ when std_match("101", result_1_selection_res) else
              \c$case_alt_1\ when std_match("110", result_1_selection_res) else
              \c$case_alt_0\ when std_match("111", result_1_selection_res) else
              ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
              , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_0_selection_res\ <= \rs1Val\ >= \rs2Val\;

  -- src/Contranomy/Core/Branch.hs:45:19-34
  \c$case_alt_0\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_0_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_1_selection_res\ <= \rs1Val\ < \rs2Val\;

  -- src/Contranomy/Core/Branch.hs:43:19-33
  \c$case_alt_1\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_1_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_2_selection_res\ <= \c$app_arg_0\ >= \c$app_arg\;

  -- src/Contranomy/Core/Branch.hs:44:18-62
  \c$case_alt_2\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_2_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_3_selection_res\ <= \c$app_arg_0\ < \c$app_arg\;

  -- src/Contranomy/Core/Branch.hs:42:18-61
  \c$case_alt_3\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_3_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_4_selection_res\ <= \rs1Val\ /= \rs2Val\;

  -- src/Contranomy/Core/Branch.hs:41:18-33
  \c$case_alt_4\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_4_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$case_alt_5_selection_res\ <= \rs1Val\ = \rs2Val\;

  -- src/Contranomy/Core/Branch.hs:40:18-33
  \c$case_alt_5\ <= ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(ds17.tup2_5_sel0_std_logic_vector_0))
                    , tup2_5_sel1_std_logic_vector_1 => ds17.tup2_5_sel1_std_logic_vector_1 ) when \c$case_alt_5_selection_res\ else
                    ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                    , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") );

  \c$app_arg\ <= signed(\rs2Val\);

  \c$app_arg_0\ <= signed(\rs1Val\);

  -- src/Contranomy/Core/Branch.hs:48:26-76
  ds17 <= (x(x'high downto 2),x(2-1 downto 0));

  -- src/Contranomy/Core/Branch.hs:48:32-76
  x <= std_logic_vector(shift_left(unsigned(\c$$fResizeBitVector_$csignExtendOut\),to_integer(to_signed(1,64))));

  \c$case_alt_6_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:181:16-31
  with (\c$case_alt_6_selection\) select
    \c$case_alt_6\ <= ( tup2_5_sel0_std_logic_vector_0 => ds17_1.tup2_5_sel0_std_logic_vector_0
                      , tup2_5_sel1_std_logic_vector_1 => ds17_1.tup2_5_sel1_std_logic_vector_1 and std_logic_vector'("10") ) when "1100111",
                      ( tup2_5_sel0_std_logic_vector_0 => std_logic_vector(unsigned(pc) + unsigned(std_logic_vector'("000000000000000000000000000001")))
                      , tup2_5_sel1_std_logic_vector_1 => std_logic_vector'("00") ) when others;

  -- src/Contranomy/Core/Branch.hs:55:26-76
  ds17_0 <= (x_0(x_0'high downto 2),x_0(2-1 downto 0));

  -- src/Contranomy/Core/Branch.hs:55:32-76
  x_0 <= std_logic_vector(shift_left(unsigned(\c$x_app_arg\),to_integer(to_signed(1,64))));

  \c$bv\ <= \c$case_scrut\.decodedinstruction_sel11_imm20j;

  \c$x_app_arg_selection_res\ <= ( \c$bv\(\c$bv\'high) ) = ('0');

  \c$x_app_arg\ <= std_logic_vector'(std_logic_vector'(std_logic_vector'(x"000")) & std_logic_vector'(\c$case_scrut\.decodedinstruction_sel11_imm20j)) when \c$x_app_arg_selection_res\ else
                   std_logic_vector'(std_logic_vector'(std_logic_vector'(x"FFF")) & std_logic_vector'(\c$case_scrut\.decodedinstruction_sel11_imm20j));

  -- src/Contranomy/Core/Branch.hs:59:24-57
  ds17_1 <= (x_1(x_1'high downto 2),x_1(2-1 downto 0));

  -- src/Contranomy/Core/Branch.hs:59:30-57
  x_1 <= std_logic_vector(unsigned(\rs1Val\) + unsigned(\c$$fResizeBitVector_$csignExtendOut\));

  \c$$fResizeBitVector_$csignExtendOut_selection_res\ <= ( \c$$fResizeBitVector_$csignExtendOut_app_arg\(\c$$fResizeBitVector_$csignExtendOut_app_arg\'high) ) = ('0');

  \c$$fResizeBitVector_$csignExtendOut\ <= std_logic_vector'(std_logic_vector'(std_logic_vector'(x"00000")) & std_logic_vector'(\c$$fResizeBitVector_$csignExtendOut_app_arg\)) when \c$$fResizeBitVector_$csignExtendOut_selection_res\ else
                                           std_logic_vector'(std_logic_vector'(std_logic_vector'(x"FFFFF")) & std_logic_vector'(\c$$fResizeBitVector_$csignExtendOut_app_arg\));

  \c$$fResizeBitVector_$csignExtendOut_app_arg_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (\c$$fResizeBitVector_$csignExtendOut_app_arg_selection\) select
    \c$$fResizeBitVector_$csignExtendOut_app_arg\ <= \c$case_scrut\.decodedinstruction_sel14_imm12b when "1100011",
                                                     \c$case_scrut\.decodedinstruction_sel12_imm12i when others;

  decodeinstruction_0_ccase_scrut : entity decodeinstruction_0
    port map
      ( result => \c$case_scrut\
      , w      => instruction );

  result <= result_0;


end;


-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;

entity alu is
  port(instruction : in std_logic_vector(31 downto 0);
       pc          : in std_logic_vector(29 downto 0);
       \rs1Value\  : in std_logic_vector(31 downto 0);
       \rs2Value\  : in std_logic_vector(31 downto 0);
       result      : out std_logic_vector(31 downto 0));
end;

architecture structural of alu is
  -- src/Contranomy/Core/ALU.hs:(54,1)-(94,31)
  signal result_0                                                : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:(90,13)-(92,86)
  signal \c$case_alt\                                            : std_logic_vector(31 downto 0);
  signal result_1                                                : std_logic_vector(0 downto 0);
  signal b                                                       : boolean;
  signal result_2                                                : std_logic_vector(0 downto 0);
  -- src/Contranomy/Core/ALU.hs:87:29-76
  signal b_0                                                     : boolean;
  signal \c$b_app_arg\                                           : signed(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:(60,3)-(65,31)
  signal \aluArg1\                                               : std_logic_vector(31 downto 0);
  signal result_3                                                : signed(63 downto 0);
  signal wild                                                    : signed(63 downto 0);
  signal \c$wild_app_arg\                                        : signed(63 downto 0);
  signal x                                                       : std_logic_vector(63 downto 0);
  -- src/Contranomy/Core/ALU.hs:(66,3)-(75,41)
  signal \aluArg2\                                               : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:179:17-32
  signal \c$aluArg2_case_alt\                                    : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:(66,3)-(75,41)
  signal \c$aluArg2_case_alt_0\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:180:15-30
  signal \c$aluArg2_case_alt_1\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:181:16-31
  signal \c$aluArg2_case_alt_2\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:186:14-29
  signal \c$aluArg2_case_alt_3\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:(71,25)-(73,39)
  signal \c$aluArg2_case_alt_4\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:72:33-37
  signal \c$aluArg2_case_alt_5\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:179:17-32
  signal \c$aluArg1_case_alt\                                    : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:180:15-30
  signal \c$aluArg1_case_alt_0\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:181:16-31
  signal \c$aluArg1_case_alt_1\                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/ALU.hs:(60,3)-(65,31)
  signal \c$aluArg1_case_alt_2\                                  : std_logic_vector(31 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut\                   : std_logic_vector(31 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut_app_arg\           : std_logic_vector(11 downto 0);
  signal \c$alu_$jOut_case_alt\                                  : contranomy_types.iop;
  signal \c$alu_$jOut_app_arg\                                   : contranomy_types.iop;
  signal \c$case_scrut\                                          : contranomy_types.decodedinstruction;
  signal \c$case_alt_selection\                                  : contranomy_types.shiftright;
  signal \c$aluArg1_selection\                                   : std_logic_vector(6 downto 0);
  signal \c$aluArg2_selection\                                   : std_logic_vector(6 downto 0);
  signal \c$aluArg2_case_alt_selection\                          : std_logic_vector(6 downto 0);
  signal \c$aluArg2_case_alt_1_selection\                        : std_logic_vector(6 downto 0);
  signal \c$aluArg2_case_alt_2_selection\                        : std_logic_vector(6 downto 0);
  signal \c$aluArg2_case_alt_3_selection\                        : std_logic_vector(6 downto 0);
  signal \c$aluArg2_case_alt_5_selection\                        : boolean;
  signal \c$aluArg1_case_alt_selection\                          : std_logic_vector(6 downto 0);
  signal \c$aluArg1_case_alt_0_selection\                        : std_logic_vector(6 downto 0);
  signal \c$aluArg1_case_alt_1_selection\                        : std_logic_vector(6 downto 0);
  signal \c$$fResizeBitVector_$csignExtendOut_selection_res\     : boolean;
  signal \c$$fResizeBitVector_$csignExtendOut_app_arg_selection\ : std_logic_vector(6 downto 0);
  signal \c$alu_$jOut_case_alt_selection\                        : std_logic_vector(6 downto 0);
  signal \c$alu_$jOut_app_arg_selection\                         : std_logic_vector(6 downto 0);

begin
  -- src/Contranomy/Core/ALU.hs:(54,1)-(94,31)
  -- src/Contranomy/Core/ALU.hs:(84,15)-(94,31)
  result_0 <= std_logic_vector(unsigned(\aluArg1\) + unsigned(\aluArg2\)) when std_match("000", \c$alu_$jOut_app_arg\) else
              std_logic_vector(shift_left(unsigned(\aluArg1\),to_integer(result_3))) when std_match("001", \c$alu_$jOut_app_arg\) else
              std_logic_vector'(std_logic_vector'(std_logic_vector'("0000000000000000000000000000000")) & std_logic_vector'(result_2)) when std_match("010", \c$alu_$jOut_app_arg\) else
              std_logic_vector'(std_logic_vector'(std_logic_vector'("0000000000000000000000000000000")) & std_logic_vector'(result_1)) when std_match("011", \c$alu_$jOut_app_arg\) else
              \aluArg1\ xor \aluArg2\ when std_match("100", \c$alu_$jOut_app_arg\) else
              \c$case_alt\ when std_match("101", \c$alu_$jOut_app_arg\) else
              \aluArg1\ or \aluArg2\ when std_match("110", \c$alu_$jOut_app_arg\) else
              \aluArg1\ and \aluArg2\;

  \c$case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel5_srla;

  -- src/Contranomy/Core/ALU.hs:(90,13)-(92,86)
  \c$case_alt\ <= std_logic_vector(shift_right(unsigned(\aluArg1\),to_integer(result_3))) when std_match("0", \c$case_alt_selection\) else
                  std_logic_vector((shift_right(\c$b_app_arg\,to_integer(result_3))));

  result_1 <= std_logic_vector'("1") when b else
              std_logic_vector'("0");

  -- src/Contranomy/Core/ALU.hs:88:29-47
  b <= \aluArg1\ < \aluArg2\;

  result_2 <= std_logic_vector'("1") when b_0 else
              std_logic_vector'("0");

  -- src/Contranomy/Core/ALU.hs:87:29-76
  b_0 <= \c$b_app_arg\ < (signed(\aluArg2\));

  \c$b_app_arg\ <= signed(\aluArg1\);

  \c$aluArg1_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Core/ALU.hs:(60,3)-(65,31)
  -- src/Contranomy/Core/ALU.hs:(60,13)-(65,31)
  -- src/Contranomy/Instruction.hs:178:15-30
  with (\c$aluArg1_selection\) select
    \aluArg1\ <= std_logic_vector'(x"00000000") when "0110111",
                 \c$aluArg1_case_alt\ when others;

  result_3 <= wild;

  wild <= \c$wild_app_arg\;

  \c$wild_app_arg\ <= signed(std_logic_vector(resize(unsigned(x),64)));

  x <= std_logic_vector'(std_logic_vector'(std_logic_vector'("00000000000000000000000000000000000000000000000000000000000")) & std_logic_vector'((\aluArg2\(4 downto 0))));

  \c$aluArg2_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Core/ALU.hs:(66,3)-(75,41)
  -- src/Contranomy/Core/ALU.hs:(66,13)-(75,41)
  -- src/Contranomy/Instruction.hs:178:15-30
  with (\c$aluArg2_selection\) select
    \aluArg2\ <= \c$aluArg2_case_alt_0\ when "0110111",
                 \c$aluArg2_case_alt\ when others;

  \c$aluArg2_case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:179:17-32
  -- src/Contranomy/Core/ALU.hs:66:18-23
  with (\c$aluArg2_case_alt_selection\) select
    \c$aluArg2_case_alt\ <= \c$aluArg2_case_alt_0\ when "0010111",
                            \c$aluArg2_case_alt_1\ when others;

  -- src/Contranomy/Core/ALU.hs:68:25-36
  \c$aluArg2_case_alt_0\ <= std_logic_vector'(std_logic_vector'(\c$case_scrut\.decodedinstruction_sel10_imm20u) & std_logic_vector'(std_logic_vector'(x"000")));

  \c$aluArg2_case_alt_1_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:180:15-30
  -- src/Contranomy/Core/ALU.hs:66:18-23
  with (\c$aluArg2_case_alt_1_selection\) select
    \c$aluArg2_case_alt_1\ <= std_logic_vector'(x"00000004") when "1101111",
                              \c$aluArg2_case_alt_2\ when others;

  \c$aluArg2_case_alt_2_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:181:16-31
  -- src/Contranomy/Core/ALU.hs:66:18-23
  with (\c$aluArg2_case_alt_2_selection\) select
    \c$aluArg2_case_alt_2\ <= std_logic_vector'(x"00000004") when "1100111",
                              \c$aluArg2_case_alt_3\ when others;

  \c$aluArg2_case_alt_3_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:186:14-29
  -- src/Contranomy/Core/ALU.hs:66:18-23
  with (\c$aluArg2_case_alt_3_selection\) select
    \c$aluArg2_case_alt_3\ <= \c$aluArg2_case_alt_4\ when "0110011",
                              \c$$fResizeBitVector_$csignExtendOut\ when others;

  -- src/Contranomy/Core/ALU.hs:(71,25)-(73,39)
  \c$aluArg2_case_alt_4\ <= \c$aluArg2_case_alt_5\ when std_match("000", \c$alu_$jOut_app_arg\) else
                            \rs2Value\;

  \c$aluArg2_case_alt_5_selection\ <= \c$case_scrut\.decodedinstruction_sel7_issub;

  -- src/Contranomy/Core/ALU.hs:72:33-37
  \c$aluArg2_case_alt_5\ <= std_logic_vector(-(signed(\rs2Value\))) when \c$aluArg2_case_alt_5_selection\ else
                            \rs2Value\;

  \c$aluArg1_case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:179:17-32
  -- src/Contranomy/Core/ALU.hs:60:18-23
  with (\c$aluArg1_case_alt_selection\) select
    \c$aluArg1_case_alt\ <= \c$aluArg1_case_alt_2\ when "0010111",
                            \c$aluArg1_case_alt_0\ when others;

  \c$aluArg1_case_alt_0_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:180:15-30
  -- src/Contranomy/Core/ALU.hs:60:18-23
  with (\c$aluArg1_case_alt_0_selection\) select
    \c$aluArg1_case_alt_0\ <= \c$aluArg1_case_alt_2\ when "1101111",
                              \c$aluArg1_case_alt_1\ when others;

  \c$aluArg1_case_alt_1_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  -- src/Contranomy/Instruction.hs:181:16-31
  -- src/Contranomy/Core/ALU.hs:60:18-23
  with (\c$aluArg1_case_alt_1_selection\) select
    \c$aluArg1_case_alt_1\ <= \c$aluArg1_case_alt_2\ when "1100111",
                              \rs1Value\ when others;

  -- src/Contranomy/Core/ALU.hs:64:24-31
  \c$aluArg1_case_alt_2\ <= std_logic_vector'(std_logic_vector'(pc) & std_logic_vector'(std_logic_vector'("00")));

  \c$$fResizeBitVector_$csignExtendOut_selection_res\ <= ( \c$$fResizeBitVector_$csignExtendOut_app_arg\(\c$$fResizeBitVector_$csignExtendOut_app_arg\'high) ) = ('0');

  \c$$fResizeBitVector_$csignExtendOut\ <= std_logic_vector'(std_logic_vector'(std_logic_vector'(x"00000")) & std_logic_vector'(\c$$fResizeBitVector_$csignExtendOut_app_arg\)) when \c$$fResizeBitVector_$csignExtendOut_selection_res\ else
                                           std_logic_vector'(std_logic_vector'(std_logic_vector'(x"FFFFF")) & std_logic_vector'(\c$$fResizeBitVector_$csignExtendOut_app_arg\));

  \c$$fResizeBitVector_$csignExtendOut_app_arg_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (\c$$fResizeBitVector_$csignExtendOut_app_arg_selection\) select
    \c$$fResizeBitVector_$csignExtendOut_app_arg\ <= \c$case_scrut\.decodedinstruction_sel13_imm12s when "0100011",
                                                     \c$case_scrut\.decodedinstruction_sel12_imm12i when others;

  \c$alu_$jOut_case_alt_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (\c$alu_$jOut_case_alt_selection\) select
    \c$alu_$jOut_case_alt\ <= \c$case_scrut\.decodedinstruction_sel4_iop when "0010011",
                              std_logic_vector(to_unsigned(0,3)) when others;

  \c$alu_$jOut_app_arg_selection\ <= \c$case_scrut\.decodedinstruction_sel0_opcode;

  with (\c$alu_$jOut_app_arg_selection\) select
    \c$alu_$jOut_app_arg\ <= \c$case_scrut\.decodedinstruction_sel4_iop when "0110011",
                             \c$alu_$jOut_case_alt\ when others;

  decodeinstruction_ccase_scrut : entity decodeinstruction
    port map
      ( result => \c$case_scrut\
      , w      => instruction );

  result <= result_0;


end;


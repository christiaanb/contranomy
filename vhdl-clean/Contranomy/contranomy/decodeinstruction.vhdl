-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;

entity decodeinstruction is
  port(w      : in std_logic_vector(31 downto 0);
       result : out contranomy_types.decodedinstruction);
end;

architecture structural of decodeinstruction is
  -- src/Contranomy/Core/Decode.hs:(70,14)-(120,16)
  signal \c$app_arg\                   : boolean;
  -- src/Contranomy/Instruction.hs:179:17-32
  signal \c$case_alt\                  : boolean;
  -- src/Contranomy/Instruction.hs:180:15-30
  signal \c$case_alt_0\                : boolean;
  -- src/Contranomy/Instruction.hs:181:16-31
  signal \c$case_alt_1\                : boolean;
  -- src/Contranomy/Instruction.hs:182:18-33
  signal \c$case_alt_2\                : boolean;
  -- src/Contranomy/Core/Decode.hs:(75,17)-(82,18)
  signal \c$case_alt_3\                : boolean;
  -- src/Contranomy/Instruction.hs:183:16-31
  signal \c$case_alt_4\                : boolean;
  -- src/Contranomy/Core/Decode.hs:(83,15)-(87,18)
  signal \c$case_alt_5\                : boolean;
  -- src/Contranomy/Instruction.hs:184:17-32
  signal \c$case_alt_6\                : boolean;
  -- src/Contranomy/Core/Decode.hs:(88,16)-(92,18)
  signal \c$case_alt_7\                : boolean;
  signal \c$case_alt_8\                : boolean;
  signal \c$case_alt_9\                : boolean;
  signal \c$case_scrut\                : contranomy_types.loadstorewidth;
  -- src/Contranomy/Instruction.hs:185:18-33
  signal result_0                      : boolean;
  -- src/Contranomy/Core/Decode.hs:(93,17)-(96,17)
  signal \c$case_alt_10\               : boolean;
  -- src/Contranomy/Instruction.hs:186:14-29
  signal \c$case_alt_11\               : boolean;
  -- src/Contranomy/Core/Decode.hs:(97,13)-(107,25)
  signal \c$case_alt_12\               : boolean;
  -- src/Contranomy/Instruction.hs:187:20-35
  signal \c$case_alt_13\               : boolean;
  signal \c$case_alt_14\               : boolean;
  -- src/Contranomy/Core/Decode.hs:(104,15)-(105,33)
  signal \c$case_alt_15\               : boolean;
  -- src/Contranomy/Instruction.hs:188:18-33
  signal \c$case_alt_16\               : boolean;
  -- src/Contranomy/Core/Decode.hs:123:19-33
  signal v1                            : std_logic_vector(6 downto 0);
  -- src/Contranomy/Core/Decode.hs:(109,17)-(119,20)
  signal \c$case_alt_17\               : boolean;
  -- src/Contranomy/Core/Decode.hs:(115,14)-(119,20)
  signal \c$case_alt_18\               : boolean;
  -- src/Contranomy/Core/Decode.hs:126:3-26
  signal result_1                      : boolean;
  -- src/Contranomy/Instruction.hs:331:18-27
  signal \c$case_alt_19\               : boolean;
  signal \c$app_arg_0\                 : std_logic_vector(6 downto 0);
  -- src/Contranomy/Core/Decode.hs:63:14-28
  signal \c$app_arg_1\                 : std_logic_vector(11 downto 0);
  signal \c$app_arg_2\                 : std_logic_vector(0 downto 0);
  signal bv                            : std_logic_vector(0 downto 0);
  -- src/Contranomy/Instruction.hs:194:1-30
  signal \c$app_arg_3\                 : std_logic_vector(0 downto 0);
  -- src/Contranomy/Core/Decode.hs:51:14-37
  signal \c$app_arg_4\                 : contranomy_types.iop;
  -- src/Contranomy/Core/Decode.hs:124:3-26
  signal func1                         : std_logic_vector(2 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal \c$app_arg_5\                 : std_logic_vector(4 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal \c$app_arg_6\                 : std_logic_vector(4 downto 0);
  signal \c$case_alt_3_selection_res\  : contranomy_types.branchcondition;
  signal \c$case_alt_8_selection\      : contranomy_types.sign_r;
  signal \c$case_alt_9_selection\      : contranomy_types.sign_r;
  signal \c$case_alt_18_selection_res\ : contranomy_types.csrtype;

begin
  -- src/Contranomy/Core/Decode.hs:(45,1)-(126,26)
  result <= ( decodedinstruction_sel0_opcode => v1
            , decodedinstruction_sel1_rd => \c$app_arg_6\
            , decodedinstruction_sel2_rs1 => (w(19 downto 15))
            , decodedinstruction_sel3_rs2 => \c$app_arg_5\
            , decodedinstruction_sel4_iop => \c$app_arg_4\
            , decodedinstruction_sel5_srla => \c$app_arg_3\
            , decodedinstruction_sel6_shamt => \c$app_arg_5\
            , decodedinstruction_sel7_issub => \c$app_arg_3\ = std_logic_vector'("1")
            , decodedinstruction_sel8_ism => bv = std_logic_vector'("1")
            , decodedinstruction_sel9_mop => func1
            , decodedinstruction_sel10_imm20u => w(31 downto 12)
            , decodedinstruction_sel11_imm20j => std_logic_vector'(std_logic_vector'((std_logic_vector'(std_logic_vector'((std_logic_vector'(std_logic_vector'(\c$app_arg_2\) & std_logic_vector'((w(19 downto 12)))))) & std_logic_vector'((w(20 downto 20)))))) & std_logic_vector'((w(30 downto 21))))
            , decodedinstruction_sel12_imm12i => \c$app_arg_1\
            , decodedinstruction_sel13_imm12s => std_logic_vector'(std_logic_vector'(\c$app_arg_0\) & std_logic_vector'(\c$app_arg_6\))
            , decodedinstruction_sel14_imm12b => std_logic_vector'(std_logic_vector'((std_logic_vector'(std_logic_vector'((std_logic_vector'(std_logic_vector'(\c$app_arg_2\) & std_logic_vector'((w(7 downto 7)))))) & std_logic_vector'((w(30 downto 25)))))) & std_logic_vector'((w(11 downto 8))))
            , decodedinstruction_sel15_func3 => func1
            , decodedinstruction_sel16_legal => \c$app_arg\ );

  -- src/Contranomy/Core/Decode.hs:(70,14)-(120,16)
  -- src/Contranomy/Instruction.hs:178:15-30
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$app_arg\ <= true when "0110111",
                   \c$case_alt\ when others;

  -- src/Contranomy/Instruction.hs:179:17-32
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt\ <= true when "0010111",
                    \c$case_alt_0\ when others;

  -- src/Contranomy/Instruction.hs:180:15-30
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_0\ <= true when "1101111",
                      \c$case_alt_1\ when others;

  -- src/Contranomy/Instruction.hs:181:16-31
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_1\ <= func1 = std_logic_vector'("000") when "1100111",
                      \c$case_alt_2\ when others;

  -- src/Contranomy/Instruction.hs:182:18-33
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_2\ <= \c$case_alt_3\ when "1100011",
                      \c$case_alt_4\ when others;

  \c$case_alt_3_selection_res\ <= func1;

  -- src/Contranomy/Core/Decode.hs:(75,17)-(82,18)
  -- src/Contranomy/Instruction.hs:266:1-36
  \c$case_alt_3\ <= true when std_match("000", \c$case_alt_3_selection_res\) else
                    true when std_match("001", \c$case_alt_3_selection_res\) else
                    true when std_match("100", \c$case_alt_3_selection_res\) else
                    true when std_match("101", \c$case_alt_3_selection_res\) else
                    true when std_match("110", \c$case_alt_3_selection_res\) else
                    true when std_match("111", \c$case_alt_3_selection_res\) else
                    false;

  -- src/Contranomy/Instruction.hs:183:16-31
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_4\ <= \c$case_alt_5\ when "0000011",
                      \c$case_alt_6\ when others;

  -- src/Contranomy/Core/Decode.hs:(83,15)-(87,18)
  -- src/Contranomy/Instruction.hs:243:1-34
  \c$case_alt_5\ <= true when std_match("-00", \c$case_scrut\) else
                    true when std_match("-01", \c$case_scrut\) else
                    true when std_match("010", \c$case_scrut\) else
                    false;

  -- src/Contranomy/Instruction.hs:184:17-32
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_6\ <= \c$case_alt_7\ when "0100011",
                      result_0 when others;

  -- src/Contranomy/Core/Decode.hs:(88,16)-(92,18)
  -- src/Contranomy/Instruction.hs:243:1-34
  \c$case_alt_7\ <= \c$case_alt_9\ when std_match("-00", \c$case_scrut\) else
                    \c$case_alt_8\ when std_match("-01", \c$case_scrut\) else
                    true when std_match("010", \c$case_scrut\) else
                    false;

  \c$case_alt_8_selection\ <= \c$case_scrut\(2 downto 2);

  \c$case_alt_8\ <= true when std_match("0", \c$case_alt_8_selection\) else
                    false;

  \c$case_alt_9_selection\ <= \c$case_scrut\(2 downto 2);

  \c$case_alt_9\ <= true when std_match("0", \c$case_alt_9_selection\) else
                    false;

  \c$case_scrut\ <= func1;

  -- src/Contranomy/Instruction.hs:185:18-33
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    result_0 <= \c$case_alt_10\ when "0010011",
                \c$case_alt_11\ when others;

  -- src/Contranomy/Core/Decode.hs:(93,17)-(96,17)
  -- src/Contranomy/Instruction.hs:206:1-23
  \c$case_alt_10\ <= \c$case_alt_15\ when std_match("101", \c$app_arg_4\) else
                     true;

  -- src/Contranomy/Instruction.hs:186:14-29
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_11\ <= \c$case_alt_12\ when "0110011",
                       \c$case_alt_13\ when others;

  -- src/Contranomy/Core/Decode.hs:(97,13)-(107,25)
  -- src/Contranomy/Instruction.hs:206:1-23
  \c$case_alt_12\ <= \c$case_alt_14\ when std_match("001", \c$app_arg_4\) else
                     \c$case_alt_14\ when std_match("010", \c$app_arg_4\) else
                     \c$case_alt_14\ when std_match("011", \c$app_arg_4\) else
                     \c$case_alt_14\ when std_match("100", \c$app_arg_4\) else
                     \c$case_alt_14\ when std_match("110", \c$app_arg_4\) else
                     \c$case_alt_14\ when std_match("111", \c$app_arg_4\) else
                     \c$case_alt_15\;

  -- src/Contranomy/Instruction.hs:187:20-35
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_13\ <= func1 = std_logic_vector'("001") when "0001111",
                       \c$case_alt_16\ when others;

  -- src/Contranomy/Core/Decode.hs:100:16-25
  \c$case_alt_14\ <= \c$app_arg_0\ = std_logic_vector'("0000000");

  -- src/Contranomy/Core/Decode.hs:(104,15)-(105,33)
  -- src/Contranomy/Core/Decode.hs:(98,16)-(99,34)
  with (\c$app_arg_0\) select
    \c$case_alt_15\ <= true when "0000000",
                       \c$app_arg_0\ = std_logic_vector'("0100000") when others;

  -- src/Contranomy/Instruction.hs:188:18-33
  -- src/Contranomy/Core/Decode.hs:70:19-24
  -- src/Contranomy/Core/Decode.hs:123:3-33
  -- src/Contranomy/Core/Decode.hs:123:12-33
  -- src/Contranomy/Instruction.hs:175:20-26
  with (v1) select
    \c$case_alt_16\ <= \c$case_alt_17\ when "1110011",
                       false when others;

  -- src/Contranomy/Core/Decode.hs:123:19-33
  v1 <= w(6 downto 0);

  -- src/Contranomy/Core/Decode.hs:(109,17)-(119,20)
  with (func1) select
    \c$case_alt_17\ <= result_1 when "000",
                       \c$case_alt_18\ when others;

  \c$case_alt_18_selection_res\ <= (func1(1 downto 0));

  -- src/Contranomy/Core/Decode.hs:(115,14)-(119,20)
  -- src/Contranomy/Instruction.hs:313:1-28
  \c$case_alt_18\ <= false when std_match("00", \c$case_alt_18_selection_res\) else
                     true;

  -- src/Contranomy/Core/Decode.hs:126:3-26
  -- src/Contranomy/Core/Decode.hs:126:12-26
  -- src/Contranomy/Core/Decode.hs:(110,14)-(114,20)
  -- src/Contranomy/Instruction.hs:330:17-26
  with (\c$app_arg_1\) select
    result_1 <= true when x"000",
                \c$case_alt_19\ when others;

  -- src/Contranomy/Instruction.hs:331:18-27
  with (\c$app_arg_1\) select
    \c$case_alt_19\ <= true when x"001",
                       \c$app_arg_1\ = std_logic_vector'(x"302") when others;

  \c$app_arg_0\ <= w(31 downto 25);

  -- src/Contranomy/Core/Decode.hs:63:14-28
  \c$app_arg_1\ <= w(31 downto 20);

  \c$app_arg_2\ <= w(31 downto 31);

  bv <= w(25 downto 25);

  -- src/Contranomy/Instruction.hs:194:1-30
  -- src/Contranomy/Core/Decode.hs:52:21-37
  \c$app_arg_3\ <= w(30 downto 30);

  -- src/Contranomy/Core/Decode.hs:51:14-37
  -- src/Contranomy/Instruction.hs:206:1-23
  \c$app_arg_4\ <= func1;

  -- src/Contranomy/Core/Decode.hs:124:3-26
  -- src/Contranomy/Core/Decode.hs:124:12-26
  func1 <= w(14 downto 12);

  -- src/Contranomy/Instruction.hs:123:1-28
  -- src/Contranomy/Core/Decode.hs:50:21-37
  \c$app_arg_5\ <= w(24 downto 20);

  -- src/Contranomy/Instruction.hs:123:1-28
  -- src/Contranomy/Core/Decode.hs:48:21-36
  \c$app_arg_6\ <= w(11 downto 7);


end;


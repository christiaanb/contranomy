-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomy_types.all;

entity loadstoreunit is
  port
    ( instruction         : in std_logic_vector(31 downto 0)
    ; instructionFault    : in boolean
    ; addr                : in std_logic_vector(31 downto 0)
    ; toStore             : in std_logic_vector(31 downto 0)
    ; dBusS2M             : in contranomy_types.wishbones2m

    ; dBusM2S             : out contranomy_types.wishbonem2s
    ; loadVal             : out contranomy_types.maybe_machineword
    ; dataAccessFault     : out contranomy_types.maybe_machineword
    ; dataAddrMisaligned  : out contranomy_types.maybe_machineword
    ; transactionComplete : out boolean
    );
end;

architecture structural of loadstoreunit is
  -- src/Contranomy/Core/LoadStore.hs:65:11-36
  signal \c$app_arg\                        : boolean;
  -- src/Contranomy/Core/LoadStore.hs:64:11-48
  signal \c$app_arg_0\                      : contranomy_types.maybe_0;
  -- src/Contranomy/Wishbone.hs:42:5-7
  signal \c$case_alt_0\                     : contranomy_types.maybe_0;
  -- src/Contranomy/Core/LoadStore.hs:(59,11)-(62,25)
  signal \c$app_arg_1\                      : contranomy_types.maybe_0;
  -- src/Contranomy/Wishbone.hs:42:5-7
  signal \c$case_alt_1\                     : contranomy_types.maybe_0;
  -- src/Contranomy/Core/LoadStore.hs:59:44-68
  signal \c$case_alt_2\                     : contranomy_types.maybe_0;
  -- src/Contranomy/Core/LoadStore.hs:62:18-25
  signal \c$app_arg_2\                      : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(49,13)-(51,68)
  signal result_1                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(49,13)-(51,68)
  signal \sign\                             : contranomy_types.sign_r;
  signal \c$case_alt_3\                     : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:51:29-67
  signal karg                               : std_logic_vector(15 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(45,13)-(47,67)
  signal result_2                           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(45,13)-(47,67)
  signal sign_0                             : contranomy_types.sign_r;
  signal \c$case_alt_4\                     : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:47:28-66
  signal karg_0                             : std_logic_vector(7 downto 0);
  signal x                                  : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:84:11-36
  signal \c$app_arg_3\                      : boolean;
  -- src/Contranomy/Core/LoadStore.hs:83:11-48
  signal \c$app_arg_4\                      : contranomy_types.maybe_0;
  -- src/Contranomy/Wishbone.hs:42:5-7
  signal \c$case_alt_6\                     : contranomy_types.maybe_0;
  -- src/Contranomy/Core/LoadStore.hs:75:28-36
  signal \c$app_arg_5\                      : std_logic_vector(31 downto 0);
  signal \c$case_alt_7\                     : std_logic_vector(31 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(124,3)-(134,12)
  signal \shiftAmount\                      : signed(63 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(130,15)-(132,12)
  signal \c$shiftAmount_case_alt\           : signed(63 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(125,15)-(129,12)
  signal \c$shiftAmount_case_alt_0\         : signed(63 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:125:20-28
  signal \c$shiftAmount_case_alt_1\         : signed(63 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:125:20-28
  signal \c$shiftAmount_case_alt_2\         : signed(63 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(112,3)-(122,17)
  signal mask                               : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(118,15)-(120,17)
  signal \c$mask_case_alt\                  : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(113,15)-(117,17)
  signal \c$mask_case_alt_0\                : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:113:20-28
  signal \c$mask_case_alt_1\                : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:113:20-28
  signal \c$mask_case_alt_2\                : std_logic_vector(3 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:(104,3)-(110,13)
  signal aligned                            : boolean;
  -- src/Contranomy/Core/LoadStore.hs:108:10-34
  signal \c$aligned_case_alt\               : boolean;
  -- src/Contranomy/Core/LoadStore.hs:(104,3)-(110,13)
  signal \c$aligned_app_arg\                : std_logic;
  -- src/Contranomy/Core/LoadStore.hs:102:3-30
  signal alignment                          : std_logic_vector(1 downto 0);
  -- src/Contranomy/Core/LoadStore.hs:100:3-31
  signal \loadStoreWidth\                   : contranomy_types.loadstorewidth;
  signal decodedInstruction                 : contranomy_types.decodedinstruction;
  -- src/Contranomy/Wishbone.hs:42:5-7
  signal \c$busFinished_case_alt\           : boolean;
  signal opCode                 : std_logic_vector(6 downto 0);
  signal \c$case_alt_0_selection\           : boolean;
  signal \c$case_alt_1_selection\           : boolean;
  signal \c$case_alt_2_selection\           : boolean;
  signal \c$case_alt_3_selection_res\       : boolean;
  signal \c$case_alt_4_selection_res\       : boolean;
  signal result_3_selection                 : std_logic_vector(6 downto 0);
  signal \c$case_alt_6_selection\           : boolean;
  signal \c$aligned_case_alt_selection_res\ : boolean;
  signal \c$busFinished_case_alt_selection\ : boolean;

begin
  opCode <= decodedInstruction.decodedinstruction_sel0_opcode;

  result_select: process(opCode, instructionFault)
  begin

    if (opCode = LOAD) and (not instructionFault) then
      dBusM2S <=
        ( wishbonem2s_sel0_addr => addr(31 downto 2)
        , wishbonem2s_sel1_writedata => std_logic_vector'(0 to 31 => '-')
        , wishbonem2s_sel2_busselect => mask
        , wishbonem2s_sel3_buscycle => aligned
        , wishbonem2s_sel4_strobe => aligned
        , wishbonem2s_sel5_writeenable => false
        , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
        , wishbonem2s_sel7_bursttypeextension => "00"
        );

      loadVal <= \c$app_arg_1\;
      dataAccessFault <= \c$case_alt_0\;
      dataAddrMisaligned <= \c$app_arg_0\;
      transactionComplete <= \c$app_arg\;

    elsif (opCode = STORE) and (not instructionFault) then
      dBusM2S <=
        ( wishbonem2s_sel0_addr                => addr(31 downto 2)
        , wishbonem2s_sel1_writedata           => \c$app_arg_5\
        , wishbonem2s_sel2_busselect           => mask
        , wishbonem2s_sel3_buscycle            => aligned
        , wishbonem2s_sel4_strobe              => aligned
        , wishbonem2s_sel5_writeenable         => aligned
        , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
        , wishbonem2s_sel7_bursttypeextension  => "00"
        );

      loadVal <= nothing_machineword;
      dataAccessFault <= \c$case_alt_6\;
      dataAddrMisaligned <= \c$app_arg_4\;
      transactionComplete <= \c$app_arg_3\;
    else
      dBusM2S <=
        ( wishbonem2s_sel0_addr                => std_logic_vector'(0 to 29 => '-')
        , wishbonem2s_sel1_writedata           => std_logic_vector'(0 to 31 => '-')
        , wishbonem2s_sel2_busselect           => std_logic_vector'(0 to 3 => '-')
        , wishbonem2s_sel3_buscycle            => false
        , wishbonem2s_sel4_strobe              => false
        , wishbonem2s_sel5_writeenable         => false
        , wishbonem2s_sel6_cycletypeidentifier => std_logic_vector'("000")
        , wishbonem2s_sel7_bursttypeextension  => "00"
        );

      loadVal <= nothing_machineword;
      dataAccessFault <= nothing_machineword;
      dataAddrMisaligned <= nothing_machineword;
      transactionComplete <= true;
    end if;
  end process result_select;



  -- src/Contranomy/Core/LoadStore.hs:65:11-36
  \c$app_arg\ <= \c$busFinished_case_alt\ when aligned else
                 true;

  -- src/Contranomy/Core/LoadStore.hs:64:11-48
  \c$app_arg_0\ <= std_logic_vector'("0" & "--------------------------------") when aligned else
                   std_logic_vector'("1" & (addr));

  \c$case_alt_0_selection\ <= dBusS2M.wishbones2m_sel2_err;

  -- src/Contranomy/Wishbone.hs:42:5-7
  \c$case_alt_0\ <= std_logic_vector'("1" & (addr)) when \c$case_alt_0_selection\ else
                    std_logic_vector'("0" & "--------------------------------");

  -- src/Contranomy/Core/LoadStore.hs:(59,11)-(62,25)
  \c$app_arg_1\ <= \c$case_alt_1\ when aligned else
                   std_logic_vector'("0" & "--------------------------------");

  \c$case_alt_1_selection\ <= dBusS2M.wishbones2m_sel2_err;

  -- src/Contranomy/Wishbone.hs:42:5-7
  \c$case_alt_1\ <= std_logic_vector'("0" & "--------------------------------") when \c$case_alt_1_selection\ else
                    \c$case_alt_2\;

  \c$case_alt_2_selection\ <= dBusS2M.wishbones2m_sel1_acknowledge;

  -- src/Contranomy/Core/LoadStore.hs:59:44-68
  -- src/Contranomy/Wishbone.hs:40:5-15
  \c$case_alt_2\ <= std_logic_vector'("1" & (\c$app_arg_2\)) when \c$case_alt_2_selection\ else
                    std_logic_vector'("0" & "--------------------------------");

  -- src/Contranomy/Core/LoadStore.hs:62:18-25
  -- src/Contranomy/Core/LoadStore.hs:(43,9)-(52,31)
  -- src/Contranomy/Core/LoadStore.hs:(43,20)-(52,31)
  \c$app_arg_2\ <= result_2 when std_match("-00", \loadStoreWidth\) else
                   result_1 when std_match("-01", \loadStoreWidth\) else
                   dBusS2M.wishbones2m_sel0_readdata;

  -- src/Contranomy/Core/LoadStore.hs:(49,13)-(51,68)
  -- src/Contranomy/Core/LoadStore.hs:(141,1)-(142,32)
  result_1 <= \c$case_alt_3\ when std_match("0", \sign\) else
              std_logic_vector'(std_logic_vector'(std_logic_vector'(x"0000")) & std_logic_vector'(karg));

  \sign\ <= \loadStoreWidth\(2 downto 2);

  \c$case_alt_3_selection_res\ <= ( karg(karg'high) ) = ('0');

  \c$case_alt_3\ <= std_logic_vector'(std_logic_vector'(std_logic_vector'(x"0000")) & std_logic_vector'(karg)) when \c$case_alt_3_selection_res\ else
                    std_logic_vector'(std_logic_vector'(std_logic_vector'(x"FFFF")) & std_logic_vector'(karg));

  karg <= x(15 downto 0);

  -- src/Contranomy/Core/LoadStore.hs:(45,13)-(47,67)
  -- src/Contranomy/Core/LoadStore.hs:(141,1)-(142,32)
  result_2 <= \c$case_alt_4\ when std_match("0", sign_0) else
              std_logic_vector'(std_logic_vector'(std_logic_vector'(x"000000")) & std_logic_vector'(karg_0));

  sign_0 <= \loadStoreWidth\(2 downto 2);

  \c$case_alt_4_selection_res\ <= ( karg_0(karg_0'high) ) = ('0');

  \c$case_alt_4\ <= std_logic_vector'(std_logic_vector'(std_logic_vector'(x"000000")) & std_logic_vector'(karg_0)) when \c$case_alt_4_selection_res\ else
                    std_logic_vector'(std_logic_vector'(std_logic_vector'(x"FFFFFF")) & std_logic_vector'(karg_0));

  karg_0 <= x(7 downto 0);

  -- src/Contranomy/Core/LoadStore.hs:47:28-66
  -- src/Contranomy/Wishbone.hs:38:5-12
  x <= std_logic_vector(shift_right(unsigned(dBusS2M.wishbones2m_sel0_readdata),to_integer(\shiftAmount\)));

  result_3_selection <= decodedInstruction.decodedinstruction_sel0_opcode;



  -- src/Contranomy/Core/LoadStore.hs:84:11-36
  \c$app_arg_3\ <= \c$busFinished_case_alt\ when aligned else
                   true;

  -- src/Contranomy/Core/LoadStore.hs:83:11-48
  \c$app_arg_4\ <= std_logic_vector'("0" & "--------------------------------") when aligned else
                   std_logic_vector'("1" & (addr));

  \c$case_alt_6_selection\ <= dBusS2M.wishbones2m_sel2_err;

  -- src/Contranomy/Wishbone.hs:42:5-7
  \c$case_alt_6\ <= std_logic_vector'("1" & (addr)) when \c$case_alt_6_selection\ else
                    std_logic_vector'("0" & "--------------------------------");

  -- src/Contranomy/Core/LoadStore.hs:75:28-36
  -- src/Contranomy/Core/LoadStore.hs:(69,9)-(72,22)
  -- src/Contranomy/Core/LoadStore.hs:(69,21)-(72,22)
  \c$app_arg_5\ <= \c$case_alt_7\ when std_match("-00", \loadStoreWidth\) else
                   \c$case_alt_7\ when std_match("-01", \loadStoreWidth\) else
                   toStore;

  -- src/Contranomy/Core/LoadStore.hs:70:21-48
  \c$case_alt_7\ <= std_logic_vector(shift_left(unsigned(toStore),to_integer(\shiftAmount\)));

  -- src/Contranomy/Core/LoadStore.hs:(124,3)-(134,12)
  -- src/Contranomy/Core/LoadStore.hs:(124,17)-(134,12)
  \shiftAmount\ <= \c$shiftAmount_case_alt_0\ when std_match("-00", \loadStoreWidth\) else
                   \c$shiftAmount_case_alt\ when std_match("-01", \loadStoreWidth\) else
                   to_signed(0,64);

  -- src/Contranomy/Core/LoadStore.hs:(130,15)-(132,12)
  with (alignment) select
    \c$shiftAmount_case_alt\ <= to_signed(16,64) when "10",
                                to_signed(0,64) when others;

  -- src/Contranomy/Core/LoadStore.hs:(125,15)-(129,12)
  with (alignment) select
    \c$shiftAmount_case_alt_0\ <= to_signed(24,64) when "11",
                                  \c$shiftAmount_case_alt_1\ when others;

  -- src/Contranomy/Core/LoadStore.hs:125:20-28
  with (alignment) select
    \c$shiftAmount_case_alt_1\ <= to_signed(16,64) when "10",
                                  \c$shiftAmount_case_alt_2\ when others;

  -- src/Contranomy/Core/LoadStore.hs:125:20-28
  with (alignment) select
    \c$shiftAmount_case_alt_2\ <= to_signed(8,64) when "01",
                                  to_signed(0,64) when others;

  -- src/Contranomy/Core/LoadStore.hs:(112,3)-(122,17)
  -- src/Contranomy/Core/LoadStore.hs:(112,10)-(122,17)
  mask <= \c$mask_case_alt_0\ when std_match("-00", \loadStoreWidth\) else
          \c$mask_case_alt\ when std_match("-01", \loadStoreWidth\) else
          std_logic_vector'(x"F");

  -- src/Contranomy/Core/LoadStore.hs:(118,15)-(120,17)
  with (alignment) select
    \c$mask_case_alt\ <= std_logic_vector'(x"C") when "10",
                         std_logic_vector'(x"3") when others;

  -- src/Contranomy/Core/LoadStore.hs:(113,15)-(117,17)
  with (alignment) select
    \c$mask_case_alt_0\ <= std_logic_vector'(x"8") when "11",
                           \c$mask_case_alt_1\ when others;

  -- src/Contranomy/Core/LoadStore.hs:113:20-28
  with (alignment) select
    \c$mask_case_alt_1\ <= std_logic_vector'(x"4") when "10",
                           \c$mask_case_alt_2\ when others;

  -- src/Contranomy/Core/LoadStore.hs:113:20-28
  with (alignment) select
    \c$mask_case_alt_2\ <= std_logic_vector'(x"2") when "01",
                           std_logic_vector'(x"1") when others;

  -- src/Contranomy/Core/LoadStore.hs:(104,3)-(110,13)
  -- src/Contranomy/Core/LoadStore.hs:(104,13)-(110,13)
  aligned <= \c$aligned_case_alt\ when std_match("-01", \loadStoreWidth\) else
             alignment = std_logic_vector'("00") when std_match("010", \loadStoreWidth\) else
             true;

  \c$aligned_case_alt_selection_res\ <= \c$aligned_app_arg\ = ('1');

  -- src/Contranomy/Core/LoadStore.hs:108:10-34
  \c$aligned_case_alt\ <= false when \c$aligned_case_alt_selection_res\ else
                          true;

  -- indexBitVector begin
  indexbitvector : block
    signal vec_index : integer range 0 to 2-1;
  begin
    vec_index <= to_integer(to_signed(0,64))
    -- pragma translate_off
                 mod 2
    -- pragma translate_on
                 ;

    \c$aligned_app_arg\ <= alignment(vec_index);
  end block;
  -- indexBitVector end

  -- src/Contranomy/Core/LoadStore.hs:102:3-30
  -- src/Contranomy/Core/LoadStore.hs:102:15-30
  alignment <= addr(1 downto 0);

  -- src/Contranomy/Core/LoadStore.hs:100:3-31
  -- src/Contranomy/Core/LoadStore.hs:100:20-31
  -- src/Contranomy/Instruction.hs:243:1-34
  \loadStoreWidth\ <= decodedInstruction.decodedinstruction_sel15_func3;

  decodeinstruction_ccase_scrut : entity decodeinstruction
    port map
      ( result => decodedInstruction
      , w      => instruction );

  \c$busFinished_case_alt_selection\ <= dBusS2M.wishbones2m_sel2_err;

  -- src/Contranomy/Wishbone.hs:42:5-7
  \c$busFinished_case_alt\ <= true when \c$busFinished_case_alt_selection\ else
                              dBusS2M.wishbones2m_sel1_acknowledge;

end;

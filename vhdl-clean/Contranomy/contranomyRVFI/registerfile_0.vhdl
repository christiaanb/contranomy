-- Automatically generated VHDL-93
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.all;
use work.all;
use work.contranomyrvfi_types.all;

entity registerfile_0 is
  port(-- clock
       \c$ds_bindCsr\   : in contranomyrvfi_types.clk_core;
       -- reset
       \c$ds_bindCsr_0\ : in contranomyrvfi_types.rst_core;
       ds               : in contranomyrvfi_types.tup3_0;
       result           : out contranomyrvfi_types.tup2_6);
end;

architecture structural of registerfile_0 is
  signal result_0           : std_logic_vector(31 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal wild               : signed(63 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal \c$wild_app_arg\   : signed(63 downto 0);
  signal result_1           : contranomyrvfi_types.register_r;
  signal \fromMaybe_d\      : contranomyrvfi_types.register_r := std_logic_vector(to_unsigned(0,5));
  signal \c$d_app_arg\      : contranomyrvfi_types.register_r;
  signal a1                 : contranomyrvfi_types.register_r;
  signal \c$d_app_arg_0\    : boolean;
  -- src/Contranomy/RegisterFile.hs:(28,3)-(30,28)
  signal \rs2M\             : contranomyrvfi_types.maybe_1;
  signal result_2           : std_logic_vector(31 downto 0);
  signal result_3           : signed(63 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal wild_0             : signed(63 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal \c$wild_app_arg_0\ : signed(63 downto 0);
  -- src/Contranomy/RegisterFile.hs:28:66-67
  signal ds1                : contranomyrvfi_types.tup2_7;
  signal a1_0               : contranomyrvfi_types.tup2_7;
  signal \c$app_arg\        : boolean;
  -- src/Contranomy/RegisterFile.hs:(28,3)-(30,28)
  signal rw                 : contranomyrvfi_types.maybe;
  -- src/Contranomy/Instruction.hs:123:1-28
  signal wild_1             : signed(63 downto 0);
  -- src/Contranomy/Instruction.hs:123:1-28
  signal \c$wild_app_arg_1\ : signed(63 downto 0);
  signal result_4           : contranomyrvfi_types.register_r;
  signal \c$fromMaybe_d_0\  : contranomyrvfi_types.register_r := std_logic_vector(to_unsigned(0,5));
  signal \c$d_app_arg_1\    : contranomyrvfi_types.register_r;
  signal a1_1               : contranomyrvfi_types.register_r;
  signal \c$d_app_arg_2\    : boolean;
  -- src/Contranomy/RegisterFile.hs:(28,3)-(30,28)
  signal \rs1M\             : contranomyrvfi_types.maybe_1;

begin
  -- src/Contranomy/RegisterFile.hs:(27,1)-(30,28)
  -- src/Contranomy/RegisterFile.hs:(28,3)-(30,28)
  result <= ( tup2_6_sel0_std_logic_vector_0 => result_2
            , tup2_6_sel1_std_logic_vector_1 => result_0 );

  -- src/Contranomy/RegisterFile.hs:30:22-27
  -- src/Contranomy/RegisterFile.hs:29:7-67
  -- src/Contranomy/RegisterFile.hs:29:16-67
  -- blockRam begin
  result_0_blockram : block
    signal result_0_ram : contranomyrvfi_types.array_of_std_logic_vector_32(0 to 31) := contranomyrvfi_types.array_of_std_logic_vector_32'( std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000") );
    signal rd  : integer range 0 to 32 - 1;
    signal wr  : integer range 0 to 32 - 1;
  begin
    rd <= to_integer((wild))
    -- pragma translate_off
                  mod 32
    -- pragma translate_on
                  ;

    wr <= to_integer(result_3)
    -- pragma translate_off
                  mod 32
    -- pragma translate_on
                  ;

    \c$n\ : process(\c$ds_bindCsr\)
    begin
      if rising_edge(\c$ds_bindCsr\) then
        if \c$app_arg\   then
          result_0_ram(wr) <= ds1.tup2_7_sel1_std_logic_vector;
        end if;
        result_0 <= result_0_ram(rd);
      end if;
    end process; 
  end block;
  --end blockRam

  wild <= \c$wild_app_arg\;

  \c$wild_app_arg\ <= signed(std_logic_vector(resize(unsigned((result_1)),64)));

  with (\rs2M\(5 downto 5)) select
    result_1 <= \fromMaybe_d\ when "0",
                a1 when others;

  -- src/Contranomy/RegisterFile.hs:41:41-43
  -- src/Contranomy/RegisterFile.hs:43:3-27
  -- src/Contranomy/RegisterFile.hs:43:9-27
  -- register begin 
  frommaybe_d_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \fromMaybe_d\ <= std_logic_vector(to_unsigned(0,5));
      elsif \c$d_app_arg_0\ then
        \fromMaybe_d\ <= \c$d_app_arg\;
      end if;
    end if;
  end process;
  -- register end

  with (\rs2M\(5 downto 5)) select
    \c$d_app_arg\ <= contranomyrvfi_types.register_r'(0 to 4 => '-') when "0",
                     a1 when others;

  a1 <= \rs2M\(4 downto 0);

  with (\rs2M\(5 downto 5)) select
    \c$d_app_arg_0\ <= false when "0",
                       true when others;

  \rs2M\ <= ds.tup3_0_sel1_maybe_1_1;

  -- src/Contranomy/RegisterFile.hs:30:15-20
  -- src/Contranomy/RegisterFile.hs:28:7-67
  -- src/Contranomy/RegisterFile.hs:28:16-67
  -- blockRam begin
  result_2_blockram : block
    signal result_2_ram : contranomyrvfi_types.array_of_std_logic_vector_32(0 to 31) := contranomyrvfi_types.array_of_std_logic_vector_32'( std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000"), std_logic_vector'(x"00000000") );
    signal rd_0  : integer range 0 to 32 - 1;
    signal wr_0  : integer range 0 to 32 - 1;
  begin
    rd_0 <= to_integer((wild_1))
    -- pragma translate_off
                  mod 32
    -- pragma translate_on
                  ;

    wr_0 <= to_integer(result_3)
    -- pragma translate_off
                  mod 32
    -- pragma translate_on
                  ;

    \c$n_0\ : process(\c$ds_bindCsr\)
    begin
      if rising_edge(\c$ds_bindCsr\) then
        if \c$app_arg\   then
          result_2_ram(wr_0) <= ds1.tup2_7_sel1_std_logic_vector;
        end if;
        result_2 <= result_2_ram(rd_0);
      end if;
    end process; 
  end block;
  --end blockRam

  result_3 <= wild_0;

  wild_0 <= \c$wild_app_arg_0\;

  \c$wild_app_arg_0\ <= signed(std_logic_vector(resize(unsigned((ds1.tup2_7_sel0_register_r)),64)));

  with (rw(37 downto 37)) select
    ds1 <= contranomyrvfi_types.tup2_7'( contranomyrvfi_types.register_r'(0 to 4 => '-'), std_logic_vector'(0 to 31 => '-') ) when "0",
           a1_0 when others;

  a1_0 <= contranomyrvfi_types.tup2_7'(contranomyrvfi_types.fromSLV(rw(36 downto 0)));

  with (rw(37 downto 37)) select
    \c$app_arg\ <= false when "0",
                   true when others;

  rw <= ds.tup3_0_sel2_maybe;

  wild_1 <= \c$wild_app_arg_1\;

  \c$wild_app_arg_1\ <= signed(std_logic_vector(resize(unsigned((result_4)),64)));

  with (\rs1M\(5 downto 5)) select
    result_4 <= \c$fromMaybe_d_0\ when "0",
                a1_1 when others;

  -- src/Contranomy/RegisterFile.hs:41:41-43
  -- src/Contranomy/RegisterFile.hs:43:3-27
  -- src/Contranomy/RegisterFile.hs:43:9-27
  -- register begin 
  cfrommaybe_d_0_register : process(\c$ds_bindCsr\)
  begin
    if rising_edge(\c$ds_bindCsr\) then
      if \c$ds_bindCsr_0\ =  '1'  then
        \c$fromMaybe_d_0\ <= std_logic_vector(to_unsigned(0,5));
      elsif \c$d_app_arg_2\ then
        \c$fromMaybe_d_0\ <= \c$d_app_arg_1\;
      end if;
    end if;
  end process;
  -- register end

  with (\rs1M\(5 downto 5)) select
    \c$d_app_arg_1\ <= contranomyrvfi_types.register_r'(0 to 4 => '-') when "0",
                       a1_1 when others;

  a1_1 <= \rs1M\(4 downto 0);

  with (\rs1M\(5 downto 5)) select
    \c$d_app_arg_2\ <= false when "0",
                       true when others;

  \rs1M\ <= ds.tup3_0_sel0_maybe_1_0;


end;


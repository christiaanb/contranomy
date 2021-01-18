library ieee;

use ieee.std_logic_1164.all;
use work.contranomy_types.all;

package constants is

  -- MCause
  constant INSTRUCTION_ADDRESS_MISALIGNED : mcause := (False, "0000");
  constant INSTRUCTION_ACCESS_FAULT : mcause := (False, "0001");
  constant ILLEGAL_INSTRUCTION : MCause := (False, "0010");
  constant BREAKPOINT : MCause := (False, "0011");
  constant LOAD_ADDRESS_MISALIGNED : MCause := (False, "0100");
  constant LOAD_ACCESS_FAULT : MCause := (False, "0101");
  constant STORE_ADDRESS_MISALIGNED : MCause := (False, "0110");
  constant STORE_ACCESS_FAULT : MCause := (False, "0111");
  constant ENVIRONMENT_CALL : MCause := (False, "1011");
  constant MACHINE_SOFTWARE_INTERRUPT : MCause := (True, "0011");
  constant MACHINE_TIMER_INTERRUPT : MCause := (True, "0111");
  constant MACHINE_EXTERNAL_INTERRUPT : MCause := (True, "1011");

  -- Opcode

  subtype Opcode is std_logic_vector(6 downto 0);

  constant LUI : Opcode := "0110111";
  constant AUIPC : Opcode := "0010111";
  constant JAL : Opcode := "1101111";
  constant JALR : Opcode := "1100111";
  constant BRANCH : Opcode := "1100011";
  constant LOAD : Opcode := "0000011";
  constant STORE : Opcode := "0100011";
  constant OP_IMM : Opcode := "0010011";
  constant OP : Opcode := "0110011";
  constant MISC_MEM : Opcode := "0001111";
  constant SYSTEM : Opcode := "1110011";

  -- IOp

  subtype IOp is std_logic_vector(2 downto 0);

  constant IOP_ADD : IOp := "000";
  constant IOP_SLL : IOp := "001";
  constant IOP_SLT : IOp := "010";
  constant IOP_SLTU : IOp := "011";
  constant IOP_XOR : IOp := "100";
  constant IOP_SR : IOp := "101";
  constant IOP_OR : IOp := "110";
  constant IOP_AND : IOp := "111";

  -- MOp

  subtype MOp is std_logic_vector(2 downto 0);

  constant MOP_MUL : MOp := "000";
  constant MOP_MULH : MOp := "001";
  constant MOP_MULHSU : MOp := "010";
  constant MOP_MULHU : MOp := "011";
  constant MOP_DIV : MOp := "100";
  constant MOP_DIVU : MOp := "101";
  constant MOP_REM : MOp := "110";
  constant MOP_REMU : MOp := "111";

  -- Load Store Width

  subtype LSW is std_logic_vector(2 downto 0);

  constant LSW_UBYTE : LSW := "100";
  constant LSW_SBYTE : LSW := "000";
  constant LSW_UHALF : LSW := "101";
  constant LSW_SHALF : LSW := "001";
  constant LSW_WORD : LSW := "010";
  constant LSW_ILL1 : LSW := "011";
  constant LSW_ILL2 : LSW := "110";
  constant LSW_ILL3 : LSW := "111";

  -- Branch Condition

  subtype Branch_Cond is std_logic_vector(2 downto 0);

  constant BC_EQ   : Branch_Cond := "000";
  constant BC_NE   : Branch_Cond := "001";
  constant BC_LT   : Branch_Cond := "010";
  constant BC_GE   : Branch_Cond := "011";
  constant BC_LTU  : Branch_Cond := "100";
  constant BC_GEU  : Branch_Cond := "101";
  constant BC_ILL1 : Branch_Cond := "110";
  constant BC_ILL2 : Branch_Cond := "111";

  -- Machine Trap Setup

  subtype CSRRegister is std_logic_vector(11 downto 0);

  constant MSTATUS    : CSRRegister  := x"300"; -- Machine status register
  constant MISA       : CSRRegister  := x"301"; -- ISA and extensions
  constant MEDELEG    : CSRRegister  := x"302"; -- Machine exception delegation register
  constant MIDELEG    : CSRRegister  := x"303"; -- Machine interrupt delegation register
  constant MIE        : CSRRegister  := x"304"; -- Machine interrupt enable register
  constant MTVEC      : CSRRegister  := x"305"; -- Machine trap-handler base address
  constant MCOUNTEREN : CSRRegister  := x"306"; -- Machine counter enable
  constant MSTATUSH   : CSRRegister  := x"307"; -- Additional machine status register, RV32 only

  -- Machine Trap Handling

  constant MSCRATCH : CSRRegister := x"340"; -- Scratch register for machine trap handlers
  constant MEPC     : CSRRegister := x"341"; -- Machine exception program counter
  constant MCAUSE   : CSRRegister := x"342"; -- Machine trap cause
  constant MTVAL    : CSRRegister := x"343"; -- Machine bad address instruction
  constant MIP      : CSRRegister := x"344"; -- Machine interrupt pending
  constant MTINST   : CSRRegister := x"34A"; -- Machine trap instruction (transformed)
  constant MTVAL2   : CSRRegister := x"34B"; -- Machine bad guest physical address

  -- Architecture-specific Registers

  constant IRQMASK    : CSRRegister := x"330";
  constant IEQPENDING : CSRRegister := x"360";

  -- CSR Type

  subtype CSR_Type is std_logic_vector(1 downto 0);

  constant CSR_ILL    : CSR_Type := "00";
  constant READ_WRITE : CSR_Type := "01";
  constant READ_SET   : CSR_Type := "10";
  constant READ_CLEAR : CSR_Type := "11";

  -- System12

  subtype System_12 is std_logic_vector(11 downto 0);

  constant ECALL  : System_12 := (others => '0');
  constant EBREAK : System_12 := (0 => '1', others => '0');
  constant MRET   : System_12 := "001100000010";

  -- Wishbone Cycle Type Identifier

  subtype Cycle_Type_Identifier is std_logic_vector(2 downto 0);

  constant CLASSIC : Cycle_Type_Identifier := "000";
  constant CONSTANT_ADDRESS_BURST := "001";
  constant INCREMENTING_BURST := "010";
  constant END_OF_BURST := "111";

end package constants;


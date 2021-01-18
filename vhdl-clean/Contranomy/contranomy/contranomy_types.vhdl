library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

package contranomy_types is
  -- iop
  subtype iop is std_logic_vector(2 downto 0);

  constant IOP_ADD  : iop := "000";
  constant IOP_SLL  : iop := "001";
  constant IOP_SLT  : iop := "010";
  constant IOP_SLTU : iop := "011";
  constant IOP_XOR  : iop := "100";
  constant IOP_SR   : iop := "101";
  constant IOP_OR   : iop := "110";
  constant IOP_AND  : iop := "111";

  type Reg is
      ( x0,  x1,  x2,  x3,  x4,  x5,  x6,  x7
      , x8,  x9,  x10, x11, x12, x13, x14, x15
      , x16, x17, x18, x19, x20, x21, x22, x23
      , x24, x25, x26, x27, x28, x29, x30, x31
  );

  -- exception_in
  subtype maybe_machineword is std_logic_vector(32 downto 0);
  type exception_in is record
    ei_instraccessfault      : boolean;
    ei_instr_addr_misaligned : boolean;
    ei_instr_illegal         : boolean;
    ei_data_access_fault     : contranomy_types.maybe_machineword;
    ei_data_addr_misaligned  : contranomy_types.maybe_machineword;
    ei_timer_interrupt       : boolean;
    ei_software_interrupt    : boolean;
    ei_external_interrupt    : std_logic_vector(31 downto 0);
  end record;

  -- CoreStage
  subtype corestage is std_logic_vector(1 downto 0);

  constant INSTRUCTION_FETCH : corestage := "00";
  constant EXECUTE_NO_FAULT  : corestage := "10";
  constant EXECUTE_FAULT     : corestage := "11";

  -- mop
  subtype mop is std_logic_vector(2 downto 0);

  constant MOP_MUL    : mop := "000";
  constant MOP_MULH   : mop := "001";
  constant MOP_MULHSU : mop := "010";
  constant MOP_MULHU  : mop := "011";
  constant MOP_DIV    : mop := "100";
  constant MOP_DIVU   : mop := "101";
  constant MOP_REM    : mop := "110";
  constant MOP_REMU   : mop := "111";


  subtype machine_word is std_logic_vector(31 downto 0);
  subtype pc is std_logic_vector(29 downto 0);

  subtype timer_interrupt is std_logic;
  subtype software_interrupt is std_logic;
  subtype external_interrupt is std_logic;

  subtype clk_core is std_logic;

  type shift_mode is (shift_mode_logical, shift_mode_arithmetic);

  -- dirty types

  subtype register_r is std_logic_vector(4 downto 0);
  subtype bursttypeextension is std_logic_vector(1 downto 0);

  type wishbones2m is record
    wishbones2m_sel0_readdata : std_logic_vector(31 downto 0);
    wishbones2m_sel1_acknowledge : boolean;
    wishbones2m_sel2_err : boolean;
  end record;
  type mstatus is record
    mstatus_sel0_mie : boolean;
    mstatus_sel1_mpie : boolean;
  end record;
  subtype csrtype is std_logic_vector(1 downto 0);
  type array_of_std_logic_vector_32 is array (integer range <>) of std_logic_vector(31 downto 0);

  type decodedinstruction is record
    decodedinstruction_sel0_opcode : std_logic_vector(6 downto 0);
    decodedinstruction_sel1_rd : contranomy_types.register_r;
    decodedinstruction_sel2_rs1 : contranomy_types.register_r;
    decodedinstruction_sel3_rs2 : contranomy_types.register_r;
    decodedinstruction_sel4_iop : contranomy_types.iop;
    decodedinstruction_sel5_srla : contranomy_types.shift_mode;
    decodedinstruction_sel6_shamt : std_logic_vector(4 downto 0);
    decodedinstruction_sel7_issub : boolean;
    decodedinstruction_sel8_ism : boolean;
    decodedinstruction_sel9_mop : contranomy_types.mop;
    decodedinstruction_sel10_imm20u : std_logic_vector(19 downto 0);
    decodedinstruction_sel11_imm20j : std_logic_vector(19 downto 0);
    decodedinstruction_sel12_imm12i : std_logic_vector(11 downto 0);
    decodedinstruction_sel13_imm12s : std_logic_vector(11 downto 0);
    decodedinstruction_sel14_imm12b : std_logic_vector(11 downto 0);
    decodedinstruction_sel15_func3 : std_logic_vector(2 downto 0);
    decodedinstruction_sel16_legal : boolean;
  end record;
  type rvficsr is record
    rvficsr_sel0_rmask : std_logic_vector(31 downto 0);
    rvficsr_sel1_wmask : std_logic_vector(31 downto 0);
    rvficsr_sel2_rdata : std_logic_vector(31 downto 0);
    rvficsr_sel3_wdata : std_logic_vector(31 downto 0);
  end record;
  type rvfi is record
    rvfi_sel0_valid : boolean;
    rvfi_sel1_order : unsigned(63 downto 0);
    rvfi_sel2_insn : std_logic_vector(31 downto 0);
    rvfi_sel3_trap : boolean;
    rvfi_sel4_halt : boolean;
    rvfi_sel5_intr : boolean;
    rvfi_sel6_mode : std_logic_vector(1 downto 0);
    rvfi_sel7_ixl : std_logic_vector(1 downto 0);
    rvfi_sel8_rs1addr : contranomy_types.register_r;
    rvfi_sel9_rs2addr : contranomy_types.register_r;
    rvfi_sel10_rs1rdata : std_logic_vector(31 downto 0);
    rvfi_sel11_rs2rdata : std_logic_vector(31 downto 0);
    rvfi_sel12_rdaddr : contranomy_types.register_r;
    rvfi_sel13_rdwdata : std_logic_vector(31 downto 0);
    rvfi_sel14_pcrdata : std_logic_vector(31 downto 0);
    rvfi_sel15_pcwdata : std_logic_vector(31 downto 0);
    rvfi_sel16_memaddr : std_logic_vector(31 downto 0);
    rvfi_sel17_memrmask : std_logic_vector(3 downto 0);
    rvfi_sel18_memwmask : std_logic_vector(3 downto 0);
    rvfi_sel19_memrdata : std_logic_vector(31 downto 0);
    rvfi_sel20_memwdata : std_logic_vector(31 downto 0);
    rvfi_sel21_misacsr : contranomy_types.rvficsr;
  end record;
  type tup2_7 is record
    tup2_7_sel0_register_r : contranomy_types.register_r;
    tup2_7_sel1_std_logic_vector : std_logic_vector(31 downto 0);
  end record;
  subtype rst_core is std_logic;
  type tup2_8 is record
    tup2_8_sel0_std_logic_vector : std_logic_vector(31 downto 0);
    tup2_8_sel1_csrtype : contranomy_types.csrtype;
  end record;
  type wishbonem2s is record
    wishbonem2s_sel0_addr : std_logic_vector(29 downto 0);
    wishbonem2s_sel1_writedata : std_logic_vector(31 downto 0);
    wishbonem2s_sel2_busselect : std_logic_vector(3 downto 0);
    wishbonem2s_sel3_buscycle : boolean;
    wishbonem2s_sel4_strobe : boolean;
    wishbonem2s_sel5_writeenable : boolean;
    wishbonem2s_sel6_cycletypeidentifier : std_logic_vector(2 downto 0);
    wishbonem2s_sel7_bursttypeextension : contranomy_types.bursttypeextension;
  end record;
  type coreout is record
    coreout_sel0_ibusm2s : contranomy_types.wishbonem2s;
    coreout_sel1_dbusm2s : contranomy_types.wishbonem2s;
  end record;
  type tup2_3 is record
    tup2_3_sel0_boolean : boolean;
    tup2_3_sel1_std_logic_vector : std_logic_vector(29 downto 0);
  end record;
  type tup2_5 is record
    tup2_5_sel0_std_logic_vector_0 : std_logic_vector(29 downto 0);
    tup2_5_sel1_std_logic_vector_1 : std_logic_vector(1 downto 0);
  end record;
  type clash_internal_1 is record
    clash_internal_1_sel0_corestage : contranomy_types.corestage;
    clash_internal_1_sel1_std_logic_vector : std_logic_vector(29 downto 0);
  end record;
  subtype interruptmode is std_logic_vector(31 downto 0);
  type corein is record
    corein_sel0_ibuss2m : contranomy_types.wishbones2m;
    corein_sel1_dbuss2m : contranomy_types.wishbones2m;
    corein_sel2_timerinterrupt : boolean;
    corein_sel3_softwareinterrupt : boolean;
    corein_sel4_externalinterrupt : std_logic_vector(31 downto 0);
  end record;
  type mie is record
    mie_sel0_meie : boolean;
    mie_sel1_mtie : boolean;
    mie_sel2_msie : boolean;
  end record;
  subtype maybe_1 is std_logic_vector(5 downto 0);
  subtype sign_r is std_logic_vector(0 downto 0);

  type mcause is record
    mcause_sel0_interrupt : boolean;
    mcause_sel1_code : std_logic_vector(3 downto 0);
  end record;
  type tup2_6 is record
    tup2_6_sel0_std_logic_vector_0 : std_logic_vector(31 downto 0);
    tup2_6_sel1_std_logic_vector_1 : std_logic_vector(31 downto 0);
  end record;
  type tup2 is record
    tup2_sel0_corein : contranomy_types.corein;
    tup2_sel1_tup2_6 : contranomy_types.tup2_6;
  end record;
  subtype branchcondition is std_logic_vector(2 downto 0);
  subtype csrop is std_logic_vector(2 downto 0);
  subtype loadstorewidth is std_logic_vector(2 downto 0);
  type machinestate is record
    machinestate_sel0_mstatus : contranomy_types.mstatus;
    machinestate_sel1_mcause : contranomy_types.mcause;
    machinestate_sel2_mtvec : contranomy_types.interruptmode;
    machinestate_sel3_mie : contranomy_types.mie;
    machinestate_sel4_mscratch : std_logic_vector(31 downto 0);
    machinestate_sel5_mepc : std_logic_vector(29 downto 0);
    machinestate_sel6_mtval : std_logic_vector(31 downto 0);
    machinestate_sel7_irqmask : std_logic_vector(31 downto 0);
  end record;
  type clash_internal_0 is record
    clash_internal_0_sel0_machinestate : contranomy_types.machinestate;
    clash_internal_0_sel1_unsigned : unsigned(63 downto 0);
  end record;
  type clash_internal is record
    clash_internal_sel0_std_logic_vector : std_logic_vector(31 downto 0);
    clash_internal_sel1_clash_internal_0 : contranomy_types.clash_internal_0;
  end record;
  type corestate is record
    corestate_sel0_stage : contranomy_types.corestage;
    corestate_sel1_pc : std_logic_vector(29 downto 0);
    corestate_sel2_instruction : std_logic_vector(31 downto 0);
    corestate_sel3_machinestate : contranomy_types.machinestate;
    corestate_sel4_rvfiorder : unsigned(63 downto 0);
  end record;
  type tup2_4 is record
    tup2_4_sel0_tup2_3 : contranomy_types.tup2_3;
    tup2_4_sel1_corestate : contranomy_types.corestate;
  end record;
  subtype maybe is std_logic_vector(37 downto 0);
  type tup3_0 is record
    tup3_0_sel0_maybe_1_0 : contranomy_types.maybe_1;
    tup3_0_sel1_maybe_1_1 : contranomy_types.maybe_1;
    tup3_0_sel2_maybe : contranomy_types.maybe;
  end record;
  type tup3 is record
    tup3_sel0_coreout : contranomy_types.coreout;
    tup3_sel1_tup3_0 : contranomy_types.tup3_0;
    tup3_sel2_rvfi : contranomy_types.rvfi;
  end record;
  type tup2_0 is record
    tup2_0_sel0_tup3 : contranomy_types.tup3;
    tup2_0_sel1_corestate : contranomy_types.corestate;
  end record;
  subtype maybe_0 is std_logic_vector(32 downto 0);

  type tup5 is record
    tup5_sel0_wishbonem2s : contranomy_types.wishbonem2s;
    tup5_sel1_maybe_0_0 : contranomy_types.maybe_0;
    tup5_sel2_maybe_0_1 : contranomy_types.maybe_0;
    tup5_sel3_maybe_0_2 : contranomy_types.maybe_0;
    tup5_sel4_boolean : boolean;
  end record;

  type tup2_1 is record
    tup2_1_sel0_maybe_0 : contranomy_types.maybe_0;
    tup2_1_sel1_std_logic_vector : std_logic_vector(31 downto 0);
  end record;
  type tup2_2 is record
    tup2_2_sel0_tup2_1 : contranomy_types.tup2_1;
    tup2_2_sel1_machinestate : contranomy_types.machinestate;
  end record;
  function toSLV (slv : in std_logic_vector) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector;
  function toSLV (sl : in std_logic) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return std_logic;
  function toSLV (b : in boolean) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector) return boolean;
  function tagToEnum (s : in signed) return boolean;
  function dataToTag (b : in boolean) return signed;
  function toSLV (u : in unsigned) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return unsigned;
  function toSLV (p : contranomy_types.wishbones2m) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.wishbones2m;
  function toSLV (p : contranomy_types.mstatus) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mstatus;
  function toSLV (value :  contranomy_types.array_of_std_logic_vector_32) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.array_of_std_logic_vector_32;
  function toSLV (p : contranomy_types.decodedinstruction) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.decodedinstruction;
  function toSLV (p : contranomy_types.rvficsr) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.rvficsr;
  function toSLV (p : contranomy_types.rvfi) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.rvfi;
  function toSLV (p : contranomy_types.tup2_7) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_7;
  function toSLV (p : contranomy_types.tup2_8) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_8;
  function toSLV (p : contranomy_types.wishbonem2s) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.wishbonem2s;
  function toSLV (p : contranomy_types.coreout) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.coreout;
  function toSLV (p : contranomy_types.tup2_3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_3;
  function toSLV (p : contranomy_types.tup2_5) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_5;
  function toSLV (p : contranomy_types.clash_internal_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal_1;
  function toSLV (p : contranomy_types.corein) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.corein;
  function toSLV (p : contranomy_types.mie) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mie;
  function toSLV (s : in signed) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return signed;
  function toSLV (p : contranomy_types.mcause) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mcause;
  function toSLV (p : contranomy_types.tup2_6) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_6;
  function toSLV (p : contranomy_types.tup2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2;
  function toSLV (p : contranomy_types.machinestate) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.machinestate;
  function toSLV (p : contranomy_types.clash_internal_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal_0;
  function toSLV (p : contranomy_types.clash_internal) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal;
  function toSLV (p : contranomy_types.corestate) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.corestate;
  function toSLV (p : contranomy_types.tup2_4) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_4;
  function toSLV (p : contranomy_types.tup3_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup3_0;
  function toSLV (p : contranomy_types.tup3) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup3;
  function toSLV (p : contranomy_types.tup2_0) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_0;
  function toSLV (p : contranomy_types.tup5) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup5;
  function toSLV (p : contranomy_types.exception_in) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.exception_in;
  function toSLV (p : contranomy_types.tup2_1) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_1;
  function toSLV (p : contranomy_types.tup2_2) return std_logic_vector;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_2;

  function toSLV (b : in shift_mode) return std_logic_vector;
  function fromSLV (sl : in std_logic_vector(0 downto 0)) return shift_mode;


end;

package body contranomy_types is
  function toSLV (b : in shift_mode) return std_logic_vector is
  begin
    case b is 
      when shift_mode_logical => return "0";
      when shift_mode_arithmetic => return "1";
    end case;
  end;
  function fromSLV (sl : in std_logic_vector(0 downto 0)) return shift_mode is
  begin
    case sl is
      when "0"    => return shift_mode_logical;
      when others => return shift_mode_arithmetic;
    end case;
  end;

  function toSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic_vector is
  begin
    return slv;
  end;
  function toSLV (sl : in std_logic) return std_logic_vector is
  begin
    return std_logic_vector'(0 => sl);
  end;
  function fromSLV (slv : in std_logic_vector) return std_logic is
    alias islv : std_logic_vector (0 to slv'length - 1) is slv;
  begin
    return islv(0);
  end;
  function toSLV (b : in boolean) return std_logic_vector is
  begin
    if b then
      return "1";
    else
      return "0";
    end if;
  end;
  function fromSLV (sl : in std_logic_vector) return boolean is
  begin
    if sl = "1" then
      return true;
    else
      return false;
    end if;
  end;
  function tagToEnum (s : in signed) return boolean is
  begin
    if s = to_signed(0,64) then
      return false;
    else
      return true;
    end if;
  end;
  function dataToTag (b : in boolean) return signed is
  begin
    if b then
      return to_signed(1,64);
    else
      return to_signed(0,64);
    end if;
  end;
  function toSLV (u : in unsigned) return std_logic_vector is
  begin
    return std_logic_vector(u);
  end;
  function fromSLV (slv : in std_logic_vector) return unsigned is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return unsigned(islv);
  end;
  function toSLV (p : contranomy_types.wishbones2m) return std_logic_vector is
  begin
    return (toSLV(p.wishbones2m_sel0_readdata) & toSLV(p.wishbones2m_sel1_acknowledge) & toSLV(p.wishbones2m_sel2_err));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.wishbones2m is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 32)),fromSLV(islv(33 to 33)));
  end;
  function toSLV (p : contranomy_types.mstatus) return std_logic_vector is
  begin
    return (toSLV(p.mstatus_sel0_mie) & toSLV(p.mstatus_sel1_mpie));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mstatus is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)));
  end;
  function toSLV (value :  contranomy_types.array_of_std_logic_vector_32) return std_logic_vector is
    alias ivalue    : contranomy_types.array_of_std_logic_vector_32(1 to value'length) is value;
    variable result : std_logic_vector(1 to value'length * 32);
  begin
    for i in ivalue'range loop
      result(((i - 1) * 32) + 1 to i*32) := toSLV(ivalue(i));
    end loop;
    return result;
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.array_of_std_logic_vector_32 is
    alias islv      : std_logic_vector(0 to slv'length - 1) is slv;
    variable result : contranomy_types.array_of_std_logic_vector_32(0 to slv'length / 32 - 1);
  begin
    for i in result'range loop
      result(i) := islv(i * 32 to (i+1) * 32 - 1);
    end loop;
    return result;
  end;
  function toSLV (p : contranomy_types.decodedinstruction) return std_logic_vector is
  begin
    return (toSLV(p.decodedinstruction_sel0_opcode) & toSLV(p.decodedinstruction_sel1_rd) & toSLV(p.decodedinstruction_sel2_rs1) & toSLV(p.decodedinstruction_sel3_rs2) & toSLV(p.decodedinstruction_sel4_iop) & toSLV(p.decodedinstruction_sel5_srla) & toSLV(p.decodedinstruction_sel6_shamt) & toSLV(p.decodedinstruction_sel7_issub) & toSLV(p.decodedinstruction_sel8_ism) & toSLV(p.decodedinstruction_sel9_mop) & toSLV(p.decodedinstruction_sel10_imm20u) & toSLV(p.decodedinstruction_sel11_imm20j) & toSLV(p.decodedinstruction_sel12_imm12i) & toSLV(p.decodedinstruction_sel13_imm12s) & toSLV(p.decodedinstruction_sel14_imm12b) & toSLV(p.decodedinstruction_sel15_func3) & toSLV(p.decodedinstruction_sel16_legal));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.decodedinstruction is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 6)),fromSLV(islv(7 to 11)),fromSLV(islv(12 to 16)),fromSLV(islv(17 to 21)),fromSLV(islv(22 to 24)),fromSLV(islv(25 to 25)),fromSLV(islv(26 to 30)),fromSLV(islv(31 to 31)),fromSLV(islv(32 to 32)),fromSLV(islv(33 to 35)),fromSLV(islv(36 to 55)),fromSLV(islv(56 to 75)),fromSLV(islv(76 to 87)),fromSLV(islv(88 to 99)),fromSLV(islv(100 to 111)),fromSLV(islv(112 to 114)),fromSLV(islv(115 to 115)));
  end;
  function toSLV (p : contranomy_types.rvficsr) return std_logic_vector is
  begin
    return (toSLV(p.rvficsr_sel0_rmask) & toSLV(p.rvficsr_sel1_wmask) & toSLV(p.rvficsr_sel2_rdata) & toSLV(p.rvficsr_sel3_wdata));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.rvficsr is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 63)),fromSLV(islv(64 to 95)),fromSLV(islv(96 to 127)));
  end;
  function toSLV (p : contranomy_types.rvfi) return std_logic_vector is
  begin
    return (toSLV(p.rvfi_sel0_valid) & toSLV(p.rvfi_sel1_order) & toSLV(p.rvfi_sel2_insn) & toSLV(p.rvfi_sel3_trap) & toSLV(p.rvfi_sel4_halt) & toSLV(p.rvfi_sel5_intr) & toSLV(p.rvfi_sel6_mode) & toSLV(p.rvfi_sel7_ixl) & toSLV(p.rvfi_sel8_rs1addr) & toSLV(p.rvfi_sel9_rs2addr) & toSLV(p.rvfi_sel10_rs1rdata) & toSLV(p.rvfi_sel11_rs2rdata) & toSLV(p.rvfi_sel12_rdaddr) & toSLV(p.rvfi_sel13_rdwdata) & toSLV(p.rvfi_sel14_pcrdata) & toSLV(p.rvfi_sel15_pcwdata) & toSLV(p.rvfi_sel16_memaddr) & toSLV(p.rvfi_sel17_memrmask) & toSLV(p.rvfi_sel18_memwmask) & toSLV(p.rvfi_sel19_memrdata) & toSLV(p.rvfi_sel20_memwdata) & toSLV(p.rvfi_sel21_misacsr));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.rvfi is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 64)),fromSLV(islv(65 to 96)),fromSLV(islv(97 to 97)),fromSLV(islv(98 to 98)),fromSLV(islv(99 to 99)),fromSLV(islv(100 to 101)),fromSLV(islv(102 to 103)),fromSLV(islv(104 to 108)),fromSLV(islv(109 to 113)),fromSLV(islv(114 to 145)),fromSLV(islv(146 to 177)),fromSLV(islv(178 to 182)),fromSLV(islv(183 to 214)),fromSLV(islv(215 to 246)),fromSLV(islv(247 to 278)),fromSLV(islv(279 to 310)),fromSLV(islv(311 to 314)),fromSLV(islv(315 to 318)),fromSLV(islv(319 to 350)),fromSLV(islv(351 to 382)),fromSLV(islv(383 to 510)));
  end;
  function toSLV (p : contranomy_types.tup2_7) return std_logic_vector is
  begin
    return (toSLV(p.tup2_7_sel0_register_r) & toSLV(p.tup2_7_sel1_std_logic_vector));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_7 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 4)),fromSLV(islv(5 to 36)));
  end;
  function toSLV (p : contranomy_types.tup2_8) return std_logic_vector is
  begin
    return (toSLV(p.tup2_8_sel0_std_logic_vector) & toSLV(p.tup2_8_sel1_csrtype));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_8 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 33)));
  end;
  function toSLV (p : contranomy_types.wishbonem2s) return std_logic_vector is
  begin
    return (toSLV(p.wishbonem2s_sel0_addr) & toSLV(p.wishbonem2s_sel1_writedata) & toSLV(p.wishbonem2s_sel2_busselect) & toSLV(p.wishbonem2s_sel3_buscycle) & toSLV(p.wishbonem2s_sel4_strobe) & toSLV(p.wishbonem2s_sel5_writeenable) & toSLV(p.wishbonem2s_sel6_cycletypeidentifier) & toSLV(p.wishbonem2s_sel7_bursttypeextension));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.wishbonem2s is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 29)),fromSLV(islv(30 to 61)),fromSLV(islv(62 to 65)),fromSLV(islv(66 to 66)),fromSLV(islv(67 to 67)),fromSLV(islv(68 to 68)),fromSLV(islv(69 to 71)),fromSLV(islv(72 to 73)));
  end;
  function toSLV (p : contranomy_types.coreout) return std_logic_vector is
  begin
    return (toSLV(p.coreout_sel0_ibusm2s) & toSLV(p.coreout_sel1_dbusm2s));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.coreout is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 73)),fromSLV(islv(74 to 147)));
  end;
  function toSLV (p : contranomy_types.tup2_3) return std_logic_vector is
  begin
    return (toSLV(p.tup2_3_sel0_boolean) & toSLV(p.tup2_3_sel1_std_logic_vector));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 30)));
  end;
  function toSLV (p : contranomy_types.tup2_5) return std_logic_vector is
  begin
    return (toSLV(p.tup2_5_sel0_std_logic_vector_0) & toSLV(p.tup2_5_sel1_std_logic_vector_1));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_5 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 29)),fromSLV(islv(30 to 31)));
  end;
  function toSLV (p : contranomy_types.clash_internal_1) return std_logic_vector is
  begin
    return (toSLV(p.clash_internal_1_sel0_corestage) & toSLV(p.clash_internal_1_sel1_std_logic_vector));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1)),fromSLV(islv(2 to 31)));
  end;
  function toSLV (p : contranomy_types.corein) return std_logic_vector is
  begin
    return (toSLV(p.corein_sel0_ibuss2m) & toSLV(p.corein_sel1_dbuss2m) & toSLV(p.corein_sel2_timerinterrupt) & toSLV(p.corein_sel3_softwareinterrupt) & toSLV(p.corein_sel4_externalinterrupt));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.corein is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 33)),fromSLV(islv(34 to 67)),fromSLV(islv(68 to 68)),fromSLV(islv(69 to 69)),fromSLV(islv(70 to 101)));
  end;
  function toSLV (p : contranomy_types.mie) return std_logic_vector is
  begin
    return (toSLV(p.mie_sel0_meie) & toSLV(p.mie_sel1_mtie) & toSLV(p.mie_sel2_msie));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mie is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 2)));
  end;
  function toSLV (s : in signed) return std_logic_vector is
  begin
    return std_logic_vector(s);
  end;
  function fromSLV (slv : in std_logic_vector) return signed is
    alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return signed(islv);
  end;
  function toSLV (p : contranomy_types.mcause) return std_logic_vector is
  begin
    return (toSLV(p.mcause_sel0_interrupt) & toSLV(p.mcause_sel1_code));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.mcause is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 4)));
  end;
  function toSLV (p : contranomy_types.tup2_6) return std_logic_vector is
  begin
    return (toSLV(p.tup2_6_sel0_std_logic_vector_0) & toSLV(p.tup2_6_sel1_std_logic_vector_1));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_6 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 63)));
  end;
  function toSLV (p : contranomy_types.tup2) return std_logic_vector is
  begin
    return (toSLV(p.tup2_sel0_corein) & toSLV(p.tup2_sel1_tup2_6));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 101)),fromSLV(islv(102 to 165)));
  end;
  function toSLV (p : contranomy_types.machinestate) return std_logic_vector is
  begin
    return (toSLV(p.machinestate_sel0_mstatus) & toSLV(p.machinestate_sel1_mcause) & toSLV(p.machinestate_sel2_mtvec) & toSLV(p.machinestate_sel3_mie) & toSLV(p.machinestate_sel4_mscratch) & toSLV(p.machinestate_sel5_mepc) & toSLV(p.machinestate_sel6_mtval) & toSLV(p.machinestate_sel7_irqmask));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.machinestate is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1)),fromSLV(islv(2 to 6)),fromSLV(islv(7 to 38)),fromSLV(islv(39 to 41)),fromSLV(islv(42 to 73)),fromSLV(islv(74 to 103)),fromSLV(islv(104 to 135)),fromSLV(islv(136 to 167)));
  end;
  function toSLV (p : contranomy_types.clash_internal_0) return std_logic_vector is
  begin
    return (toSLV(p.clash_internal_0_sel0_machinestate) & toSLV(p.clash_internal_0_sel1_unsigned));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 167)),fromSLV(islv(168 to 231)));
  end;
  function toSLV (p : contranomy_types.clash_internal) return std_logic_vector is
  begin
    return (toSLV(p.clash_internal_sel0_std_logic_vector) & toSLV(p.clash_internal_sel1_clash_internal_0));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.clash_internal is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 31)),fromSLV(islv(32 to 263)));
  end;
  function toSLV (p : contranomy_types.corestate) return std_logic_vector is
  begin
    return (toSLV(p.corestate_sel0_stage) & toSLV(p.corestate_sel1_pc) & toSLV(p.corestate_sel2_instruction) & toSLV(p.corestate_sel3_machinestate) & toSLV(p.corestate_sel4_rvfiorder));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.corestate is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 1)),fromSLV(islv(2 to 31)),fromSLV(islv(32 to 63)),fromSLV(islv(64 to 231)),fromSLV(islv(232 to 295)));
  end;
  function toSLV (p : contranomy_types.tup2_4) return std_logic_vector is
  begin
    return (toSLV(p.tup2_4_sel0_tup2_3) & toSLV(p.tup2_4_sel1_corestate));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_4 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 30)),fromSLV(islv(31 to 326)));
  end;
  function toSLV (p : contranomy_types.tup3_0) return std_logic_vector is
  begin
    return (toSLV(p.tup3_0_sel0_maybe_1_0) & toSLV(p.tup3_0_sel1_maybe_1_1) & toSLV(p.tup3_0_sel2_maybe));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup3_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 5)),fromSLV(islv(6 to 11)),fromSLV(islv(12 to 49)));
  end;
  function toSLV (p : contranomy_types.tup3) return std_logic_vector is
  begin
    return (toSLV(p.tup3_sel0_coreout) & toSLV(p.tup3_sel1_tup3_0) & toSLV(p.tup3_sel2_rvfi));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup3 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 147)),fromSLV(islv(148 to 197)),fromSLV(islv(198 to 708)));
  end;
  function toSLV (p : contranomy_types.tup2_0) return std_logic_vector is
  begin
    return (toSLV(p.tup2_0_sel0_tup3) & toSLV(p.tup2_0_sel1_corestate));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_0 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 708)),fromSLV(islv(709 to 1004)));
  end;
  function toSLV (p : contranomy_types.tup5) return std_logic_vector is
  begin
    return (toSLV(p.tup5_sel0_wishbonem2s) & toSLV(p.tup5_sel1_maybe_0_0) & toSLV(p.tup5_sel2_maybe_0_1) & toSLV(p.tup5_sel3_maybe_0_2) & toSLV(p.tup5_sel4_boolean));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup5 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 73)),fromSLV(islv(74 to 106)),fromSLV(islv(107 to 139)),fromSLV(islv(140 to 172)),fromSLV(islv(173 to 173)));
  end;
  function toSLV (p : contranomy_types.exception_in) return std_logic_vector is
  begin
    return (toSLV(p.ei_instraccessfault) & toSLV(p.ei_instr_addr_misaligned) & toSLV(p.ei_instr_illegal) & toSLV(p.ei_data_access_fault) & toSLV(p.ei_data_addr_misaligned) & toSLV(p.ei_timer_interrupt) & toSLV(p.ei_software_interrupt) & toSLV(p.ei_external_interrupt));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.exception_in is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 0)),fromSLV(islv(1 to 1)),fromSLV(islv(2 to 2)),fromSLV(islv(3 to 35)),fromSLV(islv(36 to 68)),fromSLV(islv(69 to 69)),fromSLV(islv(70 to 70)),fromSLV(islv(71 to 102)));
  end;
  function toSLV (p : contranomy_types.tup2_1) return std_logic_vector is
  begin
    return (toSLV(p.tup2_1_sel0_maybe_0) & toSLV(p.tup2_1_sel1_std_logic_vector));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_1 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 32)),fromSLV(islv(33 to 64)));
  end;
  function toSLV (p : contranomy_types.tup2_2) return std_logic_vector is
  begin
    return (toSLV(p.tup2_2_sel0_tup2_1) & toSLV(p.tup2_2_sel1_machinestate));
  end;
  function fromSLV (slv : in std_logic_vector) return contranomy_types.tup2_2 is
  alias islv : std_logic_vector(0 to slv'length - 1) is slv;
  begin
    return (fromSLV(islv(0 to 64)),fromSLV(islv(65 to 232)));
  end;

  -- constants
  constant INSTRUCTION_ADDRESS_MISALIGNED : mcause := (False, "0000");
  constant INSTRUCTION_ACCESS_FAULT       : mcause := (False, "0001");
  constant ILLEGAL_INSTRUCTION            : mcause := (False, "0010");
  constant BREAKPOINT                     : mcause := (False, "0011");
  constant LOAD_ADDRESS_MISALIGNED        : mcause := (False, "0100");
  constant LOAD_ACCESS_FAULT              : mcause := (False, "0101");
  constant STORE_ADDRESS_MISALIGNED       : mcause := (False, "0110");
  constant STORE_ACCESS_FAULT             : mcause := (False, "0111");
  constant ENVIRONMENT_CALL               : mcause := (False, "1011");
  constant MACHINE_SOFTWARE_INTERRUPT     : mcause := (True,  "0011");
  constant MACHINE_TIMER_INTERRUPT        : mcause := (True,  "0111");
  constant MACHINE_EXTERNAL_INTERRUPT     : mcause := (True,  "1011");

  -- opcode
  subtype opcode is std_logic_vector(6 downto 0);

  constant LUI      : opcode := "0110111";
  constant AUIPC    : opcode := "0010111";
  constant JAL      : opcode := "1101111";
  constant JALR     : opcode := "1100111";
  constant BRANCH   : opcode := "1100011";
  constant LOAD     : opcode := "0000011";
  constant STORE    : opcode := "0100011";
  constant OP_IMM   : opcode := "0010011";
  constant OP       : opcode := "0110011";
  constant MISC_MEM : opcode := "0001111";
  constant SYSTEM   : opcode := "1110011";


  -- Load Store Width

  subtype lsw is std_logic_vector(2 downto 0);

  constant LSW_UBYTE : lsw := "100";
  constant LSW_SBYTE : lsw := "000";
  constant LSW_UHALF : lsw := "101";
  constant LSW_SHALF : lsw := "001";
  constant LSW_WORD  : lsw := "010";
  -- constant LSW_ILL1  : lsw := "011";
  -- constant LSW_ILL2  : lsw := "110";
  -- constant LSW_ILL3  : lsw := "111";

  -- Branch Condition

  subtype branch_cond is std_logic_vector(2 downto 0);

  constant BC_EQ   : branch_cond := "000";
  constant BC_NE   : branch_cond := "001";
  constant BC_LT   : branch_cond := "010";
  constant BC_GE   : branch_cond := "011";
  constant BC_LTU  : branch_cond := "100";
  constant BC_GEU  : branch_cond := "101";
  constant BC_ILL1 : branch_cond := "110";
  constant BC_ILL2 : branch_cond := "111";

  -- Machine Trap Setup

  subtype csr_register is std_logic_vector(11 downto 0);

  constant CSR_MSTATUS    : csr_register := x"300"; -- Machine status register
  constant CSR_MISA       : csr_register := x"301"; -- ISA and extensions
  constant CSR_MEDELEG    : csr_register := x"302"; -- Machine exception delegation register
  constant CSR_MIDELEG    : csr_register := x"303"; -- Machine interrupt delegation register
  constant CSR_MIE        : csr_register := x"304"; -- Machine interrupt enable register
  constant CSR_MTVEC      : csr_register := x"305"; -- Machine trap-handler base address
  constant CSR_MCOUNTEREN : csr_register := x"306"; -- Machine counter enable
  constant CSR_MSTATUSH   : csr_register := x"307"; -- Additional machine status register, RV32 only

  -- Machine Trap Handling

  constant TRAP_MSCRATCH : csr_register := x"340"; -- Scratch register for machine trap handlers
  constant TRAP_MEPC     : csr_register := x"341"; -- Machine exception program counter
  constant TRAP_MCAUSE   : csr_register := x"342"; -- Machine trap cause
  constant TRAP_MTVAL    : csr_register := x"343"; -- Machine bad address instruction
  constant TRAP_MIP      : csr_register := x"344"; -- Machine interrupt pending
  constant TRAP_MTINST   : csr_register := x"34A"; -- Machine trap instruction (transformed)
  constant TRAP_MTVAL2   : csr_register := x"34B"; -- Machine bad guest physical address

  -- Architecture-specific Registers

  constant IRQMASK    : csr_register := x"330";
  constant IEQPENDING : csr_register := x"360";

  -- CSR Type

  subtype csr_type is std_logic_vector(1 downto 0);

  constant CSR_ILL    : csr_type := "00";
  constant READ_WRITE : csr_type := "01";
  constant READ_SET   : csr_type := "10";
  constant READ_CLEAR : csr_type := "11";

  -- System12

  subtype system_12 is std_logic_vector(11 downto 0);

  constant ECALL  : system_12 := (others => '0');
  constant EBREAK : system_12 := (0 => '1', others => '0');
  constant MRET   : system_12 := "001100000010";

  -- Wishbone Cycle Type Identifier

  subtype cycle_type_identifier is std_logic_vector(2 downto 0);

  constant CLASSIC                : cycle_type_identifier := "000";
  constant CONSTANT_ADDRESS_BURST : cycle_type_identifier := "001";
  constant INCREMENTING_BURST     : cycle_type_identifier := "010";
  constant END_OF_BURST           : cycle_type_identifier := "111";

end;


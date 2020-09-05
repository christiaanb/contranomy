{-# LANGUAGE PatternSynonyms #-}

module Contranomy.Instruction
  ( Opcode
      ( LUI
      , AUIPC
      , JAL
      , JALR
      , BRANCH
      , LOAD
      , STORE
      , OP_IMM
      , OP
      , MISC_MEM
      , SYSTEM
      )
  , ShiftRight (..)
  , IOp (..)
  , MOp (..)
  , MachineWord
  , Register (..)
  , MCause
      ( ..
      , INSTRUCTION_ADDRESS_MISALIGNED
      , INSTRUCTION_ACCESS_FAULT
      , ILLEGAL_INSTRUCTION
      , BREAKPOINT
      , LOAD_ADDRESS_MISALIGNED
      , LOAD_ACCESS_FAULT
      , STORE_ADDRESS_MISALIGNED
      , STORE_ACCESS_FAULT
      )
  , LoadStoreWidth (..)
  , BranchCondition
      ( BEQ
      , BNE
      , BLT
      , BGE
      , BLTU
      , BGEU
      )
  , CSRRegister
      ( MSTATUS, MISA, MEDELEG, MIDELEG, MIE, MTVEC, MCOUNTEREN, MSTATUSH
      , MSCRATCH, MEPC, MCAUSE, MTVAL, MIP, MTINST, MTVAL2
      ,.. )
  , CSRType
      ( ReadWrite
      , ReadSet
      , ReadClear
      )
  )
where

import Data.Function

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Prelude

import Contranomy.Clash.Extra

type MachineWord = BitVector 32

-- | Register 1-31 are general-purpose registers holding integer
-- values.
--
-- Register 0 is hardwired to the constant 0.
data Register
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | X31
  deriving (Show, Generic, NFDataX)

-- Use custom bit-representation, so we can get a TH-derived bitpack instance,
-- which significantly reduces generated Verilog
deriveDefaultAnnotation [t|Register|]
deriveBitPack  [t|Register|]

-- We cannot use a derived Enum because it use dataToTag/tagToEnum, which is
-- invalid for types with custom bit representations
instance Eq Register where
  (==) = (==) `on` pack
  {-# INLINE (==) #-}
  (/=) = (/=) `on` pack
  {-# INLINE (/=) #-}

instance Ord Register where
  (<) = (<) `on` pack
  {-# INLINE (<) #-}
  (<=) = (<=) `on` pack
  {-# INLINE (<=) #-}
  (>) = (<) `on` pack
  {-# INLINE (>) #-}
  (>=) = (>=) `on` pack
  {-# INLINE (>=) #-}

instance Enum Register where
  toEnum = unpack . toEnum
  {-# INLINE toEnum #-}
  fromEnum = fromEnum . pack
  {-# INLINE fromEnum #-}

data MCause
  = MCause
  { interrupt :: Bool
  , code :: BitVector 4
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MCause

pattern INSTRUCTION_ADDRESS_MISALIGNED, INSTRUCTION_ACCESS_FAULT, ILLEGAL_INSTRUCTION,
  BREAKPOINT, LOAD_ADDRESS_MISALIGNED, LOAD_ACCESS_FAULT, STORE_ADDRESS_MISALIGNED,
  STORE_ACCESS_FAULT :: MCause
pattern INSTRUCTION_ADDRESS_MISALIGNED = MCause False 0
pattern INSTRUCTION_ACCESS_FAULT = MCause False 1
pattern ILLEGAL_INSTRUCTION = MCause False 2
pattern BREAKPOINT = MCause False 3
pattern LOAD_ADDRESS_MISALIGNED = MCause False 4
pattern LOAD_ACCESS_FAULT = MCause False 5
pattern STORE_ADDRESS_MISALIGNED = MCause False 6
pattern STORE_ACCESS_FAULT = MCause False 7

data Opcode
  = LUI
  | AUIPC
  | JAL
  | JALR
  | BRANCH
  | LOAD
  | STORE
  | OP_IMM
  | OP
  | MISC_MEM
  | SYSTEM
  | Illegal -- Not exported
  deriving (Show)

{-# ANN module (DataReprAnn
                  $(liftQ [t|Opcode|])
                  7
                  [ ConstrRepr 'LUI      (6 `downto` 0) 0b0110111 []
                  , ConstrRepr 'AUIPC    (6 `downto` 0) 0b0010111 []
                  , ConstrRepr 'JAL      (6 `downto` 0) 0b1101111 []
                  , ConstrRepr 'JALR     (6 `downto` 0) 0b1100111 []
                  , ConstrRepr 'BRANCH   (6 `downto` 0) 0b1100011 []
                  , ConstrRepr 'LOAD     (6 `downto` 0) 0b0000011 []
                  , ConstrRepr 'STORE    (6 `downto` 0) 0b0100011 []
                  , ConstrRepr 'OP_IMM   (6 `downto` 0) 0b0010011 []
                  , ConstrRepr 'OP       (6 `downto` 0) 0b0110011 []
                  , ConstrRepr 'MISC_MEM (6 `downto` 0) 0b0001111 []
                  , ConstrRepr 'SYSTEM   (6 `downto` 0) 0b1110011 []
                  , ConstrRepr 'Illegal  0              0         []
                  ]) #-}
deriveBitPack [t| Opcode |]

data ShiftRight
  = Logical
  | Arithmetic

deriveDefaultAnnotation [t|ShiftRight|]
deriveBitPack  [t|ShiftRight|]

data IOp
  = ADD  -- 0
  | SLL  -- 1
  | SLT  -- 2
  | SLTU -- 3
  | XOR  -- 4
  | SR   -- 5
  | OR   -- 6
  | AND  -- 7

deriveDefaultAnnotation [t|IOp|]
deriveBitPack  [t|IOp|]

data MOp
  = MUL     -- 0
  | MULH    -- 1
  | MULHSU  -- 2
  | MULHU   -- 3
  | DIV     -- 4
  | DIVU    -- 5
  | REM     -- 6
  | REMU    -- 7

deriveDefaultAnnotation [t|MOp|]
deriveBitPack  [t|MOp|]

data LoadStoreWidth
  = Byte
  | Half
  | Word

deriveDefaultAnnotation [t|LoadStoreWidth|]
deriveBitPack  [t|LoadStoreWidth|]

data BranchCondition
  = BEQ
  | BNE
  | BLT
  | BGE
  | BLTU
  | BGEU
  | BIllegal
{-# ANN module (DataReprAnn
                  $(liftQ [t|BranchCondition|])
                  3
                  [ ConstrRepr 'BEQ      (2 `downto` 0) 0b000 []
                  , ConstrRepr 'BNE      (2 `downto` 0) 0b001 []
                  , ConstrRepr 'BLT      (2 `downto` 0) 0b100 []
                  , ConstrRepr 'BGE      (2 `downto` 0) 0b101 []
                  , ConstrRepr 'BLTU     (2 `downto` 0) 0b110 []
                  , ConstrRepr 'BGEU     (2 `downto` 0) 0b111 []
                  , ConstrRepr 'BIllegal 0              0     []
                  ]) #-}
deriveBitPack [t| BranchCondition |]


newtype CSRRegister = CSRRegister (BitVector 12)

-- Machine Trap Setup
pattern MSTATUS, MISA, MEDELEG, MIDELEG, MIE, MTVEC, MCOUNTEREN, MSTATUSH :: CSRRegister

pattern MSTATUS    = CSRRegister 0x300 -- Machine status register
pattern MISA       = CSRRegister 0x301 -- ISA and extensions
pattern MEDELEG    = CSRRegister 0x302 -- Machine exception delegation register
pattern MIDELEG    = CSRRegister 0x303 -- Machine interrupt delegation register
pattern MIE        = CSRRegister 0x304 -- Machine interrupt enable register
pattern MTVEC      = CSRRegister 0x305 -- Machine trap-handler base address
pattern MCOUNTEREN = CSRRegister 0x306 -- Machine counter enable
pattern MSTATUSH   = CSRRegister 0x307 -- Additional machine status register, RV32 only

-- Machine Trap Handling
pattern MSCRATCH, MEPC, MCAUSE, MTVAL, MIP, MTINST, MTVAL2 :: CSRRegister

pattern MSCRATCH = CSRRegister 0x340 -- Scratch register for machine trap handlers
pattern MEPC     = CSRRegister 0x341 -- Machine exception program counter
pattern MCAUSE   = CSRRegister 0x342 -- Machine trap cause
pattern MTVAL    = CSRRegister 0x343 -- Machine bad address instruction
pattern MIP      = CSRRegister 0x344 -- Machine interrupt pending
pattern MTINST   = CSRRegister 0x34A -- Machine trap instruction (transformed)
pattern MTVAL2   = CSRRegister 0x34B -- Machine bad guest physical address

data CSRType
  = ReadWrite
  | ReadSet
  | ReadClear
  | CSRIllegal
{-# ANN module (DataReprAnn
                  $(liftQ [t|CSRType|])
                  2
                  [ ConstrRepr 'ReadWrite  (1 `downto` 0) 0b01 []
                  , ConstrRepr 'ReadSet    (1 `downto` 0) 0b10 []
                  , ConstrRepr 'ReadClear  (1 `downto` 0) 0b11 []
                  , ConstrRepr 'CSRIllegal 0              0    []
                  ]) #-}
deriveBitPack [t| CSRType |]

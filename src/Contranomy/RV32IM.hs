{-|
Copyright Moritz Kiefer (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Moritz Kiefer nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Contranomy.RV32IM
  ( Register(..)
  , CSRRegister
      ( MSTATUS, MISA, MEDELEG, MIDELEG, MIE, MTVEC, MCOUNTEREN, MSTATUSH
      , MSCRATCH, MEPC, MCAUSE, MTVAL, MIP, MTINST, MTVAL2
      ,.. )
  , Instr(..)
  -- * Integer Register-Immediate Instructions
  , RegisterImmediateInstr(..)
  , IOpcode(..)
  , ShiftOpcode(..)
  -- * Integer Register-Register Instructions
  , RegisterRegisterInstr(..)
  , ROpcode(..)
  -- * Control Transfer Instructions
  , JumpInstr(..)
  , BranchInstr(..)
  , BranchCond(..)
  -- * Load and Store Instructions
  , MemoryInstr(..)
  , Width(..)
  , LoadWidth(..)
  -- * Memory Synchronization Instructions
  , SynchronizationInstr(..)
  , SyncOrdering(..)
  -- * Control and Status Register Instructions
  , CSRInstr(..)
  , CSRType(..)
  -- * Environment Call and Breakpoints
  , EnvironmentInstr(..)
  -- * Word Types
  , Word5
  , Word12
  , Word20
  ) where

import Clash.Class.AutoReg (AutoReg)
import Clash.Prelude
import Clash.Annotations.BitRepresentation.Deriving

data Instr
  = BranchInstr !BranchInstr
  | CSRInstr !CSRInstr
  | EnvironmentInstr !EnvironmentInstr
  | JumpInstr !JumpInstr
  | MemoryInstr !MemoryInstr
  | RRInstr !RegisterRegisterInstr
  | RIInstr !RegisterImmediateInstr
  | SyncInstr !SynchronizationInstr
  deriving (Show, Eq, Ord, Generic, NFDataX, AutoReg)

-- R: 7, 5, 5, 3, 5, 7
-- M: 7, 5, 5, 3, 5, 7

data JumpInstr
  = JAL { imm  :: !Word20
        , dest :: !Register
        }
  | JALR { offset :: !Word12
         , base   :: !Register
         , dest   :: !Register
         }
  deriving (Show, Eq, Ord, Generic, NFDataX)

data BranchCond
  = BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  deriving (Show, Eq, Ord, Generic, NFDataX)

data BranchInstr
  = Branch
  { imm  :: !Word12
  , cond :: !BranchCond
  , src2 :: !Register
  , src1 :: !Register
  }
  deriving (Show, Eq, Ord, Generic, NFDataX)

data LoadWidth
  = Width !Width
  | HalfUnsigned
  | ByteUnsigned
  deriving (Show, Eq, Ord, Generic, NFDataX)

data Width
  = Byte
  | Half
  | Word
  deriving (Show, Eq, Ord, Generic, NFDataX)

data MemoryInstr
  = LOAD { loadWidth  :: !LoadWidth
         , offset :: !Word12
         , base   :: !Register
         , dest   :: !Register
         }
  | STORE { width  :: !Width
          , offset :: !Word12
          , src    :: !Register
          , base   :: !Register
          }
  deriving (Show, Eq, Ord, Generic, NFDataX)

data IOpcode
  = ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  deriving (Show, Eq, Ord, Generic, NFDataX)

data ShiftOpcode
  = SLLI
  | SRLI
  | SRAI
  deriving (Show, Eq, Ord, Generic, NFDataX)

data RegisterImmediateInstr
  = IInstr { iOpcode :: !IOpcode
           , imm12   :: !Word12
           , src     :: !Register
           , dest    :: !Register
           }
  | ShiftInstr { sOpcode :: !ShiftOpcode
               , shamt   :: !Word5
               , src     :: !Register
               , dest    :: !Register
               }
  | LUI { imm20 :: !Word20
        , dest  :: !Register
        }
  | AUIPC { imm20 :: !Word20
          , dest  :: !Register
          }
  deriving (Show, Eq, Ord, Generic, NFDataX)

data ROpcode
  = ADD
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SUB
  | SRA
  | MUL
  | MULH
  | MULHSU
  | MULHU
  | DIV
  | DIVU
  | REM
  | REMU
  deriving (Show, Eq, Ord, Generic, NFDataX)

data RegisterRegisterInstr
  = RInstr
  { opcode :: !ROpcode
  , src2   :: !Register
  , src1   :: !Register
  , dest   :: !Register
  }
  deriving (Show, Eq, Ord, Generic, NFDataX)

data SyncOrdering = SyncOrd
  { deviceInput :: !Bool
  , deviceOutput :: !Bool
  , memoryReads :: !Bool
  , memoryWrites :: !Bool
  } deriving (Show, Eq, Ord, Generic, NFDataX)

data SynchronizationInstr
  = FENCE !SyncOrdering
          !SyncOrdering
  | FENCEI
  deriving (Show, Eq, Ord, Generic, NFDataX)

data EnvironmentInstr
  = ECALL
  | EBREAK
  deriving (Show, Eq, Ord, Generic, NFDataX)

-- | Control and status register instruction type
data CSRType
  = ReadWrite
  | ReadSet
  | ReadClear
  deriving (Show, Eq, Ord, Generic, NFDataX)


newtype CSRRegister = CSRRegister Word12
  deriving stock (Show, Eq, Ord)
  deriving newtype (NFDataX)

-- | Control and Status Register Instructions
data CSRInstr
  = CSRRInstr { csrType :: !CSRType
              , csr     :: !CSRRegister
              , src     :: !Register
              , dest    :: !Register
              }
  | CSRIInstr { csrType :: !CSRType
              , csr     :: !CSRRegister
              , imm     :: !Word5
              , dest    :: !Register
              }
  deriving (Show, Eq, Ord, Generic, NFDataX)

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
  deriving (Show, Eq, Ord, Generic, NFDataX)

-- We cannot use a derived Enum because it use dataToTag/tagToEnum, which is
-- invalid for types with custom bit representations
instance Enum Register where
  toEnum = unpack . toEnum
  fromEnum = fromEnum . pack

type Word5 = BitVector 5
type Word12 = BitVector 12
type Word20 = BitVector 20

-- Use custom bit-representation, so we can get a TH-derived bitpack instance,
-- which significantly reduces generated Verilog
deriveDefaultAnnotation [t|Register|]
deriveBitPack  [t|Register|]

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

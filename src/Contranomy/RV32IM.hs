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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Contranomy.RV32IM
  ( Register(..)
  , CSRRegister(..)
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

import Clash.Prelude

data Instr
  = BranchInstr !BranchInstr
  | CSRInstr !CSRInstr
  | EnvironmentInstr !EnvironmentInstr
  | JumpInstr !JumpInstr
  | MemoryInstr !MemoryInstr
  | RRInstr !RegisterRegisterInstr
  | RIInstr !RegisterImmediateInstr
  | SyncInstr !SynchronizationInstr
  deriving (Show, Eq, Ord, Generic, NFDataX)

-- R: 7, 5, 5, 3, 5, 7
-- M: 7, 5, 5, 3, 5, 7

data JumpInstr
  = JAL !Word20
        !Register
  | JALR !Word12
         !Register
         !Register
  deriving (Show, Eq, Ord, Generic, NFDataX)

data BranchCond
  = BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  deriving (Show, Eq, Ord, Generic, NFDataX)

data BranchInstr =
  Branch !Word12
         !BranchCond
         !Register
         !Register
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
  = LOAD !LoadWidth
         !Word12
         !Register
         !Register
  | STORE !Width
          !Word12
          !Register
          !Register
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
  = IInstr !IOpcode
           !Word12
           !Register
           !Register
  | ShiftInstr !ShiftOpcode
               !Word5
               !Register
               !Register
  | LUI !Word20
        !Register
  | AUIPC !Word20
          !Register
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

data RegisterRegisterInstr =
  RInstr !ROpcode
         !Register
         !Register
         !Register
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
  = CSRRInstr !CSRType
              !CSRRegister
              !Register
              !Register
  | CSRIInstr !CSRType
              !CSRRegister
              !Word5
              !Register
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
  deriving (Show, Eq, Ord, Generic, BitPack, NFDataX)

type Word5 = Unsigned 5
type Word12 = Unsigned 12
type Word20 = Unsigned 20

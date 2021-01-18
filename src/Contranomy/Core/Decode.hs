{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Contranomy.Core.Decode where

import Clash.Prelude

import Contranomy.Instruction
import Contranomy.Core.SharedTypes

data DecodedInstruction
  = DecodedInstruction
  { opcode :: Opcode
  , rd     :: Register
  , rs1    :: Register
  , rs2    :: Register
  , iop    :: IOp
  , srla   :: ShiftRight
  , shamt  :: BitVector 5
  , isSub  :: Bool
  , isM    :: Bool
  , mop    :: MOp
  , imm20U :: BitVector 20
  , imm20J :: BitVector 20
  , imm12I :: BitVector 12
  , imm12S :: BitVector 12
  , imm12B :: BitVector 12
  , func3  :: BitVector 3
  , legal  :: Bool
  }

{-# NOINLINE decodeInstruction #-}
-- | This functions extracts the relavents bits out of an instruction for the
-- different functional units of the CPU. In addition it checks whether the
-- instruction is legal and can be executed by the CPU.
decodeInstruction ::
  MachineWord ->
  DecodedInstruction
decodeInstruction w
  = DecodedInstruction
  { opcode = opcode
  , rd     = unpack (slice d11 d7 w)
  , rs1    = unpack (slice d19 d15 w)
  , rs2    = unpack (slice d24 d20 w)
  , iop    = unpack (slice d14 d12 w)
  , srla   = unpack (slice d30 d30 w)
  , shamt  = unpack (slice d24 d20 w)
  , isSub  = unpack (slice d30 d30 w)
  , isM    = unpack (slice d25 d25 w)
  , mop    = unpack (slice d14 d12 w)
  , imm20U = slice d31 d12 w
  , imm20J = slice d31 d31 w ++#
             slice d19 d12 w ++#
             slice d20 d20 w ++#
             slice d30 d21 w

  , imm12I = slice d31 d20 w
  , imm12S = slice d31 d25 w ++# slice d11 d7 w
  , imm12B = slice d31 d31 w ++#
             slice  d7  d7 w ++#
             slice d30 d25 w ++#
             slice d11  d8 w
  , func3  = func3
  , legal  = case opcode of
      LUI -> True
      AUIPC -> True
      JAL -> True
      JALR -> func3 == 0
      BRANCH -> case unpack func3 of
        BEQ -> True
        BNE -> True
        BLT -> True
        BGE -> True
        BLTU -> True
        BGEU -> True
        _ -> False
      LOAD -> case unpack func3 of
        Byte _ -> True
        Half _ -> True
        Word -> True
        _ -> False
      STORE -> case unpack func3 of
        Byte Signed -> True
        Half Signed -> True
        Word -> True
        _ -> False
      OP_IMM -> case unpack func3 of
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        _ -> True
      OP -> case unpack func3 of
        ADD -> func7 == 0 ||        -- ADD
               func7 == 0b010_0000  -- SUB
        SLL -> func7 == 0
        SLT -> func7 == 0
        SLTU -> func7 == 0
        XOR -> func7 == 0
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        OR -> func7 == 0
        AND -> func7 == 0
      MISC_MEM -> func3 == 1
      SYSTEM -> case func3 of
        0 -> case System12 func12 of
          ECALL -> True
          EBREAK -> True
          MRET -> True
          _ -> False
        _ -> case unpack (slice d1 d0 func3) of
          ReadWrite -> True
          ReadSet -> True
          ReadClear -> True
          _ -> False
      _ -> False
  }
 where
  opcode = unpack (slice d6 d0 w)
  func3  = slice d14 d12 w
  func7  = slice d31 d25 w
  func12 = slice d31 d20 w

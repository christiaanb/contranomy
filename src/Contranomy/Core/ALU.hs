{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.ALU where

import Clash.Prelude

import Contranomy.Clash.Extra
import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

{-# NOINLINE alu #-}
-- | The ALU implements the following instructions, or at least parts of them
--
-- * LUI
-- * AUIPC
--
-- * ADD/SUB/ADDI
-- * SLL/SLLI
-- * SLT/SLTI
-- * SLTU/SLTUI
-- * XOR/XORI
-- * SRL/SRLI
-- * SRA/SRAI
-- * OR/ORI
-- * AND/ANDI
--
-- It is additionally used to calculate the PC after the PC of the current instruction for:
--
-- * JAL
-- * JALR
--
-- And it performs the address calculation for the Load and Store instructions
--
-- * LOAD
-- * STORE
alu ::
  -- | Instruction
  MachineWord ->
  -- | Program counter
  PC ->
  -- | Value of RS1
  MachineWord ->
  -- | Value of RS2
  MachineWord ->
  -- | Result
  MachineWord
alu instruction pc rs1Value rs2Value = aluResult
 where
  DecodedInstruction
    { opcode, iop, srla, isSub, imm12I, imm20U, imm12S }
    = decodeInstruction instruction

  aluArg1 = case opcode of
              LUI   -> 0
              AUIPC -> pc ++# 0
              JAL   -> pc ++# 0
              JALR  -> pc ++# 0
              _     -> rs1Value
  aluArg2 = case opcode of
              LUI    -> imm20U ++# 0
              AUIPC  -> imm20U ++# 0
              JAL    -> 4
              JALR   -> 4
              OP     -> case aluOp of
                          ADD | isSub -> negate rs2Value
                          _ -> rs2Value
              STORE  -> signExtend imm12S
              _      -> signExtend imm12I

  aluArg2Shamt = unpack (zeroExtend (slice d4 d0 aluArg2))

  aluOp = case opcode of
            OP     -> iop
            OP_IMM -> iop
            _      -> ADD

  aluResult = case aluOp of
    ADD  -> aluArg1 + aluArg2
    SLL  -> aluArg1 `shiftL` aluArg2Shamt
    SLT  -> boolToBitVector ((unpack aluArg1 :: Signed 32) < unpack aluArg2)
    SLTU -> boolToBitVector (aluArg1 < aluArg2)
    XOR  -> aluArg1 `xor` aluArg2
    SR   -> case srla of
              Logical    -> aluArg1 `shiftR` aluArg2Shamt
              Arithmetic -> pack ((unpack aluArg1 :: Signed 32) `shiftR` aluArg2Shamt)
    OR   -> aluArg1 .|. aluArg2
    AND  -> aluArg1 .&. aluArg2

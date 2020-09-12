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
  aluResult = 0

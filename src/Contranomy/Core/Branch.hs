{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.Branch where

import Clash.Prelude

import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

branchUnit ::
  -- | instruction
  MachineWord->
  -- | rs1 value
  MachineWord ->
  -- | rs2 value
  MachineWord ->
  -- | PC
  PC ->
  -- |
  -- 1. MSBs of the calculated next PC
  -- 2. LSBs of the calculated next PC
  (PC, BitVector 2)
branchUnit instruction rs1Val rs2Val pc = case opcode of
  BRANCH ->
    let taken = case unpack func3 of
                  BEQ -> rs1Val == rs2Val
                  BNE -> rs1Val /= rs2Val
                  BLT -> (unpack rs1Val :: Signed 32) < unpack rs2Val
                  BLTU -> rs1Val < rs2Val
                  BGE -> (unpack rs1Val :: Signed 32) >= unpack rs2Val
                  BGEU -> rs1Val >= rs2Val
                  _ -> False
     in if taken then
          let (offset,align) = split (signExtend imm12B `shiftL` 1 :: MachineWord)
           in (pc + offset,align)
        else
          (pc + 1, 0)

  JAL ->
    let (offset,align) = split (signExtend imm20J `shiftL` 1 :: MachineWord)
     in (pc + offset, align)

  JALR ->
    let (pcN, align) = split (rs1Val + signExtend imm12I)
        alignLSBZero = align .&. 0b10
     in (pcN, alignLSBZero)

  _ ->
    (pc + 1, 0)
 where
  DecodedInstruction {opcode,func3,imm12B,imm12I,imm20J} = decodeInstruction instruction
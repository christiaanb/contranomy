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

-- | The branch unit performs the address calculation for the next instruction
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
  -- We split the calculated next PC into two ranges
  --
  -- 1. Upper 30 MSBs of the calculated next PC
  -- 2. Lower 2 LSBs of the calculated next PC
  --
  -- That's because our addresses must be 4-byte aligned. If the lower two
  -- bits are non-zero, the Exception unit will take ensure that we Trap
  (PC, BitVector 2)
branchUnit instruction rs1Val rs2Val pc = case opcode of
  _ ->
    (pc + 1, 0)
 where
  DecodedInstruction {opcode} = decodeInstruction instruction

module Contranomy.RegisterFile where

import Clash.Prelude

import Contranomy.Instruction

registerFile ::
  (HiddenClock dom, HiddenEnable dom) =>
  Signal dom (Register,Register,Maybe (Register,MachineWord)) ->
  Signal dom (MachineWord,MachineWord)
registerFile (unbundle -> (rs1,rs2,rw)) =
  let rs1Val = blockRam (replicate d32 0) rs1 rw
      rs2Val = blockRam (replicate d32 0) rs2 rw
   in bundle (rs1Val,rs2Val)

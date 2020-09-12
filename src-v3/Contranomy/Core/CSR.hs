{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Contranomy.Core.CSR where

import Control.Lens
import Control.Monad.Trans.State
import Data.Generics.Labels ()

import Clash.Prelude

import Contranomy.Clash.Extra
import Contranomy.Instruction
import Contranomy.Core.Decode
import Contranomy.Core.MachineState
import Contranomy.Core.SharedTypes

-- | This function implements reading from and writing to control and status
-- registers (CSR).
--
-- Currently implements the following standard RISCV CSRs:
--
-- * misa
-- * mstatus
-- * mtvec
-- * mip
-- * mie
-- * mscratch
-- * mepc
-- * mcause
-- * mtval
--
-- And two uArch specific CSRs for external interrupt handling:
--
-- * IRQMASK at address 0x330, the mask for the external interrupt vectors
-- * IRQPENDING at address 0x360, to know which external interrupt is pending
--
-- misa, mip, and IRQPENDING are read-only
csrUnit ::
  Bool ->
  -- | Instruction
  MachineWord ->
  -- | RS1 Value
  MachineWord ->
  -- | Software interrupt
  Bool ->
  -- | Timer interrupt
  Bool ->
  -- | External interrupt
  MachineWord ->
  State MachineState (Maybe MachineWord, MachineWord)
csrUnit trap instruction rs1Val softwareInterrupt timerInterrupt externalInterrupt
  -- Is this a CSR operations?

  -- Only read and write the machine state if a trap/interrupt hasn't been
  -- raised. Otherwise we're reading/updating the machine state that was
  -- altered by the execption handler. Once we return from the trap/interrupt
  -- handler we can access and update these CSRs in their non-exceptional state.
  | not trap
  = return (Nothing, undefined)

  | otherwise
  = return (Nothing, undefined)

csrWrite ::
  CSRType ->
  MachineWord ->
  Maybe MachineWord ->
  MachineWord
csrWrite _ oldValue _ = oldValue

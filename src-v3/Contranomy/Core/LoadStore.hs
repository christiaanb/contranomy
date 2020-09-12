{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.LoadStore where

import Clash.Prelude

import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction
import Contranomy.Wishbone

-- | This function performs data-bus transactions for loads and stores.
--
-- It does not initiate the transaction if the address is misaligned for the
-- given load/store size, nor does it perform bus transactions for faulty
-- instructions (not even loads, they might have side-effects).
loadStoreUnit ::
  -- | Instruction
  MachineWord ->
  -- | Instruction faulty, no data bus transactions should happen for faulty instrucctions
  Bool ->
  -- | Load/Store address (calculated by the ALU)
  MachineWord ->
  -- | The value to store
  MachineWord ->
  -- | Data bus response (slave-to-master)
  WishboneS2M 4 ->
  -- |
  -- 1. Data bus initiation (master-to-slave)
  -- 2. The address causing a data-access fault on the data bus
  -- 3. The misaligned address
  -- 4. Data bus transaction completed
  (WishboneM2S 4 30, Maybe MachineWord, Maybe MachineWord, Maybe MachineWord, Bool)
loadStoreUnit instruction instructionFault addr toStore dBusS2M = case opcode of
  _otherwise ->
    ( wishboneM2S
    , Nothing
    , Nothing
    , Nothing
    , True
    )
 where
  DecodedInstruction {opcode} = decodeInstruction instruction

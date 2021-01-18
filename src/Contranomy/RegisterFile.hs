{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Contranomy.RegisterFile where

import Data.Maybe

import Clash.Prelude

import Contranomy.Instruction

{-# NOINLINE registerFile #-}
-- | 32-space register file with 2 read ports and 1 write port
registerFile ::
  (HiddenClockResetEnable dom, Num a, NFDataX a) =>
  -- | Signal of:
  -- 1. Read addres A
  -- 2. Read addres B
  -- 3. Address/Value pair to write
  Signal dom (Maybe Register,Maybe Register,Maybe (Register,a)) ->
  -- | Signal of:
  -- Value at address A - delayed by 1 clock cycle
  -- Value at adderss B - delayed by 1 clock cycle
  Signal dom (a,a)
registerFile (unbundle -> (rs1M,rs2M,rw)) =
  let rs1Val = blockRam (replicate d32 0) (regMaybeNext X0 rs1M) rw
      rs2Val = blockRam (replicate d32 0) (regMaybeNext X0 rs2M) rw
   in bundle (rs1Val,rs2Val)

regMaybeNext ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- | Start value
  a ->
  -- | The current/new value value
  Signal dom (Maybe a) ->
  -- | Old value when the current/new value was a "Nothing",
  -- the current/new value otherwise
  Signal dom a
regMaybeNext start newM = fromMaybe <$> old <*> newM
 where
  old = regMaybe start newM

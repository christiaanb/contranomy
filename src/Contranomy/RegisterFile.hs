{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Contranomy.RegisterFile where

import Data.Maybe

import Clash.Prelude

import Contranomy.Instruction

registerFile ::
  (HiddenClockResetEnable dom, Num a, NFDataX a) =>
  Signal dom (Maybe Register,Maybe Register,Maybe (Register,a)) ->
  Signal dom (a,a)
registerFile (unbundle -> (rs1M,rs2M,rw)) =
  let rs1Val = blockRam (replicate d32 0) (regMaybeNext X0 rs1M) rw
      rs2Val = blockRam (replicate d32 0) (regMaybeNext X0 rs2M) rw
   in bundle (rs1Val,rs2Val)

regMaybeNext ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- | Start value
  a ->
  -- | Next value
  Signal dom (Maybe a) ->
  -- | Old value when the next value was a "Nothing", the new value otherwise
  Signal dom a
regMaybeNext start newM = fromMaybe <$> old <*> newM
 where
  old = regMaybe start newM

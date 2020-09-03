module Contranomy.Clash.Extra where

import Clash.Class.AutoReg (AutoReg)
import Clash.Prelude
import Control.Monad.Trans.State

mealyAuto ::
  (HiddenClockResetEnable dom, AutoReg s) =>
  (s -> i -> (o,s)) ->
  s ->
  (Signal dom i -> Signal dom o)
mealyAuto transition start =
  \i -> let (o,sN) = unbundle (transition <$> s <*> i)
            s      = setName @"core" autoReg start sN
        in  o
{-# INLINE mealyAuto #-}

withState :: s -> State s o -> (o,s)
withState s m = runState m s
{-# INLINE withState #-}

mealyAutoB ::
  (HiddenClockResetEnable dom, AutoReg s, Bundle i, Bundle o) =>
  (s -> i -> (s,o)) ->
  s ->
  (Unbundled dom i -> Unbundled dom o)
mealyAutoB transition start =
  \i -> let (sN,o) = unbundle (transition <$> s <*> (bundle i))
            s      = setName @"core" autoReg start sN
        in  unbundle o
{-# INLINE mealyAutoB #-}

boolToBitVector :: KnownNat n => Bool -> BitVector n
boolToBitVector = resize . pack
{-# INLINE boolToBitVector #-}

downto :: (Num a, Bits a) => Int -> Int -> a
downto h l = (1 `shiftL` (h - l + 1) - 1) `shiftL` l
{-# INLINE downto #-}

bitsDownTo :: (Num a, Bits a) => Int -> Int -> a -> a
bitsDownTo h l = \w -> w .&. downto h l
{-# INLINE bitsDownTo #-}

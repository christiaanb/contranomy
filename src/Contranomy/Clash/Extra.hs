module Contranomy.Clash.Extra where

import Clash.Class.AutoReg (AutoReg)
import Clash.Prelude

mealyAutoB ::
  (HiddenClockResetEnable dom, AutoReg s, Bundle i, Bundle o) =>
  (s -> i -> (s,o)) ->
  s ->
  (Unbundled dom i -> Unbundled dom o)
mealyAutoB transition start =
  \i -> let (sN,o) = unbundle (transition <$> s <*> (bundle i))
            s      = autoReg start sN
        in  unbundle o
{-# INLINE mealyAutoB #-}

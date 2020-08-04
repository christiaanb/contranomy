{-# OPTIONS_GHC -fno-warn-orphans #-}
module Contranomy where

import Clash.Prelude
import Clash.Annotations.TH

import Contranomy.Core
import Contranomy.WishBone

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

contronomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "iBusWishbone" ::: Signal Core (WishBoneS2M 4)
  , "dBusWishbone" ::: Signal Core (WishBoneS2M 4) ) ->
  ( "iBusWishbone" ::: Signal Core (WishBoneM2S 4 32)
  , "dbusWishbone" ::: Signal Core (WishBoneM2S 4 32)
  )
contronomy clk rst = exposeClockResetEnable core clk rst enableGen

makeTopEntity 'contronomy
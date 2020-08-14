{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Contranomy where

import Clash.Prelude
import Clash.Annotations.TH

import Contranomy.Core
import Contranomy.RVFI
import Contranomy.WishBone

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

contranomyRVFI ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "iBusWishbone" ::: Signal Core (WishBoneS2M 4)
  , "dBusWishbone" ::: Signal Core (WishBoneS2M 4) ) ->
  ( "iBusWishbone" ::: Signal Core (WishBoneM2S 4 30)
  , "dbusWishbone" ::: Signal Core (WishBoneM2S 4 32)
  , "" ::: Signal Core RVFI
  )
contranomyRVFI clk rst = exposeClockResetEnable core clk rst enableGen

makeTopEntity 'contranomyRVFI

contranomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "iBusWishbone" ::: Signal Core (WishBoneS2M 4)
  , "dBusWishbone" ::: Signal Core (WishBoneS2M 4) ) ->
  ( "iBusWishbone" ::: Signal Core (WishBoneM2S 4 30)
  , "dbusWishbone" ::: Signal Core (WishBoneM2S 4 32)
  )
contranomy clk rst = exposeClockResetEnable (dropRVFI . core) clk rst enableGen
 where
  dropRVFI (iBus,dBus,_) = (iBus,dBus)

makeTopEntity 'contranomy

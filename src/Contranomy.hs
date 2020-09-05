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
import Contranomy.RegisterFile

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

contranomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreOut,regWrite) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in coreOut

makeTopEntity 'contranomy

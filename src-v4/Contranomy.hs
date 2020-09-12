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
import Contranomy.RVFI

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

-- | Contranomy RV32I core
contranomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,_) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in coreResult

makeTopEntity 'contranomy

-- | Contranomy RV32I core with RVFI interface
contranomyRVFI ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFI clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreResult,regWrite,rvfiOut) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in (coreResult,rvfiOut)

makeTopEntity 'contranomyRVFI

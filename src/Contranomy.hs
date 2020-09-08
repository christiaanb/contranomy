{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Contranomy where

import Clash.Prelude
import Clash.Annotations.TH

import Contranomy.Core
import Contranomy.RegisterFile
import Contranomy.RVFI
import Contranomy.WishBone

import qualified Data.List as L

createDomain vXilinxSystem{vName="Core", vPeriod=hzToPeriod 100e6}

contranomy ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut)
contranomy clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreOut,regWrite,_) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in coreOut

makeTopEntity 'contranomy

contranomyRVFI ::
  "clk" ::: Clock Core ->
  "reset" ::: Reset Core ->
  ( "" ::: Signal Core CoreIn) ->
  ( "" ::: Signal Core CoreOut
  , "" ::: Signal Core RVFI)
contranomyRVFI clk rst coreIn = withClockResetEnable clk rst enableGen $
  let (coreOut,regWrite,rvfi) = core (coreIn,regOut)
      regOut = registerFile regWrite
   in (coreOut,rvfi)

makeTopEntity 'contranomyRVFI

testCoreIn :: Bool -> CoreIn
testCoreIn ack
  = CoreIn
  { iBusS2M = (defS2M @4) { readData = instr, acknowledge = ack }
  , dBusS2M = defS2M
  , timerInterrupt = False
  , softwareInterrupt = False
  , externalInterrupt = 1
  }
 where
  instr = 0b00110000001000000000000001110011 :: BitVector 32

testSequence
  = L.replicate 7 (testCoreIn False) <>
    [testCoreIn True] <>
    L.replicate 7 (testCoreIn False) <>
    [testCoreIn True] <>
    L.replicate 7 (testCoreIn False)
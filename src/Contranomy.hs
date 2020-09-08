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
import Contranomy.Instruction

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

testCoreIn :: Bool -> BitVector 32 -> CoreIn
testCoreIn ack instr
  = CoreIn
  { iBusS2M = (defS2M @4) { readData = instr, acknowledge = ack }
  , dBusS2M = defS2M
  , timerInterrupt = False
  , softwareInterrupt = False
  , externalInterrupt = 2
  }
--  where
--   instr = 0b00110000001000000000000001110011 :: BitVector 32

testSequence
  = [testCoreIn True luit0] <>              -- 0:la t0 90
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True csrwmtvec] <>          -- 1:crsw mtvec t0
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True luit0_1] <>            -- 2:li t0 0x800
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True lit0_1] <>            -- 3:li t0 0x800
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True csrsmie] <>            -- 4:crss mie t0
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True lit0_2] <>            -- 5:li t0 2
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True csrwirq] <>            -- 6:crsw irqmask t0
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True lit0_3] <>            -- 7:li t0 0x8
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True csrsmstatus] <>        -- 8:crss mstatus t0
    L.replicate 5 (testCoreIn False 0) <>
    [testCoreIn True instr_mret] <>         -- 9:
    L.replicate 5 (testCoreIn False 0)
 where
  luit0 = (90 :: BitVector 20) ++# pack X5 ++# pack LUI

  csrwmtvec = srcDst ++# pack X5 ++# (1 :: BitVector 3) ++# pack X0 ++# pack SYSTEM
   where
    CSRRegister srcDst = MTVEC

  luit0_1 = (0x1 :: BitVector 20) ++# pack X5 ++# pack LUI
  lit0_1  = (0x800 :: BitVector 12) ++# pack X5 ++# pack ADD ++# pack X5 ++# pack OP_IMM

  csrsmie = srcDst ++# pack X5 ++# (2 :: BitVector 3) ++# pack X0 ++# pack SYSTEM
   where
    CSRRegister srcDst = MIE

  lit0_2 = (2 :: BitVector 12) ++# pack X0 ++# pack ADD ++# pack X5 ++# pack OP_IMM

  csrwirq = srcDst ++# pack X5 ++# (1 :: BitVector 3) ++# pack X0 ++# pack SYSTEM
   where
    CSRRegister srcDst = IRQMASK

  lit0_3 = (0x8 :: BitVector 12) ++# pack X0 ++# pack ADD ++# pack X5 ++# pack OP_IMM

  csrsmstatus = srcDst ++# pack X5 ++# (2 :: BitVector 3) ++# pack X0 ++# pack SYSTEM
   where
    CSRRegister srcDst = MSTATUS

  instr_mret = 0b00110000001000000000000001110011 :: BitVector 32

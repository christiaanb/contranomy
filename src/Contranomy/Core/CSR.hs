{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}

module Contranomy.Core.CSR where

import Control.Lens
import Control.Monad.Trans.State
import Data.Generics.Labels ()

import Clash.Prelude

import Contranomy.Instruction
import Contranomy.Core.Decode
import Contranomy.Core.MachineState
import Contranomy.Core.SharedTypes

csrUnit ::
  Bool ->
  -- | Instruction
  MachineWord ->
  -- | RS1 Value
  MachineWord ->
  -- | Software interrupt
  Bool ->
  -- | Timer interrupt
  Bool ->
  -- | External interrupt
  MachineWord ->
  State MachineState (Maybe MachineWord, MachineWord)
csrUnit trap instruction rs1Val softwareInterrupt timerInterrupt externalInterrupt
  | SYSTEM <- opcode
  , func3 /= 0
  , not trap
  = do

    MachineState
          {mstatus=MStatus{mie,mpie}
          ,mie=Mie{meie,mtie,msie}
          ,mtvec
          ,mscratch
          ,mcause=MCause{interrupt,code}
          ,mtval
          ,mepc
          ,irqmask
          } <- get

    let csrType = unpack (slice d1 d0 func3)
        uimm = pack rs1

    let writeValue0 =
          if testBit func3 2 then
            zeroExtend uimm
          else
            rs1Val
        writeValue1 = case csrType of
          ReadWrite -> Just writeValue0
          _ | uimm == 0 -> Nothing
            | otherwise -> Just writeValue0

    case CSRRegister srcDest of
      MSTATUS -> do
        let oldValue = bitB mpie 7 .|. bitB mie 3
            newValue = csrWrite csrType oldValue writeValue1
        #mstatus .= MStatus { mpie=testBit newValue 7
                            , mie=testBit newValue 3 }
        return (Just oldValue, newValue)
      MISA -> do
        let oldValue = bit 30 .|. bit 8
            newValue = csrWrite csrType oldValue writeValue1
        return (Just oldValue, newValue)
      MIP -> do
        let oldValue = bitB (externalInterrupt /= 0) 11 .|.
                       bitB timerInterrupt 7 .|.
                       bitB softwareInterrupt 3
            newValue = csrWrite csrType oldValue writeValue1
        return (Just oldValue, newValue)
      MIE -> do
        let oldValue = bitB meie 11 .|. bitB mtie 7 .|. bitB msie 3
            newValue = csrWrite csrType oldValue writeValue1
        #mie .= Mie {meie=testBit newValue 11
                    ,mtie=testBit newValue 7
                    ,msie=testBit newValue 3
                    }
        return (Just oldValue, newValue)
      MTVEC -> do
        let oldValue = pack mtvec
            newValue = csrWrite csrType oldValue writeValue1
        #mtvec .= (unpack newValue :: InterruptMode)
        return (Just oldValue, newValue)
      MSCRATCH -> do
        let oldValue = mscratch
            newValue = csrWrite csrType oldValue writeValue1
        #mscratch .= newValue
        return (Just oldValue, newValue)
      MEPC -> do
        let oldValue = mepc ++# 0
            newValue = csrWrite csrType oldValue writeValue1
        #mepc .= slice d31 d2 newValue
        return (Just oldValue, newValue)
      MCAUSE -> do
        let oldValue = pack interrupt ++# 0 ++# code
            newValue = csrWrite csrType oldValue writeValue1
        #mcause .= MCause { interrupt = testBit newValue 31, code = truncateB newValue }
        return (Just oldValue, newValue)
      MTVAL -> do
        let oldValue = mtval
            newValue = csrWrite csrType oldValue writeValue1
        #mtval .= newValue
        return (Just oldValue, newValue)
      IRQMASK -> do
        let oldValue = irqmask
            newValue = csrWrite csrType oldValue writeValue1
        #irqmask .= newValue
        return (Just oldValue, newValue)
      IRQPENDING -> do
        let newValue = csrWrite csrType externalInterrupt writeValue1
        return (Just externalInterrupt, newValue)
      _ -> return (Nothing, undefined)

  | otherwise
  = return (Nothing, undefined)
 where
  DecodedInstruction {opcode,func3,rs1,imm12I=srcDest} = decodeInstruction instruction

  bitB b i = if b then bit i else 0

  csrWrite ::
    CSRType ->
    MachineWord ->
    Maybe MachineWord ->
    MachineWord
  csrWrite ReadWrite oldValue newValueM = maybe oldValue id newValueM
  csrWrite ReadSet oldValue newValueM   = maybe oldValue (oldValue .|.) newValueM
  csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM
  csrWrite _ oldValue _ = oldValue
  {-# INLINE csrWrite #-}
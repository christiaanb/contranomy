{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Contranomy.Core.Exception where

import Control.Lens
import Control.Monad.Trans.State
import Data.Maybe
import Data.Generics.Labels ()

import Clash.Prelude

import Contranomy.Core.CoreState
import Contranomy.Core.Decode
import Contranomy.Core.MachineState
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

data ExceptionIn
  = ExceptionIn
  { instrAccessFault    :: Bool
  , instrAddrMisaligned :: Bool
  , instrIllegal        :: Bool
  , dataAccessFault     :: Maybe MachineWord
  , dataAddrMisaligned  :: Maybe MachineWord
  , timerInterrupt      :: Bool
  , softwareInterrupt   :: Bool
  , externalInterrupt   :: MachineWord
  }

handleExceptions ::
  CoreState ->
  ExceptionIn ->
  -- | Load/Store unit finished
  Bool ->
  -- | Next PC
  (PC,BitVector 2) ->
  State CoreState (Bool,PC)
handleExceptions CoreState{pc,instruction,machineState} exceptionIn lsFinished (pcN,align) = do
  let ExceptionIn
        { instrAccessFault
        , instrAddrMisaligned
        , instrIllegal
        , dataAccessFault
        , dataAddrMisaligned
        , timerInterrupt
        , softwareInterrupt
        , externalInterrupt
        } = exceptionIn

  let DecodedInstruction{opcode,func3,imm12I} = decodeInstruction instruction
  let breakpoint = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == EBREAK
        _ -> False

  let eCall = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == ECALL
        _ -> False

  let mret = case opcode of
        SYSTEM
          | func3 == 0
          -> System12 imm12I == MRET
        _ -> False

  let trap =
        instrAccessFault || instrAddrMisaligned || instrIllegal ||
        isJust dataAccessFault || isJust dataAddrMisaligned || breakpoint || eCall

  let MachineState{mstatus,mie=Mie{mtie,msie,meie},mepc,mtvec,irqmask} = machineState
      MStatus{mie,mpie} = mstatus

      timerInterrupt1    = timerInterrupt && mtie
      softwareInterrupt1 = softwareInterrupt && msie
      externalInterrupt1 = ((externalInterrupt .&. irqmask) /= 0) && meie
  let interrupt =
        lsFinished && mie && (timerInterrupt1 || softwareInterrupt1 || externalInterrupt1)


  if trap || interrupt then do
    #machineState .= machineState
                    { mstatus = MStatus { mpie = mie, mie = False }
                    , mcause =
                      if interrupt then
                        if softwareInterrupt1 then
                          MACHINE_SOFTWARE_INTERRUPT
                        else if timerInterrupt1 then
                          MACHINE_TIMER_INTERRUPT
                        else -- externalInterrupt1
                          MACHINE_EXTERNAL_INTERRUPT
                      else
                        if instrAccessFault then
                          INSTRUCTION_ACCESS_FAULT
                        else if instrIllegal then
                          ILLEGAL_INSTRUCTION
                        else if instrAddrMisaligned then
                          INSTRUCTION_ADDRESS_MISALIGNED
                        else if eCall then
                          ENVIRONMENT_CALL
                        else if breakpoint then
                          BREAKPOINT
                        else case dataAddrMisaligned of
                          Just _ -> case opcode of
                            LOAD -> LOAD_ADDRESS_MISALIGNED
                            _ -> STORE_ADDRESS_MISALIGNED
                          _ -> case opcode of -- dataAccessFault
                            LOAD -> LOAD_ADDRESS_MISALIGNED
                            _ -> STORE_ADDRESS_MISALIGNED
                    , mepc = pc ++# 0
                    , mtval =
                      if instrAddrMisaligned then
                        pcN ++# align
                      else if instrIllegal then
                        instruction
                      else if instrAccessFault then
                        pc ++# 0
                      else if breakpoint then
                        pc ++# 0
                      else case dataAddrMisaligned of
                        Just addr -> addr
                        _ -> case dataAccessFault of
                          Just addr -> addr
                          _ -> 0
                    }
    let pcN1 = trapBase mtvec
    return (True,pcN1)
  else if mret then do
    #machineState .= machineState { mstatus = mstatus {mie = mpie} }
    return (False,mepc)
  else do
    return (False,pcN)
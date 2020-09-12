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

-- | This function takes care of exception handling, both synchronous exceptions
-- (traps) and asynchronous exceptions (interrupts). It takes in information
-- from other functional units whether to raise a trap.
--
-- It updates all the relevant Machine CSRs when a trap or interrupt is enterred
-- and then returns the PC of the trap handler (stored in mtvec).
--
-- This function also implements the MRET, machine trap return, instruction
-- since we need to update the mstatus CSR, and return the PC from before the
-- trap was entered (stored in mepc)
handleExceptions ::
  CoreState ->
  ExceptionIn ->
  -- | Load/Store unit finished
  Bool ->
  -- | Next PC
  (PC,BitVector 2) ->
  -- |
  -- 1. Indication whether a trap or interrupt was raised
  -- 2. The PC for the next instruction cycle
  State CoreState (Bool,PC)
handleExceptions CoreState{pc,instruction,machineState} exceptionIn lsFinished (pcN,align) = do
    return (False,pcN)

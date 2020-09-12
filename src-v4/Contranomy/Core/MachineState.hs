{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DuplicateRecordFields #-}

module Contranomy.Core.MachineState
  ( MStatus (..)
  , InterruptMode (Direct, Vectored, trapBase)
  , Mie (..)
  , MachineState (..)
  )
where

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Class.AutoReg
import Clash.Prelude

import Contranomy.Clash.Extra
import Contranomy.Core.SharedTypes
import Contranomy.Instruction

-- | Machine status
data MStatus
  = MStatus
  { -- | Machine interrupts enabled
    mie :: Bool
    -- | Machine interrupts previously enabled
  , mpie :: Bool
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MStatus

data InterruptMode
  = Direct {trapBase :: (BitVector 30)}
  | Vectored {trapBase :: (BitVector 30)}
  | IllegalIM1
  | IllegalIM2
  deriving (Generic, NFDataX, AutoReg)

{-# ANN module (DataReprAnn
                  $(liftQ [t|InterruptMode|])
                  32
                  [ ConstrRepr 'Direct     (1 `downto` 0) 0 [31 `downto` 2]
                  , ConstrRepr 'Vectored   (1 `downto` 0) 1 [31 `downto` 2]
                  , ConstrRepr 'IllegalIM1 (1 `downto` 0) 2 []
                  , ConstrRepr 'IllegalIM2 (1 `downto` 0) 3 []
                  ]) #-}
deriveBitPack [t| InterruptMode |]

-- | Machine interrupt enable state
data Mie
  = Mie
  { meie :: Bool
  , mtie :: Bool
  , msie :: Bool
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''Mie

data MachineState
  = MachineState
  { mstatus :: MStatus
  , mcause :: MCause
  , mtvec :: InterruptMode
  , mie :: Mie
  , mscratch :: MachineWord
  , mepc :: PC
  , mtval :: MachineWord
  , irqmask :: MachineWord
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MachineState
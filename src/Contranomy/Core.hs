{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core (topEntity,plus) where

import Clash.Prelude hiding (cycle, select)

import Contranomy.Decode
import Contranomy.RV32IM
import Contranomy.WishBone

data CoreStage
  = InstructionFetch
  | Execute
  | WriteBack
  deriving (Generic, NFDataX)

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: Unsigned 32
  , instruction :: Instr
  }
  deriving (Generic, NFDataX)

core ::
  SystemClockResetEnable =>
  ( Signal System (WishBoneS2M 4)
  , Signal System (WishBoneS2M 4) ) ->
  ( Signal System (WishBoneM2S 4 32)
  , Signal System (WishBoneM2S 4 32)
  )
core = mealyB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 0
    , instruction = noop
    }

  transition ::
    CoreState ->
    ( WishBoneS2M 4, WishBoneS2M 4 ) ->
    ( CoreState
    , ( WishBoneM2S 4 32
      , WishBoneM2S 4 32 ))
  transition s@(CoreState { stage = InstructionFetch, pc }) (iBus,dBus)
    | bitCoerce (acknowledge iBus)
    = ( s { stage = Execute
          , pc = pc+4
          , instruction = decodeInstruction (readData iBus)
          }
      , ( defM2S
        , defM2S ) )
    | otherwise
    = ( s
      , ( defM2S { addr = pack pc
                 , cycle = True
                 , strobe = True
                 }
        , defM2S )
      )

  defM2S :: WishBoneM2S 4 32
  defM2S
    = WishBoneM2S
    { addr = 0
    , writeData = 0
    , select = maxBound
    , cycle = False
    , strobe = False
    , writeEnable = False
    , cycleTypeIdentifier = Classic
    , burstTypeExtension = LinearBurst
    }


plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity :: Signed 8 -> Signed 8 -> Signed 8
topEntity = plus

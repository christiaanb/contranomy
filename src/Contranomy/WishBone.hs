{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE PatternSynonyms #-}

module Contranomy.WishBone where

import Clash.Prelude

data WishBoneM2S bytes addressWidth
  = WishBoneM2S
  { -- | ADR
    addr :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData :: "DAT_MOSI" ::: BitVector (8 * bytes)
    -- | SEL
  , select :: "SEL" ::: BitVector bytes
    -- | CYC
  , cycle :: "CYC" ::: Bool
    -- | STB
  , strobe :: "STB" ::: Bool
    -- | WE
  , writeEnable :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension :: "BTE" ::: BurstTypeExtension
  }

data WishBoneS2M bytes
  = WishBoneS2M
  { -- | DAT
    readData :: "DAT_MISO" ::: BitVector (8 * bytes)
    -- | ACK
  , acknowledge :: "ACK" ::: Bit
    -- | ERR
  , err :: "ERR" ::: Bit
  }

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
pattern Classic = CycleTypeIdentifier 0
pattern ConstantAddressBurst = CycleTypeIdentifier 1
pattern IncrementingBurst = CycleTypeIdentifier 2
pattern EndOfBurst = CycleTypeIdentifier 7

newtype BurstTypeExtension = BurstTypeExtension (BitVector 2)

pattern LinearBurst, Beat4Burst, Beat8Burst, Beat16Burst :: BurstTypeExtension
pattern LinearBurst = BurstTypeExtension 0
pattern Beat4Burst = BurstTypeExtension 1
pattern Beat8Burst = BurstTypeExtension 2
pattern Beat16Burst = BurstTypeExtension 3

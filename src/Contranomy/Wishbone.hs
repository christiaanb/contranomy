{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE PatternSynonyms #-}

module Contranomy.Wishbone where

import Clash.Prelude

data WishboneM2S bytes addressWidth
  = WishboneM2S
  { -- | ADR
    addr :: "ADR" ::: BitVector addressWidth
    -- | DAT
  , writeData :: "DAT_MOSI" ::: BitVector (8 * bytes)
    -- | SEL
  , busSelect :: "SEL" ::: BitVector bytes
    -- | CYC
  , busCycle :: "CYC" ::: Bool
    -- | STB
  , strobe :: "STB" ::: Bool
    -- | WE
  , writeEnable :: "WE" ::: Bool
    -- | CTI
  , cycleTypeIdentifier :: "CTI" ::: CycleTypeIdentifier
    -- | BTE
  , burstTypeExtension :: "BTE" ::: BurstTypeExtension
  }

data WishboneS2M bytes
  = WishboneS2M
  { -- | DAT
    readData :: "DAT_MISO" ::: BitVector (8 * bytes)
    -- | ACK
  , acknowledge :: "ACK" ::: Bool
    -- | ERR
  , err :: "ERR" ::: Bool
  }

newtype CycleTypeIdentifier = CycleTypeIdentifier (BitVector 3)

pattern Classic, ConstantAddressBurst, IncrementingBurst, EndOfBurst :: CycleTypeIdentifier
pattern Classic = CycleTypeIdentifier 0
pattern ConstantAddressBurst = CycleTypeIdentifier 1
pattern IncrementingBurst = CycleTypeIdentifier 2
pattern EndOfBurst = CycleTypeIdentifier 7

data BurstTypeExtension
  = LinearBurst
  | Beat4Burst
  | Beat8Burst
  | Beat16Burst

wishboneM2S ::
  forall bytes addressWidth .
  WishboneM2S bytes addressWidth
wishboneM2S
  = WishboneM2S
  { addr = undefined
  , writeData = undefined
  , busSelect = undefined
  , busCycle = False
  , strobe = False
  , writeEnable = False
  , cycleTypeIdentifier = Classic
  , burstTypeExtension = LinearBurst
  }

wishboneS2M ::
  forall bytes .
  KnownNat bytes =>
  WishboneS2M bytes
wishboneS2M
  = WishboneS2M
  { readData = 0
  , acknowledge = False
  , err = False
  }

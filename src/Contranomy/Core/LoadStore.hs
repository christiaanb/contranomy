{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core.LoadStore where

import Clash.Prelude

import Contranomy.Core.Decode
import Contranomy.Core.SharedTypes
import Contranomy.Instruction
import Contranomy.Wishbone

loadStoreUnit ::
  -- Instruction
  MachineWord ->
  -- Instruction faulty
  Bool ->
  -- address
  MachineWord ->
  -- store
  MachineWord ->
  -- DBUS
  WishboneS2M 4 ->
  (WishboneM2S 4 30, Maybe MachineWord, Maybe MachineWord, Maybe MachineWord, Bool)
loadStoreUnit instruction instructionFault addr store dBusS2M = case opcode of
  LOAD | not instructionFault ->
    let loadData = case loadStoreWidth of
          Byte sign ->
            loadExtend
              sign
              (slice d7 d0 (readData dBusS2M `shiftR` shiftAmount))
          Half sign ->
            loadExtend
              sign
              (slice d15 d0 (readData dBusS2M `shiftR` shiftAmount))
          _ -> readData dBusS2M
     in ( wishboneM2S
            { addr = slice d31 d2 addr
            , busSelect = mask
            , busCycle = aligned
            , strobe = aligned
            }
        , if not aligned || err dBusS2M || not (acknowledge dBusS2M) then
             Nothing
          else
            Just loadData
        , if err dBusS2M then Just addr else Nothing
        , if aligned then Nothing else Just addr
        , busFinished
        )

  STORE | not instructionFault ->
    let storeData = case loadStoreWidth of
          Byte _ -> store `shiftL` shiftAmount
          Half _ -> store `shiftL` shiftAmount
          _ -> store
     in ( wishboneM2S
           { addr = slice d31 d2 addr
           , writeData = storeData
           , busSelect = mask
           , busCycle = aligned
           , strobe = aligned
           , writeEnable = aligned
           }
        , Nothing
        , if err dBusS2M then Just addr else Nothing
        , if aligned then Nothing else Just addr
        , busFinished
        )

  _otherwise ->
    ( wishboneM2S
    , Nothing
    , Nothing
    , Nothing
    , True
    )
 where
  DecodedInstruction {opcode,func3} = decodeInstruction instruction

  busFinished = err dBusS2M || acknowledge dBusS2M

  loadStoreWidth :: LoadStoreWidth
  loadStoreWidth = unpack func3

  alignment = slice d1 d0 addr

  aligned = case loadStoreWidth of
    Word
      -> alignment == 0
    Half _
      -> not (testBit alignment 0)
    _word
      -> True

  mask = case loadStoreWidth of
    Byte _ -> case alignment of
      3 -> 0b1000
      2 -> 0b0100
      1 -> 0b0010
      _ -> 0b0001
    Half _ -> case alignment of
      2 -> 0b1100
      _ -> 0b0011
    _word
        -> 0b1111

  shiftAmount = case loadStoreWidth of
    Byte _ -> case alignment of
      3 -> 24
      2 -> 16
      1 -> 8
      _ -> 0
    Half _ -> case alignment of
      2 -> 16
      _ -> 0
    _word
        -> 0

loadExtend ::
  (KnownNat n, n <= 32) =>
  Sign ->
  BitVector (32 - n) ->
  MachineWord
loadExtend Unsigned = zeroExtend
loadExtend Signed   = signExtend
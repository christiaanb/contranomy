{-|
Copyright Moritz Kiefer (c) 2016

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Moritz Kiefer nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Decode where

import Data.Maybe
import Clash.Prelude

import Contranomy.RV32IM

decodeInstruction :: BitVector 32 -> Instr
decodeInstruction word = fromMaybe noop $ case opCode of
  0b1100011 -> BranchInstr <$> decodeBranchInstr word
  0b1110011 -> decodeCSROrEnvInstr word
  0b1101111 -> JumpInstr <$> decodeJALInstr word
  0b1100111 -> JumpInstr <$> decodeJALRInstr word
  0b0000011 -> MemoryInstr <$> decodeLoadInstr word
  0b0100011 -> MemoryInstr <$> decodeStoreInstr word
  0b0110011 -> RRInstr <$> decodeRRInstr word
  0b0010011 -> RIInstr <$> decodeRIInstr word
  0b0110111 -> RIInstr <$> decodeLUIInstr word
  0b0010111 -> RIInstr <$> decodeAUIPCInstr word
  0b0001111 -> SyncInstr <$> decodeSyncInstr word
  _ -> Nothing
 where
  opCode = slice d6 d0 word

noop :: Instr
noop = RIInstr (IInstr ADDI 0 X0 X0)

decodeBranchCond :: BitVector 3 -> Maybe BranchCond
decodeBranchCond cond = case cond of
  0b000 -> pure BEQ
  0b001 -> pure BNE
  0b100 -> pure BLT
  0b101 -> pure BGE
  0b110 -> pure BLTU
  0b111 -> pure BGEU
  _     -> Nothing

decodeBranchInstr :: BitVector 32 -> Maybe BranchInstr
decodeBranchInstr word = do
  cond <- decodeBranchCond condBV
  pure (Branch {imm, cond, src2, src1})
 where
  imm12    = slice d31 d31 word
  imm10to5 = slice d30 d25 word
  src2     = unpack (slice d24 d20 word)
  src1     = unpack (slice d19 d15 word)
  condBV   = slice d14 d12 word
  imm4to1  = slice d11 d8 word
  imm11    = slice d7 d7 word
  imm      = unpack (imm12 ++# imm11 ++# imm10to5 ++# imm4to1)

decodeCSROrEnvInstr ::  BitVector 32 -> Maybe Instr
decodeCSROrEnvInstr word = case slice d14 d12 word of
  0b000 -> EnvironmentInstr <$> decodeEnvInstr word
  _ -> CSRInstr <$> decodeCSRInstr word

decodeEnvInstr :: BitVector 32 -> Maybe EnvironmentInstr
decodeEnvInstr word = case slice d31 d20 word of
  0 -> pure ECALL
  1 -> pure EBREAK
  0b001100000010 -> pure MRET
  _ -> Nothing

decodeCSRType :: BitVector 2 -> Maybe CSRType
decodeCSRType twoBits = case twoBits of
  0b01 -> pure ReadWrite
  0b10 -> pure ReadSet
  0b11 -> pure ReadClear
  _    -> Nothing

decodeCSRInstr :: BitVector 32 -> Maybe CSRInstr
decodeCSRInstr word = do
  csrType <- decodeCSRType (slice d13 d12 word)
  if testBit word 14 then
    let zimm = unpack (slice d19 d15 word)
    in  pure $ CSRIInstr csrType csr zimm dest
  else
    let src = unpack (slice d19 d15 word)
    in  pure $ CSRRInstr csrType csr src dest
 where
  csr  = CSRRegister . unpack $ slice d31 d20 word
  dest = unpack (slice d11 d7 word)


decodeJALInstr :: BitVector 32 -> Maybe JumpInstr
decodeJALInstr word = pure (JAL imm dest)
 where
  imm20 = slice d31 d31 word
  imm10to1 = slice d30 d21 word
  imm11 = slice d20 d20 word
  imm19to12 = slice d19 d12 word
  dest = unpack (slice d11 d7 word)
  imm  = unpack (imm20 ++# imm19to12 ++# imm11 ++# imm10to1)

decodeJALRInstr :: BitVector 32 -> Maybe JumpInstr
decodeJALRInstr word = pure (JALR offset base dest)
 where
  dest = unpack (slice d11 d7 word)
  base = unpack (slice d19 d15 word)
  offset = unpack (slice d31 d20 word)


decodeLoadWidth :: BitVector 3 -> Maybe LoadWidth
decodeLoadWidth threeBits = case threeBits of
  0b000 -> pure (Width Byte)
  0b001 -> pure (Width Half)
  0b010 -> pure (Width Word)
  0b100 -> pure ByteUnsigned
  0b101 -> pure HalfUnsigned
  _ -> Nothing

decodeLoadInstr :: BitVector 32 -> Maybe MemoryInstr
decodeLoadInstr word = do
  width <- decodeLoadWidth (slice d14 d12 word)
  pure (LOAD width offset base dest)
 where
  offset = unpack (slice d31 d20 word)
  base = unpack (slice d19 d15 word)
  dest = unpack (slice d11 d7 word)

decodeWidth :: BitVector 2 -> Maybe Width
decodeWidth twoBits = case twoBits of
  0b00 -> pure Byte
  0b01 -> pure Half
  0b10 -> pure Word
  _ -> Nothing

decodeStoreInstr :: BitVector 32 -> Maybe MemoryInstr
decodeStoreInstr word = do
  widthD <- decodeWidth width
  pure (STORE widthD offset src base)
 where
  offset11to5 = slice d31 d25 word
  src = unpack (slice d24 d20 word)
  base = unpack (slice d19 d15 word)
  width = slice d13 d12 word
  offset4to0 = slice d11 d7 word
  offset = unpack (offset11to5 ++# offset4to0)

decodeROpcode :: Bool -> Bool -> BitVector 3 -> Maybe ROpcode
decodeROpcode False True threeBits = case threeBits of
  0b000 -> pure MUL
  0b001 -> pure MULH
  0b010 -> pure MULHSU
  0b011 -> pure MULHU
  0b100 -> pure DIV
  0b101 -> pure DIVU
  0b110 -> pure REM
  _     -> pure REMU

decodeROpcode funct7_6 False threeBits = case threeBits of
    0b000 -> pure (if funct7_6 then SUB else ADD)
    0b001 -> pure SLL
    0b010 -> pure SLT
    0b011 -> pure SLTU
    0b100 -> pure XOR
    0b101 -> pure (if funct7_6 then SRA else SRL)
    0b110 -> pure OR
    _     -> pure AND

decodeROpcode _ _ _ = Nothing

decodeRRInstr :: BitVector 32 -> Maybe RegisterRegisterInstr
decodeRRInstr word = do
  opcode <- decodeROpcode funct7_6 funct7_1 (slice d14 d12 word)
  pure (RInstr opcode src2 src1 dest)
 where
  funct7_6 = testBit word 30
  funct7_1 = testBit word 25
  src2   = unpack (slice d24 d20 word)
  src1   = unpack (slice d19 d15 word)
  dest   = unpack (slice d11  d7 word)


decodeIOpcode :: BitVector 3 -> Maybe IOpcode
decodeIOpcode threeBits = case threeBits of
  0b000 -> pure ADDI
  0b010 -> pure SLTI
  0b011 -> pure SLTIU
  0b100 -> pure XORI
  0b110 -> pure ORI
  0b111 -> pure ANDI
  _ -> Nothing

decodeShiftOpcode :: Bool -> BitVector 3 -> Maybe ShiftOpcode
decodeShiftOpcode funct7_6 threeBits = case threeBits of
  0b001 -> pure SLLI
  0b101 -> pure (if funct7_6 then SRAI else SRLI)
  _ -> Nothing

isShiftOpcode :: BitVector 3 -> Bool
isShiftOpcode word = word == 0b001 || word == 0b101

decodeRIInstr :: BitVector 32 -> Maybe RegisterImmediateInstr
decodeRIInstr word =
  if isShiftOpcode opcode then do
    let shamt = unpack (slice d24 d20 word)
    shiftOpcode <- decodeShiftOpcode (testBit word 30) opcode
    pure (ShiftInstr shiftOpcode shamt src dest)
  else do
    let immediate = unpack (slice d31 d20 word)
    iOpcode <- decodeIOpcode opcode
    pure (IInstr iOpcode immediate src dest)
 where
  dest = unpack (slice d11 d7 word)
  src = unpack (slice d19 d15 word)
  opcode = slice d14 d12 word

decodeLUIInstr :: BitVector 32 -> Maybe RegisterImmediateInstr
decodeLUIInstr word = pure (LUI immediate dest)
 where
  dest = unpack (slice d11 d7 word)
  immediate = unpack (slice d31 d12 word)

decodeAUIPCInstr :: BitVector 32 -> Maybe RegisterImmediateInstr
decodeAUIPCInstr word = pure (AUIPC immediate dest)
 where
  dest = unpack (slice d11 d7 word)
  immediate = unpack (slice d31 d12 word)

decodeSyncOrderingAtBit :: Int -> BitVector 32 -> SyncOrdering
decodeSyncOrderingAtBit i word =
  SyncOrd
    (testBit word (i + 3))
    (testBit word (i + 2))
    (testBit word (i + 1))
    (testBit word i)

decodeSyncInstr :: BitVector 32 -> Maybe SynchronizationInstr
decodeSyncInstr word =
  if testBit word 12
    then pure FENCEI
    else pure
           (FENCE
              (decodeSyncOrderingAtBit 24 word)
              (decodeSyncOrderingAtBit 20 word))

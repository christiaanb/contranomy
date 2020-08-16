
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

module Contranomy.Encode
  ( encodeInstruction
  )
where

import Clash.Prelude hiding (reads)

import Contranomy.Clash.Extra
import Contranomy.RV32IM

encodeInstruction :: Instr -> Word32
encodeInstruction (BranchInstr branchInst) = encodeBranchInst branchInst
encodeInstruction (CSRInstr csrInstr) = encodeCSRInstr csrInstr
encodeInstruction (EnvironmentInstr envInstr) = encodeEnvInstr envInstr
encodeInstruction (JumpInstr jumpInstr) = encodeJumpInstr jumpInstr
encodeInstruction (MemoryInstr memInstr) = encodeMemoryInstr memInstr
encodeInstruction (RRInstr rrInstr) = encodeRegisterRegisterInstr rrInstr
encodeInstruction (RIInstr riInstr) = encodeRegisterImmediateInstr riInstr
encodeInstruction (SyncInstr syncInstr) = encodeSynchronizationInstr syncInstr

-- | imm[12] | imm[10:5] | rs2 | rs1 | funct3 | imm[4:1] | imm[11] | opcode
encodeBranchInst :: BranchInstr -> BitVector 32
encodeBranchInst (Branch offset cond src2 src1) =
  ((offset' .&. bit 11) `shiftL` 20) .|.
  ((bitsDownTo 9 4 offset') `shiftL` 21) .|.
  (encodeRegister src2 `shiftL` 20) .|.
  (encodeRegister src1 `shiftL` 15) .|.
  (encodeBranchCond cond `shiftL` 12) .|.
  ((bitsDownTo 3 0 offset') `shiftL` 8) .|.
  ((offset' .&. bit 10) `shiftR` 3) .|.
  0b1100011
  where
    offset' :: Word32
    offset' = fromIntegral offset

encodeBranchCond :: BranchCond -> Word32
encodeBranchCond BEQ = 0b000
encodeBranchCond BNE = 0b001
encodeBranchCond BLT = 0b100
encodeBranchCond BGE = 0b101
encodeBranchCond BLTU = 0b110
encodeBranchCond BGEU = 0b111

encodeCSRInstr :: CSRInstr -> Word32
encodeCSRInstr (CSRRInstr csrType (CSRRegister csr) src dest) =
  (fromIntegral csr `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (opcode' `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1110011
  where
    opcode' =
      case csrType of
        ReadWrite -> 0b001
        ReadSet -> 0b010
        ReadClear -> 0b011
encodeCSRInstr (CSRIInstr csrType (CSRRegister csr) immediate dest) =
  (fromIntegral csr `shiftL` 20) .|.
  (fromIntegral immediate `shiftL` 15) .|.
  (opcode' `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1110011
  where
    opcode' =
      case csrType of
        ReadWrite -> 0b101
        ReadSet -> 0b110
        ReadClear -> 0b111

encodeEnvInstr :: EnvironmentInstr -> Word32
encodeEnvInstr ECALL = 0b1110011
encodeEnvInstr EBREAK = setBit 0b1110011 20
encodeEnvInstr MRET = 0b001100000010 `shiftL` 20 .|. 0b1110011

encodeJumpInstr :: JumpInstr -> Word32
encodeJumpInstr (JAL offset dest) =
  ((offset' .&. bit 19) `shiftL` 12) .|.
  (bitsDownTo 9 0 offset' `shiftL` 21) .|.
  ((offset' .&. bit 10) `shiftL` 10) .|.
  (bitsDownTo 18 11 offset' `shiftL` 1) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b1101111
  where offset' = fromIntegral offset
encodeJumpInstr (JALR offset base dest) =
  (offset' `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  -- 3 zeroes
  (encodeRegister dest `shiftL` 7) .|.
  0b1100111
  where offset' = fromIntegral offset

encodeWidth :: Width -> Word32
encodeWidth Byte = 0b000
encodeWidth Half = 0b001
encodeWidth Word = 0b010

encodeLoadWidth :: LoadWidth -> Word32
encodeLoadWidth (Width w) = encodeWidth w
encodeLoadWidth HalfUnsigned = 0b101
encodeLoadWidth ByteUnsigned = 0b100

encodeMemoryInstr :: MemoryInstr -> Word32
encodeMemoryInstr (LOAD width offset base dest) =
  (offset' `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  (encodeLoadWidth width `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0000011
  where offset' = fromIntegral offset
encodeMemoryInstr (STORE width offset src base) =
  (bitsDownTo 11 5 offset' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 20) .|.
  (encodeRegister base `shiftL` 15) .|.
  (encodeWidth width `shiftL` 12) .|.
  (bitsDownTo 4 0 offset' `shiftL` 7) .|.
  0b0100011
  where offset' = fromIntegral offset

encodeROpcode :: ROpcode -> Word32
encodeROpcode oc =
  case oc of
    ADD -> 0b000
    SUB -> 0b000
    MUL -> 0b000
    SLL -> 0b001
    MULH -> 0b001
    SLT -> 0b010
    MULHSU -> 0b010
    SLTU -> 0b011
    MULHU -> 0b011
    XOR -> 0b100
    DIV -> 0b100
    SRL -> 0b101
    SRA -> 0b101
    DIVU -> 0b101
    OR -> 0b110
    REM -> 0b110
    AND -> 0b111
    REMU -> 0b111

encodeRegisterRegisterInstr :: RegisterRegisterInstr -> Word32
encodeRegisterRegisterInstr (RInstr opcode src2 src1 dest) =
  (funct7 `shiftL` 25) .|.
  (encodeRegister src2 `shiftL` 20) .|.
  (encodeRegister src1 `shiftL` 15) .|.
  (encodeROpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0110011
  where
    funct7 =
      case opcode of
        SUB -> 0b0100000
        SRA -> 0b0100000
        MUL -> 0b0000010
        MULH -> 0b0000010
        MULHSU -> 0b0000010
        MULHU -> 0b0000010
        DIV -> 0b0000010
        DIVU -> 0b0000010
        REM -> 0b0000010
        REMU -> 0b0000010
        _ -> 0

encodeIOpcode :: IOpcode -> Word32
encodeIOpcode oc =
  case oc of
    ADDI -> 0b000
    SLTI -> 0b010
    SLTIU -> 0b011
    XORI -> 0b100
    ORI -> 0b110
    ANDI -> 0b111

encodeShiftOpcode :: ShiftOpcode -> Word32
encodeShiftOpcode oc =
  case oc of
    SLLI -> 0b001
    SRLI -> 0b101
    SRAI -> 0b101
encodeRegisterImmediateInstr :: RegisterImmediateInstr -> Word32
encodeRegisterImmediateInstr (IInstr opcode immediate src dest) =
  (immediate' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (encodeIOpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010011
  where immediate' = fromIntegral immediate
encodeRegisterImmediateInstr (ShiftInstr opcode shamt src dest) =
  (funct7 `shiftL` 25) .|.
  (shamt' `shiftL` 20) .|.
  (encodeRegister src `shiftL` 15) .|.
  (encodeShiftOpcode opcode `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010011
  where shamt' = fromIntegral shamt
        funct7 = case opcode of
          SLLI -> 0
          SRLI -> 0
          SRAI -> 0b0100000
encodeRegisterImmediateInstr (LUI {imm20,dest}) =
  (fromIntegral imm20 `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0110111
encodeRegisterImmediateInstr (AUIPC {imm20,dest}) =
  (fromIntegral imm20 `shiftL` 12) .|.
  (encodeRegister dest `shiftL` 7) .|.
  0b0010111

encodeSyncOrdering :: SyncOrdering -> Word32
encodeSyncOrdering (SyncOrd inp outp reads writes) =
  inp' .|. outp' .|. reads' .|. writes'
  where
    inp'
      | inp = 0b1000
      | otherwise = 0
    outp'
      | outp = 0b0100
      | otherwise = 0
    reads'
      | reads = 0b0010
      | otherwise = 0
    writes'
      | writes = 0b0001
      | otherwise = 0

encodeSynchronizationInstr :: SynchronizationInstr -> Word32
encodeSynchronizationInstr (FENCE pred' succ') =
  (encodeSyncOrdering pred' `shiftL` 24) .|.
  (encodeSyncOrdering succ' `shiftL` 20) .|.
  0b0001111
encodeSynchronizationInstr FENCEI = setBit 0b0001111 12

encodeRegister :: Register -> Word32
encodeRegister = resize . pack

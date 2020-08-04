{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Contranomy.Core (core) where

import Clash.Prelude hiding (cycle, select)

import Contranomy.Decode
import Contranomy.RV32IM
import Contranomy.WishBone

data CoreStage
  = InstructionFetch
  | Execute
  deriving (Generic, NFDataX)

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: Unsigned 32
  , instruction :: Instr
  , registers :: Vec 31 (BitVector 32)
  }
  deriving (Generic, NFDataX)

core ::
  HiddenClockResetEnable dom =>
  ( Signal dom (WishBoneS2M 4)
  , Signal dom (WishBoneS2M 4) ) ->
  ( Signal dom (WishBoneM2S 4 32)
  , Signal dom (WishBoneM2S 4 32)
  )
core = mealyB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 0
    , instruction = noop
    , registers = deepErrorX "undefined"
    }

  transition ::
    CoreState ->
    ( WishBoneS2M 4, WishBoneS2M 4 ) ->
    ( CoreState
    , ( WishBoneM2S 4 32
      , WishBoneM2S 4 32 ))
  transition s@(CoreState { stage = InstructionFetch, pc }) (iBus,_)
    = ( s { stage = if bitCoerce (acknowledge iBus) then
                      Execute
                    else
                      InstructionFetch
          , instruction = decodeInstruction (readData iBus)
          }
      , ( defM2S { addr   = pack pc
                 , cycle  = True
                 , strobe = True
                 }
        , defM2S ) )

  transition s@(CoreState { stage = Execute, instruction, pc, registers }) (_,dBus)
    =
    let registers0 = 0 :> registers

        dstReg = case instruction of
          RRInstr (RInstr {dest}) -> Just dest
          RIInstr iinstr -> case iinstr of
            IInstr {dest} -> Just dest
            ShiftInstr {dest} -> Just dest
            LUI {dest} -> Just dest
            AUIPC {dest} -> Just dest
          CSRInstr {} -> error "Not yet implemented"
          MemoryInstr minstr -> case minstr of
            LOAD {dest} | bitCoerce (acknowledge dBus) -> Just dest
            _ -> Nothing
          JumpInstr jinstr -> case jinstr of
            JAL {dest} -> Just dest
            JALR {dest} -> Just dest
          _ -> Nothing

        aluResult = case instruction of
          RRInstr (RInstr {opcode,src1,src2}) ->
            let arg1 = registers0 !! src1
                arg2 = registers0 !! src2
                shiftAmount = unpack (resize (slice d4 d0 arg2))
            in case opcode of
              ADD -> arg1 + arg2
              SUB -> arg1 - arg2
              SLT -> if (unpack arg1 :: Signed 32) < unpack arg2 then
                       1
                     else
                       0
              SLTU -> if arg1 < arg2 then
                        1
                      else
                        0
              AND -> arg1 .&. arg2
              OR -> arg1 .|. arg2
              XOR -> arg1 `xor` arg2
              SLL -> shiftL arg1 shiftAmount
              SRL -> shiftR arg1 shiftAmount
              SRA -> pack (shiftR (unpack arg1 :: Signed 32) shiftAmount)
              MUL -> error "Not yet implemented"
              MULH -> error "Not yet implemented"
              MULHSU -> error "Not yet implemented"
              MULHU -> error "Not yet implemented"
              DIV -> error "Not yet implemented"
              DIVU -> error "Not yet implemented"
              REM -> error "Not yet implemented"
              REMU -> error "Not yet implemented"
          RIInstr iinstr -> case iinstr of
            IInstr {iOpcode,src,imm12} ->
              let arg1 = registers0 !! src
                  arg2 = signExtend imm12
              in case iOpcode of
                ADDI -> arg1 + arg2
                SLTI -> if (unpack arg1 :: Signed 32) < unpack arg2 then
                          1
                        else
                          0
                SLTIU -> if arg1 < arg2 then
                           1
                         else
                           0
                XORI -> arg1 `xor` arg2
                ORI -> arg1 .|. arg2
                ANDI -> arg1 .&. arg2
            ShiftInstr {sOpcode,src,shamt} ->
              let arg1 = registers0 !! src
                  shiftAmount = unpack (resize shamt)
              in case sOpcode of
                SLLI -> shiftL arg1 shiftAmount
                SRLI -> shiftR arg1 shiftAmount
                SRAI -> pack (shiftR (unpack arg1 :: Signed 32) shiftAmount)
            LUI {imm20} ->
              imm20 ++# 0
            AUIPC {imm20} ->
              pack pc + (imm20 ++# 0)
          MemoryInstr minstr -> case minstr of
            LOAD {loadWidth} -> case loadWidth of
              Width Byte -> signExtend (slice d7 d0 (readData dBus))
              Width Half -> signExtend (slice d16 d0 (readData dBus))
              HalfUnsigned -> zeroExtend (slice d16 d0 (readData dBus))
              ByteUnsigned -> zeroExtend (slice d7 d0 (readData dBus))
              _ -> readData dBus
            _ -> 0
          JumpInstr jinstr -> case jinstr of
            JAL {imm} -> signExtend imm + 4
            JALR {offset,base} ->
              (slice d31 d1 (registers0 !! base + signExtend offset) ++# 0) + 4
          _ -> 0
    in
      ( s { stage = case instruction of
              MemoryInstr {} ->
                if bitCoerce (acknowledge dBus) then
                  InstructionFetch
                else
                  Execute
              _ -> InstructionFetch
          , pc = case instruction of
              BranchInstr (Branch {imm,cond,src1,src2}) ->
                let arg1 = registers0 !! src1
                    arg2 = registers0 !! src2
                    taken = case cond of
                      BEQ -> arg1 == arg2
                      BNE -> arg1 /= arg2
                      BLT -> (unpack arg1 :: Signed 32) < unpack arg2
                      BLTU -> arg1 < arg2
                      BGE -> (unpack arg2 :: Signed 32) > unpack arg2
                      BGEU -> arg1 > arg2
                in if taken then
                     pc + unpack (signExtend imm `shiftL` 1)
                   else
                     pc + 4
              JumpInstr jinstr -> case jinstr of
                JAL {imm} ->
                  unpack (signExtend imm `shiftL` 1)
                JALR {offset,base} ->
                  unpack (slice d31 d1 (registers0 !! base + signExtend offset) ++# 0)
              MemoryInstr {} | not (bitCoerce (acknowledge dBus)) -> pc
              _ -> pc+4
          , registers = case dstReg of
              Just dst -> tail (replace dst aluResult registers0)
              Nothing  -> registers
          }
      , ( defM2S
        , case instruction of
            MemoryInstr minstr -> case minstr of
              LOAD {loadWidth,offset,base} ->
                defM2S
                  { addr   = registers0 !! base + signExtend offset
                  , select = loadWidthSelect loadWidth
                  , cycle  = True
                  , strobe = True
                  }
              STORE {width,offset,src,base} ->
                defM2S
                  { addr = registers0 !! base + signExtend offset
                  , writeData = registers0 !! src
                  , select = loadWidthSelect (Width width)
                  , cycle = True
                  , strobe = True
                  , writeEnable = True
                  }
            _ -> defM2S
        ) )

  loadWidthSelect :: LoadWidth -> BitVector 4
  loadWidthSelect lw = case lw of
    Width Byte -> 0b0001
    Width Half -> 0b0011
    Width Word -> 0b1111
    HalfUnsigned -> 0b0011
    ByteUnsigned -> 0b0001

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

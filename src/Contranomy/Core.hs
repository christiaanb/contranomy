{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DisambiguateRecordFields #-}
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
  , registers :: Vec 31 (BitVector 32)
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

  transition s@(CoreState { stage = Execute, instruction, pc, registers }) (iBus,dBus)
    =
    let registers0 = 0 :> registers

        dstReg = case instruction of
          RRInstr (RInstr {dest} ) -> Just dest
          RIInstr iinstr -> case iinstr of
            IInstr {dest} -> Just dest
            ShiftInstr {dest} -> Just dest
            LUI {dest} -> Just dest
            AUIPC {dest} -> Just dest
          CSRInstr {} -> error "Not yet implemented"
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
          _ -> 0

    in
      ( s { pc = case instruction of
              BranchInstr {} -> error "Not yet implemented"
              JumpInstr {} -> error "Not yet implemented"
              _ -> pc + 4
          , registers = case dstReg of
              Just dst -> tail (replace dst aluResult registers0)
              Nothing  -> registers
          }
      , ( defM2S
        , defM2S
        ) )

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

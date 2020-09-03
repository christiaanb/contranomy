{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Contranomy.Core where

import Control.Lens
import Data.Generics.Labels ()
import Data.Maybe

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Class.AutoReg as AutoReg
import Clash.Prelude hiding (cycle, select)

import Contranomy.Clash.Extra
-- import Contranomy.Decode
-- import Contranomy.Encode
import Contranomy.Instruction
import Contranomy.RegisterFile
-- import Contranomy.RV32IM
import Contranomy.RVFI
import Contranomy.WishBone

type PC = BitVector 30

data CoreStage
  = InstructionFetch
  | Execute Bool
  deriving (Generic, NFDataX, AutoReg)

data MStatus
  = MStatus
  { mie :: Bool
  , mpie :: Bool
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MStatus

data InterruptMode
  = Direct {trapBase :: (BitVector 30)}
  | Vectored {trapBase :: (BitVector 30)}
  deriving (Generic, NFDataX, AutoReg)

{-# ANN module (DataReprAnn
                  $(liftQ [t|InterruptMode|])
                  32
                  [ ConstrRepr 'Direct   (1 `downto` 0) 0 [31 `downto` 2]
                  , ConstrRepr 'Vectored (1 `downto` 0) 1 [31 `downto` 2]
                  ]) #-}
deriveBitPack [t| InterruptMode |]

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
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MachineState

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: PC
  , instruction :: MachineWord
  , registers :: RegisterFile
  , machineState :: MachineState
  , rvfiOrder :: Unsigned 64
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CoreState

type TimerInterrupt = Bool
type SoftwareInterrupt = Bool
type ExternalInterrupt = MachineWord

data CoreIn
  = CoreIn
  { iBusS2M :: "iBusWishbone" ::: WishBoneS2M 4
  , dBusS2M :: "dBusWishbone" :::  WishBoneS2M 4
  , timerInterrupt :: "timerInterrupt" ::: TimerInterrupt
  , softwareInterrupt :: "softwareInterrupt" ::: SoftwareInterrupt
  , externalInterrupt :: "externalInterrupt" ::: ExternalInterrupt
  }

data CoreOut
  = CoreOut
  { iBusM2S :: "iBusWishbone" ::: WishBoneM2S 4 30
  , dBusM2S :: "dBusWishbone" ::: WishBoneM2S 4 30
  , rvfi :: "" ::: RVFI
  }

defCoreOut :: CoreOut
defCoreOut = CoreOut { iBusM2S = defM2S, dBusM2S = defM2S, rvfi = defRVFI }

core ::
  HiddenClockResetEnable dom =>
  Signal dom CoreIn ->
  Signal dom CoreOut
core = mealyAuto transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 0
    , instruction = 0
    , registers = emptyRegisterFile
    , machineState = machineStart
    , rvfiOrder = 0
    }

  machineStart
    = MachineState
    { mstatus = MStatus { mie = False, mpie = False }
    , mcause = MCause { interrupt = False, code = 0 }
    , mtvec = Direct 0
    , mie = Mie { meie = False, mtie = False, msie = False }
    , mscratch = 0
    , mepc = 0
    , mtval = 0
    }

transition ::
  CoreState ->
  CoreIn ->
  ( CoreOut
  , CoreState )
transition s@CoreState{stage=InstructionFetch, pc} CoreIn{iBusS2M} = withState s do
  #stage .= if err iBusS2M then
              Execute False
            else if acknowledge iBusS2M then
              Execute True
            else
              InstructionFetch

  #instruction .= readData iBusS2M

  return $ defCoreOut
         { iBusM2S = defM2S
                   { addr   = pc
                   , select = 0b1111
                   , cycle  = True
                   , strobe = True } }

transition s@CoreState{stage=Execute False, pc, machineState} _ = withState s do
  let MachineState{mstatus=MStatus{mie},mtvec} = machineState
  #machineState .= machineState
                 { mstatus = MStatus { mpie = mie, mie = False }
                 , mcause  = INSTRUCTION_ACCESS_FAULT
                 , mepc    = pc
                 , mtval   = pc ++# 0 }

  #pc .= 0 ++# slice d29 d2 (trapBase mtvec)

  #stage .= InstructionFetch

  return defCoreOut

transition s@CoreState{stage=Execute True,instruction,registers,pc} CoreIn{dBusS2M} = withState s do
  let DecodedInstruction
        { opcode, rd, rs1, rs2, iop, srla, shamt, isSub, imm12I, imm20U, imm12S, func3 }
        = decodeInstruction instruction

      (rs1Val,rs2Val) = readRegisterFile registers rs1 rs2

      aluArg1 = case opcode of
                  LUI   -> 0
                  AUIPC -> pc ++# 0
                  JAL   -> pc ++# 0
                  JALR  -> pc ++# 0
                  _     -> rs1Val
      aluArg2 = case opcode of
                  LUI    -> imm20U ++# 0
                  AUIPC  -> imm20U ++# 0
                  JAL    -> 4
                  JALR   -> 4
                  OP     -> if isSub then negate rs2Val else rs2Val
                  STORE  -> signExtend imm12S
                  _      -> signExtend imm12I
      aluOp   = case opcode of
                  OP     -> iop
                  OP_IMM -> iop
                  _      -> ADD

      aluIResult = case aluOp of
        ADD  -> aluArg1 + aluArg2
        SLL  -> aluArg1 `shiftL` unpack (zeroExtend shamt)
        SLT  -> boolToMachineWord ((unpack aluArg1 :: Signed 32) < unpack aluArg2)
        SLTU -> boolToMachineWord (aluArg1 < aluArg2)
        XOR  -> aluArg2 `xor` aluArg2
        SR   -> case srla of
                  Logical    -> aluArg1 `shiftR` unpack (zeroExtend shamt)
                  Arithmetic -> pack ((unpack aluArg1 :: Signed 32) `shiftR` unpack (zeroExtend shamt))
        OR   -> aluArg1 .|. aluArg2
        AND  -> aluArg2 .&. aluArg2

      (dBusM2S,ldVal,busErr,addrUnaligned,lsFinished) =
        loadStoreUnit opcode func3 aluIResult rs2Val dBusS2M

      rdVal = case opcode of
        BRANCH   -> Nothing
        MISC_MEM -> Nothing
        SYSTEM   -> Nothing
        STORE    -> Nothing
        LOAD     -> ldVal
        _        -> Just aluIResult

  #registers .= writeRegisterFile registers ((rd,) <$> rdVal)

  return $ defCoreOut
         { dBusM2S = dBusM2S }

loadStoreUnit ::
  Opcode ->
  -- func3
  BitVector 3 ->
  -- address
  MachineWord ->
  -- store
  MachineWord ->
  -- DBUS
  WishBoneS2M 4 ->
  (WishBoneM2S 4 30, Maybe MachineWord, Bool, Bool, Bool)
loadStoreUnit opcode func3 addr store dBusS2M = case opcode of
  LOAD -> let
    lextend ::
      (KnownNat n, n <= 32) =>
      BitVector (32 - n) ->
      MachineWord
    lextend = if unpack (slice d2 d2 func3) then
                zeroExtend
              else
                signExtend

    loadData = case lsw of
                 Word -> readData dBusS2M
                 Half -> lextend (
                         slice d15 d0 (
                         readData dBusS2M `shiftR` shiftAmount))
                 Byte -> lextend (
                         slice d7 d0 (
                         readData dBusS2M `shiftR` shiftAmount))
    in
    ( defM2S
        { addr = slice d31 d2 addr
        , select = mask
        , cycle = True
        , strobe = True
        }
    , if unaligned || err dBusS2M || not (acknowledge dBusS2M) then
        Nothing
      else
        Just loadData
    , err dBusS2M
    , unaligned
    , acknowledge dBusS2M
    )
  STORE -> let
    storeData = case lsw of
                  Word -> store
                  Half -> store `shiftL` shiftAmount
                  Byte -> store `shiftL` shiftAmount
    in
    ( defM2S
       { addr = slice d31 d2 addr
       , writeData = storeData
       , select = mask
       , cycle = True
       , strobe = True
       , writeEnable = True
       }
    , Nothing
    , err dBusS2M
    , unaligned
    , acknowledge dBusS2M
    )
  _ ->
    ( defM2S
    , undefined
    , False
    , False
    , True
    )
 where
  lsw = unpack (slice d1 d0 func3)

  alignment = slice d1 d0 addr

  unaligned = case lsw of
    Word -> alignment /= 0
    Half -> testBit alignment 0
    Byte -> False

  mask = case lsw of
    Word -> 0b1111
    Half -> case alignment of
              2 -> 0b1100
              _ -> 0b0011
    Byte -> case alignment of
              3 -> 0b1000
              2 -> 0b0100
              1 -> 0b0010
              _ -> 0b0001

  shiftAmount = case lsw of
    Word -> 0
    Half -> case alignment of
              2 -> 16
              _ -> 0
    Byte -> case alignment of
              3 -> 24
              2 -> 16
              1 -> 8
              _ -> 0

data DecodedInstruction
  = DecodedInstruction
  { opcode :: Opcode
  , rd     :: Register
  , rs1    :: Register
  , rs2    :: Register
  , iop    :: IOp
  , srla   :: ShiftRight
  , shamt  :: BitVector 5
  , isSub  :: Bool
  , isM    :: Bool
  , mop    :: MOp
  , imm20U :: BitVector 20
  , imm20J :: BitVector 20
  , imm12I :: BitVector 12
  , imm12S :: BitVector 12
  , imm12B :: BitVector 12
  , func3  :: BitVector 3
  }

decodeInstruction ::
  MachineWord ->
  DecodedInstruction
decodeInstruction w
  = DecodedInstruction
  { opcode = unpack (slice d6 d0 w)
  , rd     = unpack (slice d11 d7 w)
  , rs1    = unpack (slice d19 d15 w)
  , rs2    = unpack (slice d24 d20 w)
  , iop    = unpack (slice d14 d12 w)
  , srla   = unpack (slice d30 d30 w)
  , shamt  = unpack (slice d24 d20 w)
  , isSub  = unpack (slice d30 d30 w)
  , isM    = unpack (slice d25 d25 w)
  , mop    = unpack (slice d14 d12 w)
  , imm20U = slice d31 d12 w
  , imm20J = slice d31 d31 w ++#
             slice d19 d12 w ++#
             slice d20 d20 w ++#
             slice d30 d21 w

  , imm12I = slice d31 d20 w
  , imm12S = slice d31 d25 w ++# slice d11 d7 w
  , imm12B = slice d31 d31 w ++#
             slice  d7  d7 w ++#
             slice d30 d25 w ++#
             slice d11  d8 w
  , func3  = slice d14 d12 w
  }

boolToMachineWord :: Bool -> MachineWord
boolToMachineWord = zeroExtend . pack
{-# INLINE boolToMachineWord #-}

-- isLegalInstruction :: Opcode -> Bool
-- isLegalInstruction o = case o of
--   LUI -> True
--   AUIPC -> True
--   JAL -> True
--   JALR -> True
--   Branch -> True
--   Load -> True
--   Store -> True
--   Imm -> True
--   RegReg -> True
--   MiscMem -> True
--   System -> True
--   _ -> False

-- data MachineState
--   = MachineState
--   { mstatus :: MStatus
--   , mcause :: MCause
--   , mtvec :: InterruptMode
--   , mie :: Mie
--   , mscratch :: MachineWord
--   , mepc :: PC
--   , mtval :: MachineWord
--   }
--   deriving (Generic, NFDataX)

-- toRVFI ::
--   -- | Current decoded instruction
--   Instr ->
--   -- | Order
--   Unsigned 64 ->
--   -- | Registers
--   RegisterFile ->
--   -- | Destination register
--   Maybe Register ->
--   -- | Destination register write data
--   BitVector 32 ->
--   -- | Current PC
--   BitVector 30 ->
--   -- | Next PC
--   BitVector 30 ->
--   -- | Trap
--   Bool ->
--   -- | Data Bus M2S
--   WishBoneM2S 4 30 ->
--   -- | Data Bus S2M
--   WishBoneS2M 4 ->
--   RVFI
-- toRVFI instruction rvfiOrder registers dstReg aluResult pc pcN trap dBusM2S dBusS2M =
--   let rs1AddrN = case instruction of
--                    BranchInstr (Branch {src1}) -> src1
--                    CSRInstr (CSRRInstr {src}) -> src
--                    JumpInstr (JALR {base}) -> base
--                    MemoryInstr (LOAD {base}) -> base
--                    MemoryInstr (STORE {base}) -> base
--                    RRInstr (RInstr {src1}) -> src1
--                    RIInstr (IInstr {src}) -> src
--                    RIInstr (ShiftInstr {src}) -> src
--                    _ -> X0

--       rs2AddrN = case instruction of
--                    BranchInstr (Branch {src2}) -> src2
--                    MemoryInstr (STORE {src}) -> src
--                    RRInstr (RInstr {src2}) -> src2
--                    _ -> X0
--   in  defRVFI
--         { valid = case instruction of
--             MemoryInstr {} | not trap -> acknowledge dBusS2M
--             _ -> True
--         , order    = rvfiOrder
--         , insn     = encodeInstruction instruction
--         , trap     = trap
--         , rs1Addr  = rs1AddrN
--         , rs2Addr  = rs2AddrN
--         , rs1RData = readRegisterFile registers rs1AddrN
--         , rs2RData = readRegisterFile registers rs2AddrN
--         , rdAddr   = fromMaybe X0 dstReg
--           -- Since we take the tail for the written to registers, this is okay
--         , rdWData  = case fromMaybe X0 dstReg of
--                        X0 -> 0
--                        _ -> aluResult
--         , pcRData  = pc ++# 0
--         , pcWData  = pcN ++# 0
--         , memAddr  = if trap then 0 else addr dBusM2S ++# 0
--         , memRMask = if strobe dBusM2S && not (writeEnable dBusM2S) then
--                        select dBusM2S
--                      else
--                        0
--         , memWMask = if strobe dBusM2S && writeEnable dBusM2S then
--                        select dBusM2S
--                      else
--                        0
--         , memRData = if strobe dBusM2S && not (writeEnable dBusM2S) then
--                        readData dBusS2M
--                      else
--                        0
--         , memWData = if strobe dBusM2S && writeEnable dBusM2S then
--                        writeData dBusM2S
--                      else
--                        0
--         }

-- csrWrite ::
--   CSRType ->
--   Word32 ->
--   Maybe Word32 ->
--   Word32
-- csrWrite ReadWrite oldValue Nothing = oldValue
-- csrWrite ReadWrite _ (Just newValue) = newValue

-- csrWrite ReadSet oldValue Nothing = oldValue
-- csrWrite ReadSet oldValue (Just newValue) = oldValue .|. newValue

-- csrWrite ReadClear oldValue Nothing = oldValue
-- csrWrite ReadClear oldValue (Just newValue) = oldValue .&. newValue

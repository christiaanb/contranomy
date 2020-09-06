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
import Control.Monad
import Control.Monad.Trans.State (State)

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Class.AutoReg as AutoReg
import Clash.Prelude hiding (cycle, select)

import Contranomy.Clash.Extra
import Contranomy.Instruction
import Contranomy.RVFI
import Contranomy.WishBone

type PC = BitVector 30

data CoreStage
  = InstructionFetch
  | Decode Bool
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
  (Signal dom CoreIn, Signal dom (MachineWord, MachineWord)) ->
  (Signal dom CoreOut, Signal dom (Register, Register, Maybe (Register, MachineWord)))
core = mealyAutoB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 0
    , instruction = 0
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
  (CoreIn, (MachineWord, MachineWord)) ->
  ( (CoreOut, (Register, Register, Maybe (Register, MachineWord)))
  , CoreState )
transition s@CoreState{stage=InstructionFetch, pc} (CoreIn{iBusS2M},_) = runState' s do
  #stage .= if err iBusS2M then
              Decode True
            else if acknowledge iBusS2M then
              Decode False
            else
              InstructionFetch

  #instruction .= readData iBusS2M

  return . (,(undefined,undefined,Nothing)) $ defCoreOut
         { iBusM2S = defM2S
                   { addr   = pc
                   , select = 0b1111
                   , cycle  = True
                   , strobe = True } }

transition s@CoreState{stage=Decode instrFault,instruction} _ = runState' s do
  #stage .= Execute instrFault
  let DecodedInstruction {rs1,rs2} = decodeInstruction instruction
  return (defCoreOut,(rs1,rs2,Nothing))

transition s@CoreState{stage=Execute instrFault,instruction,pc,machineState,rvfiOrder}
  (CoreIn{dBusS2M},(rs1Val,rs2Val)) = runState' s do
  let DecodedInstruction
        { opcode, rd, rs1, rs2, iop, srla, isSub, imm12I, imm20U, imm12S }
        = decodeInstruction instruction

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
                  OP     -> case aluOp of
                              ADD | isSub -> negate rs2Val
                              _ -> rs2Val
                  STORE  -> signExtend imm12S
                  _      -> signExtend imm12I

      aluArg2Shamt = unpack (zeroExtend (slice d4 d0 aluArg2))

      aluOp   = case opcode of
                  OP     -> iop
                  OP_IMM -> iop
                  _      -> ADD

      aluIResult = case aluOp of
        ADD  -> aluArg1 + aluArg2
        SLL  -> aluArg1 `shiftL` aluArg2Shamt
        SLT  -> boolToMachineWord ((unpack aluArg1 :: Signed 32) < unpack aluArg2)
        SLTU -> boolToMachineWord (aluArg1 < aluArg2)
        XOR  -> aluArg1 `xor` aluArg2
        SR   -> case srla of
                  Logical    -> aluArg1 `shiftR` aluArg2Shamt
                  Arithmetic -> pack ((unpack aluArg1 :: Signed 32) `shiftR` aluArg2Shamt)
        OR   -> aluArg1 .|. aluArg2
        AND  -> aluArg1 .&. aluArg2

      (dBusM2S,ldVal,dataFault,addrUnaligned,lsFinished) =
        loadStoreUnit instruction aluIResult rs2Val dBusS2M

      pcN = branchUnit instruction rs1Val rs2Val pc
      psMisaligned = snd pcN /= 0

  csrVal@(csrOld,_) <- csrUnit instruction instrFault rs1Val machineState

  let rdVal = case opcode of
        BRANCH   -> Nothing
        MISC_MEM -> Nothing
        SYSTEM   -> csrOld
        STORE    -> Nothing
        LOAD     -> ldVal
        _        -> Just aluIResult

  (trap,pcN1) <-
    handleExceptions s opcode instrFault dataFault addrUnaligned psMisaligned pcN

  let registerWrite = if trap || rd == X0 then Nothing else (rd,) <$> rdVal

  when lsFinished do
    #pc .= pcN1
    #rvfiOrder += 1
    #stage .= InstructionFetch

  return . (,(rs1,rs2,registerWrite)) $ defCoreOut
         { dBusM2S = dBusM2S
         , rvfi = toRVFI lsFinished rvfiOrder instruction trap rs1Val rs2Val
                    registerWrite pc pcN1 dBusM2S dBusS2M csrVal
         }


toRVFI ::
  -- lsFinished
  Bool ->
  -- rvfiOrder
  Unsigned 64 ->
  -- instruction
  BitVector 32 ->
  -- trap
  Bool ->
  -- rs1 value
  MachineWord ->
  -- rs2 value
  MachineWord ->
  -- rdVal
  Maybe (Register,MachineWord) ->
  -- pc
  PC ->
  -- pcN
  PC ->
  -- dbusM2S
  WishBoneM2S 4 30 ->
  -- dbusS2M
  WishBoneS2M 4 ->
  -- MISA CRS
  (Maybe MachineWord, MachineWord) ->
  RVFI
toRVFI lsFinished rvfiOrder instruction trap rs1Val rs2Val rdVal pc pcN dBusM2S dBusS2M csrVal
  = defRVFI
  { valid    = lsFinished
  , order    = rvfiOrder
  , insn     = instruction
  , trap     = trap
  , rs1Addr  = rs1
  , rs2Addr  = rs2
  , rs1RData = rs1Val
  , rs2RData = rs2Val
  , rdAddr   = maybe X0 fst rdVal
  , rdWData  = maybe 0 snd rdVal
  , pcRData  = pc ++# 0
  , pcWData  = pcN ++# 0
  , memAddr  = if trap then 0 else addr dBusM2S ++# 0
  , memRMask = if strobe dBusM2S && not (writeEnable dBusM2S) then
                  select dBusM2S
                else
                  0
  , memWMask = if strobe dBusM2S && writeEnable dBusM2S then
                  select dBusM2S
                else
                  0
  , memRData = if strobe dBusM2S && not (writeEnable dBusM2S) then
                  readData dBusS2M
                else
                  0
  , memWData = if strobe dBusM2S && writeEnable dBusM2S then
                  writeData dBusM2S
                else
                  0
  , misaCSR  = case csrVal of
      (Just old,new)
        | MISA <- CSRRegister srcDest
        -> RVFICSR { rmask = maxBound
                   , wmask = maxBound
                   , rdata = old
                   , wdata = new
                   }
      _ -> defRVFICSR {rmask = 4}
  }
 where
  DecodedInstruction {rs1,rs2,imm12I=srcDest} = decodeInstruction instruction

handleExceptions ::
  CoreState ->
  Opcode ->
  -- instructionAccessFault
  Bool ->
  -- dataAccessFault
  Bool ->
  -- addrUnalignedFault
  Bool ->
  -- pcUnalignedFault
  Bool ->
  -- Next PC
  (PC,BitVector 2) ->
  State CoreState (Bool,PC)
handleExceptions CoreState{pc,machineState} opcode instrFault dataFault
  addrMisaligned pcMisaligned (pcN,align) = do
    let trap = instrFault || dataFault || addrMisaligned || pcMisaligned
    let MachineState{mstatus=MStatus{mie},mtvec} = machineState

    if trap then do
      #machineState .= machineState
                     { mstatus = MStatus { mpie = mie, mie = False }
                     , mcause  = if instrFault then
                                   INSTRUCTION_ACCESS_FAULT
                                 else if pcMisaligned then
                                   INSTRUCTION_ADDRESS_MISALIGNED
                                 else if addrMisaligned then
                                   case opcode of
                                     LOAD -> LOAD_ADDRESS_MISALIGNED
                                     _ -> STORE_ADDRESS_MISALIGNED
                                 else -- dataFault
                                   case opcode of
                                     LOAD -> LOAD_ACCESS_FAULT
                                     _ -> STORE_ACCESS_FAULT


                     , mepc    = pc ++# 0
                     , mtval   = if pcMisaligned then
                                  pcN ++# align
                                else
                                  pc ++# 0
                     }
      let pcN1 = zeroExtend (slice d29 d2 (trapBase mtvec))
      return (True,pcN1)
    else do
      return (False,pcN)

loadStoreUnit ::
  -- Instruction
  BitVector 32 ->
  -- address
  MachineWord ->
  -- store
  MachineWord ->
  -- DBUS
  WishBoneS2M 4 ->
  (WishBoneM2S 4 30, Maybe MachineWord, Bool, Bool, Bool)
loadStoreUnit instruction addr store dBusS2M = case opcode of
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
    , lsFinished
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
    , lsFinished
    )
  _ ->
    ( defM2S
    , undefined
    , False
    , False
    , True
    )
 where
  DecodedInstruction {opcode,func3} = decodeInstruction instruction

  lsFinished = unaligned || err dBusS2M || acknowledge dBusS2M

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

branchUnit ::
  -- instruction
  BitVector 32 ->
  -- rs1
  MachineWord ->
  -- rs2
  MachineWord ->
  -- PC
  PC ->
  (PC, BitVector 2)
branchUnit instruction rs1Val rs2Val pc = case opcode of
  BRANCH ->
    let taken = case unpack func3 of
                  BEQ -> rs1Val == rs2Val
                  BNE -> rs1Val /= rs2Val
                  BLT -> (unpack rs1Val :: Signed 32) < unpack rs2Val
                  BLTU -> rs1Val < rs2Val
                  BGE -> (unpack rs1Val :: Signed 32) >= unpack rs2Val
                  BGEU -> rs1Val >= rs2Val
                  _ -> False
     in if taken then
          let (offset,align) = split (signExtend imm12B `shiftL` 1 :: MachineWord)
           in (pc + offset,align)
        else
          (pc + 1, 0)

  JAL ->
    let (offset,align) = split (signExtend imm20J `shiftL` 1 :: MachineWord)
     in (pc + offset, align)

  JALR ->
    let (pcN, align) = split (rs1Val + signExtend imm12I)
        alignLSBZero = align .&. 0b10
     in (pcN, alignLSBZero)

  _ ->
    (pc + 1, 0)
 where
  DecodedInstruction {opcode,func3,imm12B,imm12I,imm20J} = decodeInstruction instruction


csrUnit ::
  BitVector 32 ->
  Bool ->
  MachineWord ->
  MachineState ->
  State CoreState (Maybe MachineWord, MachineWord)
csrUnit instruction instrFault rs1Val machineState
  | not instrFault
  , SYSTEM <- opcode
  , func3 /= 0
  = zoom #machineState do
    let MachineState
          {mstatus=MStatus{mie,mpie}
          ,mie=Mie{meie,mtie,msie}
          ,mtvec
          ,mscratch
          ,mcause=MCause{interrupt,code}
          ,mtval
          ,mepc
          } = machineState

    let csrType = unpack (slice d1 d0 func3)
        uimm = pack rs1

    let writeValue0 =
          if testBit func3 2 then
            zeroExtend uimm
          else
            rs1Val
        writeValue1 = case csrType of
          ReadWrite -> Just writeValue0
          _ | uimm == 0 -> Nothing
            | otherwise -> Just writeValue0

    case CSRRegister srcDest of
      MSTATUS -> do
        let oldValue = bitB mpie 7 .|. bitB mie 3
            newValue = csrWrite csrType oldValue writeValue1
        #mstatus .= MStatus {mie=testBit newValue 7,mpie=testBit newValue 3}
        return (Just oldValue, newValue)
      MISA -> do
        let oldValue = bit 30 .|. bit 8
            newValue = csrWrite csrType oldValue writeValue1
        return (Just oldValue, newValue)
      MIE -> do
        let oldValue = bitB meie 11 .|. bitB mtie 7 .|. bitB msie 3
            newValue = csrWrite csrType oldValue writeValue1
        #mie .= Mie {meie=testBit newValue 11
                    ,mtie=testBit newValue 7
                    ,msie=testBit newValue 3
                    }
        return (Just oldValue, newValue)
      MTVEC -> do
        let oldValue = pack mtvec
            newValue = csrWrite csrType oldValue writeValue1
        #mtvec .= unpack newValue
        return (Just oldValue, newValue)
      MSCRATCH -> do
        let oldValue = mscratch
            newValue = csrWrite csrType oldValue writeValue1
        #mscratch .= newValue
        return (Just oldValue, newValue)
      MEPC -> do
        let oldValue = mepc ++# 0
            newValue = csrWrite csrType oldValue writeValue1
        #mepc .= slice d31 d2 newValue
        return (Just oldValue, newValue)
      MCAUSE -> do
        let oldValue = pack interrupt ++# 0 ++# code
            newValue = csrWrite csrType oldValue writeValue1
        #mcause .= MCause { interrupt = testBit newValue 31, code = truncateB newValue }
        return (Just oldValue, newValue)
      MTVAL -> do
        let oldValue = mtval
            newValue = csrWrite csrType oldValue writeValue1
        #mtval .= newValue
        return (Just oldValue, newValue)
      _ -> return (Nothing, undefined)

  | otherwise
  = return (Nothing, undefined)
 where
  DecodedInstruction {opcode,func3,rs1,imm12I=srcDest} = decodeInstruction instruction

  bitB b i = if b then bit i else 0

  csrWrite ::
    CSRType ->
    MachineWord ->
    Maybe MachineWord ->
    MachineWord
  csrWrite ReadWrite oldValue newValueM = maybe oldValue id newValueM
  csrWrite ReadSet oldValue newValueM   = maybe oldValue (oldValue .|.) newValueM
  csrWrite ReadClear oldValue newValueM = maybe oldValue ((oldValue .&.) . complement) newValueM
  csrWrite _ oldValue _ = oldValue
  {-# INLINE csrWrite #-}

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
{-# INLINE decodeInstruction #-}

boolToMachineWord :: Bool -> MachineWord
boolToMachineWord = zeroExtend . pack
{-# INLINE boolToMachineWord #-}

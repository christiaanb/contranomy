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
{-# LANGUAGE NumericUnderscores #-}
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

import Debug.Trace

type PC = BitVector 30

data CoreStage
  = InstructionFetch
  | Decode
  | Execute
  deriving (Generic, NFDataX, ShowX, AutoReg)

data MStatus
  = MStatus
  { mie :: Bool
  , mpie :: Bool
  }
  deriving (Generic, NFDataX, ShowX)

deriveAutoReg ''MStatus

data InterruptMode
  = Direct {trapBase :: (BitVector 30)}
  | Vectored {trapBase :: (BitVector 30)}
  deriving (Generic, NFDataX, ShowX, AutoReg)

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
  deriving (Generic, NFDataX, ShowX)

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
  , irqmask :: MachineWord
  }
  deriving (Generic, NFDataX, ShowX)

deriveAutoReg ''MachineState

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: PC
  , instruction :: MachineWord
  , machineState :: MachineState
  , rvfiOrder :: Unsigned 64
  }
  deriving (Generic, NFDataX, ShowX)

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
  }

instance Show CoreOut where
  show _ = "CoreOut"

defCoreOut :: CoreOut
defCoreOut = CoreOut { iBusM2S = defM2S, dBusM2S = defM2S }

core ::
  HiddenClockResetEnable dom =>
  (Signal dom CoreIn, Signal dom (MachineWord, MachineWord)) ->
  ( Signal dom CoreOut
  , Signal dom (Register, Register, Maybe (Register, MachineWord))
  , Signal dom RVFI )
core = mealyAutoB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 90
    , instruction = 0
    , machineState = machineStart
    , rvfiOrder = 0
    }

  machineStart
    = MachineState
    { mstatus = MStatus { mie = True, mpie = False }
    , mcause = MCause { interrupt = False, code = 0 }
    , mtvec = Direct 40
    , mie = Mie { meie = True, mtie = False, msie = False }
    , mscratch = 0
    , mepc = 80
    , mtval = 0
    , irqmask = 1
    }

transition ::
  CoreState ->
  (CoreIn, (MachineWord, MachineWord)) ->
  ( (CoreOut, (Register, Register, Maybe (Register, MachineWord)), RVFI)
  , CoreState )
transition
  s@CoreState{stage=InstructionFetch, pc}
  (CoreIn{iBusS2M,softwareInterrupt,timerInterrupt,externalInterrupt},_) = trace (showX s) $ runState' s do
  #instruction .= readData iBusS2M

  let DecodedInstruction {legal} = decodeInstruction (readData iBusS2M)

  let exceptionIn = defExceptionIn
                      { instrAccessFault  = err iBusS2M
                      , instrIllegal      = acknowledge iBusS2M && not legal
                      , softwareInterrupt = softwareInterrupt
                      , timerInterrupt    = timerInterrupt
                      , externalInterrupt = externalInterrupt
                      }
  (trap,pcN) <- handleExceptions s exceptionIn (pc,0)

  #stage .= if trap then
              InstructionFetch
            else if acknowledge iBusS2M then
              Decode
            else
              InstructionFetch

  #pc .= pcN

  return . (,(undefined,undefined,Nothing),defRVFI) $ defCoreOut
         { iBusM2S = defM2S
                   { addr   = pc
                   , select = 0b1111
                   , cycle  = True
                   , strobe = True } }


transition s@CoreState{stage=Decode,instruction} _ = trace (showX s) $ runState' s do
  #stage .= Execute
  let DecodedInstruction {rs1,rs2} = decodeInstruction instruction
  return (defCoreOut,(rs1,rs2,Nothing),defRVFI)


transition s@CoreState{stage=Execute,instruction,pc,machineState,rvfiOrder}
  (CoreIn{dBusS2M,softwareInterrupt,timerInterrupt,externalInterrupt},(rs1Val,rs2Val)) = trace (showX s) $ runState' s do
  let DecodedInstruction
        { opcode, rd, rs1, rs2, iop, srla, isSub, imm12I, imm20U, imm12S, func3 }
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

      (dBusM2S,ldVal,dataAccessFault,dataAddrMisaligned,lsFinished) =
        loadStoreUnit instruction aluIResult rs2Val dBusS2M

  let pcN = branchUnit instruction rs1Val rs2Val pc

  csrVal@(csrOld,_) <-
    csrUnit instruction rs1Val machineState softwareInterrupt timerInterrupt externalInterrupt

  let rdVal = case opcode of
        BRANCH   -> Nothing
        MISC_MEM -> Nothing
        SYSTEM   -> csrOld
        STORE    -> Nothing
        LOAD     -> ldVal
        _        -> Just aluIResult

  let exceptionIn = defExceptionIn
                      { instrAddrMisaligned = snd pcN /= 0
                      , dataAccessFault = dataAccessFault
                      , dataAddrMisaligned = dataAddrMisaligned
                      , breakpoint = case opcode of
                          SYSTEM | func3 == 0 -> imm12I == 1
                          _ -> False
                      , eCall = case opcode of
                          SYSTEM | func3 == 0 -> imm12I == 0
                          _ -> False
                      , mret = case opcode of
                          SYSTEM | func3 == 0 -> imm12I == 0b0011000_00010
                          _ -> False
                      }
  (trap,pcN1) <- handleExceptions s exceptionIn pcN

  let registerWrite = if trap || rd == X0 then Nothing else (rd,) <$> rdVal

  when lsFinished do
    #pc .= pcN1
    #rvfiOrder += 1
    #stage .= InstructionFetch

  let rvfi = toRVFI lsFinished rvfiOrder instruction trap rs1Val rs2Val
                    registerWrite pc pcN1 dBusM2S dBusS2M csrVal

  return . (,(rs1,rs2,registerWrite),rvfi) $ defCoreOut
         { dBusM2S = dBusM2S
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
      (Just old,newVal)
        | MISA <- CSRRegister srcDest
        -> RVFICSR { rmask = maxBound
                   , wmask = maxBound
                   , rdata = old
                   , wdata = newVal
                   }
      _ -> defRVFICSR {rmask = 4}
  }
 where
  DecodedInstruction {rs1,rs2,imm12I=srcDest} = decodeInstruction instruction

data ExceptionIn
  = ExceptionIn
  { instrAccessFault    :: Bool
  , instrAddrMisaligned :: Bool
  , instrIllegal        :: Bool
  , dataAccessFault     :: Maybe MachineWord
  , dataAddrMisaligned  :: Maybe MachineWord
  , breakpoint          :: Bool
  , eCall               :: Bool
  , mret                :: Bool
  , timerInterrupt      :: Bool
  , softwareInterrupt   :: Bool
  , externalInterrupt   :: BitVector 32
  }

defExceptionIn :: ExceptionIn
defExceptionIn
  = ExceptionIn
  { instrAccessFault    = False
  , instrAddrMisaligned = False
  , instrIllegal        = False
  , dataAccessFault     = Nothing
  , dataAddrMisaligned  = Nothing
  , breakpoint          = False
  , eCall               = False
  , mret                = False
  , timerInterrupt      = False
  , softwareInterrupt   = False
  , externalInterrupt   = 0
  }

handleExceptions ::
  CoreState ->
  ExceptionIn ->
  -- Next PC
  (PC,BitVector 2) ->
  State CoreState (Bool,PC)
handleExceptions CoreState{pc,instruction,machineState} exceptionIn (pcN,align) = do
    let ExceptionIn
          { instrAccessFault
          , instrAddrMisaligned
          , instrIllegal
          , dataAccessFault
          , dataAddrMisaligned
          , breakpoint
          , eCall
          , timerInterrupt
          , softwareInterrupt
          , externalInterrupt
          , mret
          } = exceptionIn
    let trap = instrAccessFault || instrAddrMisaligned || instrIllegal ||
               isJust dataAccessFault || isJust dataAddrMisaligned || breakpoint || eCall
    let MachineState{mstatus,mie=Mie{mtie,msie,meie},mepc,mtvec,irqmask} = machineState
        MStatus{mie,mpie} = mstatus

        timerInterrupt1    = timerInterrupt && mtie
        softwareInterrupt1 = softwareInterrupt && msie
        externalInterrupt1 = ((externalInterrupt .&. irqmask) /= 0) && meie
    let interrupt = mie && (timerInterrupt1 || softwareInterrupt1 || externalInterrupt1)
    let DecodedInstruction{opcode} = decodeInstruction instruction

    if trap || interrupt then do
      #machineState .= machineState
                     { mstatus = MStatus { mpie = mie, mie = False }
                     , mcause =
                        if interrupt then
                          if softwareInterrupt1 then
                            MACHINE_SOFTWARE_INTERRUPT
                          else if timerInterrupt1 then
                            MACHINE_TIMER_INTERRUPT
                          else -- externalInterrupt1
                            MACHINE_EXTERNAL_INTERRUPT
                        else
                          if instrAccessFault then
                            INSTRUCTION_ACCESS_FAULT
                          else if instrIllegal then
                            ILLEGAL_INSTRUCTION
                          else if instrAddrMisaligned then
                            INSTRUCTION_ADDRESS_MISALIGNED
                          else if eCall then
                            ENVIRONMENT_CALL
                          else if breakpoint then
                            BREAKPOINT
                          else case dataAddrMisaligned of
                            Just _ -> case opcode of
                              LOAD -> LOAD_ADDRESS_MISALIGNED
                              _ -> STORE_ADDRESS_MISALIGNED
                            _ -> case opcode of -- dataAccessFault
                              LOAD -> LOAD_ADDRESS_MISALIGNED
                              _ -> STORE_ADDRESS_MISALIGNED
                     , mepc = pc ++# 0
                     , mtval =
                        if instrAddrMisaligned then
                          pcN ++# align
                        else if instrIllegal then
                          instruction
                        else if instrAccessFault then
                          pc ++# 0
                        else if breakpoint then
                          pc ++# 0
                        else case dataAddrMisaligned of
                          Just addr -> addr
                          _ -> case dataAccessFault of
                            Just addr -> addr
                            _ -> 0
                     }
      let pcN1 = zeroExtend (slice d29 d2 (trapBase mtvec))
      return (True,pcN1)
    else if mret then do
      #machineState .= machineState { mstatus = mstatus {mie = mpie} }
      return (False,mepc)
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
  (WishBoneM2S 4 30, Maybe MachineWord, Maybe MachineWord, Maybe MachineWord, Bool)
loadStoreUnit instruction addr store dBusS2M = case opcode of
  LOAD -> let
    lextend ::
      (KnownNat n, n <= 32) =>
      Sign ->
      BitVector (32 - n) ->
      MachineWord
    lextend Unsigned = zeroExtend
    lextend Signed = signExtend

    loadData = case lsw of
                 Byte s -> lextend s (
                         slice d7 d0 (
                         readData dBusS2M `shiftR` shiftAmount))
                 Half s -> lextend s (
                         slice d15 d0 (
                         readData dBusS2M `shiftR` shiftAmount))
                 _ -> readData dBusS2M
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
    , if err dBusS2M then Just addr else Nothing
    , if unaligned then Just addr else Nothing
    , lsFinished
    )
  STORE -> let
    storeData = case lsw of
                  Byte _ -> store `shiftL` shiftAmount
                  Half _ -> store `shiftL` shiftAmount
                  _ -> store
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
    , if err dBusS2M then Just addr else Nothing
    , if unaligned then Just addr else Nothing
    , lsFinished
    )
  _ ->
    ( defM2S
    , undefined
    , Nothing
    , Nothing
    , True
    )
 where
  DecodedInstruction {opcode,func3} = decodeInstruction instruction

  lsFinished = unaligned || err dBusS2M || acknowledge dBusS2M

  lsw = unpack func3

  alignment = slice d1 d0 addr

  unaligned = case lsw of
    Word   -> alignment /= 0
    Half _ -> testBit alignment 0
    _ -> False

  mask = case lsw of
    Byte _ -> case alignment of
              3 -> 0b1000
              2 -> 0b0100
              1 -> 0b0010
              _ -> 0b0001
    Half _ -> case alignment of
              2 -> 0b1100
              _ -> 0b0011
    _ -> 0b1111

  shiftAmount = case lsw of
    Byte _ -> case alignment of
              3 -> 24
              2 -> 16
              1 -> 8
              _ -> 0
    Half _ -> case alignment of
              2 -> 16
              _ -> 0
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
  MachineWord ->
  MachineState ->
  Bool ->
  Bool ->
  MachineWord ->
  State CoreState (Maybe MachineWord, MachineWord)
csrUnit instruction rs1Val machineState softwareInterrupt timerInterrupt externalInterrupt
  | SYSTEM <- opcode
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
          ,irqmask
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
      MIP -> do
        let oldValue = bitB (externalInterrupt /= 0) 11 .|.
                       bitB timerInterrupt 7 .|.
                       bitB softwareInterrupt 3
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
      IRQMASK -> do
        let oldValue = irqmask
            newValue = csrWrite csrType oldValue writeValue1
        #irqmask .= newValue
        return (Just oldValue, newValue)
      IRQPENDING -> do
        let newValue = csrWrite csrType externalInterrupt writeValue1
        return (Just externalInterrupt, newValue)
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
  , legal  :: Bool
  }

decodeInstruction ::
  MachineWord ->
  DecodedInstruction
decodeInstruction w
  = DecodedInstruction
  { opcode = opcode
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
  , func3  = func3
  , legal  = case opcode of
      LUI -> True
      AUIPC -> True
      JAL -> True
      JALR -> func3 == 0
      BRANCH -> case unpack func3 of
        BEQ -> True
        BNE -> True
        BLT -> True
        BGE -> True
        BLTU -> True
        BGEU -> True
        _ -> False
      LOAD -> case unpack func3 of
        Byte _ -> True
        Half _ -> True
        Word -> True
        _ -> False
      STORE -> case unpack func3 of
        Byte Unsigned -> True
        Half Unsigned -> True
        Word -> True
        _ -> False
      OP_IMM -> case unpack func3 of
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        _ -> True
      OP -> case unpack func3 of
        ADD -> func7 == 0 ||        -- ADD
               func7 == 0b010_0000  -- SUB
        SLL -> func7 == 0
        SLT -> func7 == 0
        SLTU -> func7 == 0
        XOR -> func7 == 0
        SR -> func7 == 0 ||        -- SRL
              func7 == 0b010_0000  -- SRA
        OR -> func7 == 0
        AND -> func7 == 0
      MISC_MEM -> func3 == 1
      SYSTEM -> case func3 of
        0 -> func12 == 0 ||  -- ECALL
             func12 == 1 ||  -- EBREAK
             func12 == 0b0011000_00010 -- MRET
        _ -> case unpack (slice d1 d0 func3) of
          ReadWrite -> True
          ReadSet -> True
          ReadClear -> True
          _ -> False
      _ -> False
  }
 where
  opcode = unpack (slice d6 d0 w)
  func3 = slice d14 d12 w
  func7 = slice d31 d25 w
  func12 = slice d31 d20 w


{-# INLINE decodeInstruction #-}

boolToMachineWord :: Bool -> MachineWord
boolToMachineWord = zeroExtend . pack
{-# INLINE boolToMachineWord #-}

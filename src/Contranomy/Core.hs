{-|
Copyright  :  (C) 2020, Christiaan Baaij
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Contranomy.Core where

import Data.Maybe

import Clash.Annotations.BitRepresentation
import Clash.Annotations.BitRepresentation.Deriving
import Clash.Class.AutoReg as AutoReg
import Clash.Prelude hiding (cycle, select)

import Contranomy.Clash.Extra
import Contranomy.Decode
import Contranomy.Encode
import Contranomy.RegisterFile
import Contranomy.RV32IM
import Contranomy.RVFI
import Contranomy.WishBone

data CoreStage
  = InstructionFetch
  | Execute
  deriving (Generic, NFDataX, AutoReg)

data MStatus
  = MStatus
  { mie :: Bool
  , mpie :: Bool
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MStatus

data MCause
  = MCause
  { interrupt :: Bool
  , code :: BitVector 4
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MCause

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

data MachineState
  = MachineState
  { mstatus :: MStatus
  , mcause :: MCause
  , mtvec :: InterruptMode
  , mscratch :: Word32
  , mepc :: BitVector 30
  , mtval :: Word32
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MachineState

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: BitVector 30
  , instruction :: Instr
  , registers :: RegisterFile
  , machineState :: MachineState
  , rvfiOrder :: Unsigned 64
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CoreState

core ::
  HiddenClockResetEnable dom =>
  ( Signal dom (WishBoneS2M 4)
  , Signal dom (WishBoneS2M 4) ) ->
  ( Signal dom (WishBoneM2S 4 30)
  , Signal dom (WishBoneM2S 4 30)
  , Signal dom RVFI
  )
core = mealyAutoB transition cpuStart
 where
  cpuStart
    = CoreState
    { stage = InstructionFetch
    , pc = 0
    , instruction = noop
    , registers = emptyRegisterFile
    , machineState = machineStart
    , rvfiOrder = 0
    }

  machineStart
    = MachineState
    { mstatus = MStatus { mie = False, mpie = False }
    , mcause = MCause { interrupt = False, code = 0 }
    , mtvec = Direct 0
    , mscratch = 0
    , mepc = 0
    , mtval = 0
    }

transition ::
  CoreState ->
  ( WishBoneS2M 4, WishBoneS2M 4 ) ->
  ( CoreState
  , ( WishBoneM2S 4 30
    , WishBoneM2S 4 30
    , RVFI ))
transition s@(CoreState { stage = InstructionFetch, pc }) (iBus,_)
  = ( s { stage = if acknowledge iBus then
                    Execute
                  else
                    InstructionFetch
        , instruction = decodeInstruction (readData iBus)
        }
    , ( (defM2S @4 @30)
               { addr   = pc
               , cycle  = True
               , strobe = True
               }
      , defM2S
      , defRVFI ) )

transition s@(CoreState { stage = Execute, instruction, pc, registers, machineState, rvfiOrder }) (_,dBusS2M)
  =
  let dstReg = case instruction of
        RRInstr (RInstr {dest}) -> Just dest
        RIInstr iinstr -> case iinstr of
          IInstr {dest} -> Just dest
          ShiftInstr {dest} -> Just dest
          LUI {dest} -> Just dest
          AUIPC {dest} -> Just dest
        CSRInstr csrInstr -> case csrInstr of
          CSRRInstr {dest} -> Just dest
          CSRIInstr {dest} -> Just dest
        MemoryInstr minstr -> case minstr of
          LOAD {dest} | not loadTrap && acknowledge dBusS2M -> Just dest
          _ -> Nothing
        JumpInstr jinstr | not pcTrap -> case jinstr of
          JAL {dest} -> Just dest
          JALR {dest} -> Just dest
        _ -> Nothing

      aluResult = case instruction of
        RRInstr (RInstr {opcode,src1,src2}) ->
          let arg1 = readRegisterFile registers src1
              arg2 = readRegisterFile registers src2
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
#if !defined(RISCV_FORMAL_ALTOPS)
            MUL -> arg1 * arg2
            MULH -> slice d63 d32 (signExtend arg1 * signExtend arg2 :: BitVector 64)
            MULHSU -> slice d63 d32 (signExtend arg1 * zeroExtend arg2 :: BitVector 64)
            MULHU -> slice d63 d32 (zeroExtend arg1 * zeroExtend arg2 :: BitVector 64)
            DIV -> if arg2 == 0 then
                     (-1)
                   else if arg1 == pack (minBound :: Signed 32) && arg2 == (-1) then
                     pack (minBound :: Signed 32)
                   else
                     pack ((unpack arg1 :: Signed 32) `quot` unpack arg2)
            DIVU -> if arg2 == 0 then
                      (-1)
                    else
                      arg1 `quot` arg2
            REM -> if arg2 == 0 then
                     arg1
                   else if arg1 == pack (minBound :: Signed 32) && arg2 == (-1) then
                     0
                   else
                     pack ((unpack arg1 :: Signed 32) `rem` unpack arg2)
            REMU -> if arg2 == 0 then
                      arg1
                    else
                      arg1 `rem` arg2
#else
            MUL -> (arg1 + arg2) `xor` 0x2cdf52a55876063e
            MULH -> (arg1 + arg2) `xor` 0x15d01651f6583fb7
            MULHSU -> (arg1 - arg2) `xor` 0xea3969edecfbe137
            MULHU -> (arg1 + arg2) `xor` 0xd13db50d949ce5e8
            DIV -> (arg1 - arg2) `xor` 0x29bbf66f7f8529ec
            DIVU -> (arg1 - arg2) `xor` 0x8c629acb10e8fd70
            REM -> (arg1 - arg2) `xor` 0xf5b7d8538da68fa5
            REMU -> (arg1 - arg2) `xor` 0xbc4402413138d0e1
#endif
        RIInstr iinstr -> case iinstr of
          IInstr {iOpcode,src,imm12} ->
            let arg1 = readRegisterFile registers src
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
            let arg1 = readRegisterFile registers src
                shiftAmount = unpack (resize shamt)
            in case sOpcode of
              SLLI -> shiftL arg1 shiftAmount
              SRLI -> shiftR arg1 shiftAmount
              SRAI -> pack (shiftR (unpack arg1 :: Signed 32) shiftAmount)
          LUI {imm20} ->
            imm20 ++# 0
          AUIPC {imm20} ->
            (pc ++# 0) + (imm20 ++# 0)
        CSRInstr csrInstr -> case csr csrInstr of
          MSTATUS -> case mstatus machineState of
            MStatus {mpie, mie} ->
              shiftL (boolToBitVector mpie) 7 .|.
              shiftL (boolToBitVector mie) 3
          MTVEC -> pack (mtvec machineState)
          MSCRATCH -> mscratch machineState
          MEPC -> resize (mepc machineState) `shiftL` 2
          MCAUSE -> case mcause machineState of
            MCause {interrupt, code} ->
              pack interrupt ++# 0 ++# code
          MTVAL -> mtval machineState
          _ -> 0

        MemoryInstr minstr -> case minstr of
          LOAD {loadWidth} ->
            let alignedRData = readData dBusS2M `shiftR` (fromEnum loadAlign * 8)
            in  case loadWidth of
                  Width Byte -> signExtend (slice d7 d0 alignedRData)
                  Width Half -> signExtend (slice d15 d0 alignedRData)
                  HalfUnsigned -> zeroExtend (slice d15 d0 alignedRData)
                  ByteUnsigned -> zeroExtend (slice d7 d0 alignedRData)
                  _ -> alignedRData
          _ -> 0
        JumpInstr {} -> (pc + 1) ++# 0
        _ -> 0

      (pcN0,pcTrap) = case instruction of
        BranchInstr (Branch {imm,cond,src1,src2}) ->
          let arg1 = readRegisterFile registers src1
              arg2 = readRegisterFile registers src2
              taken = case cond of
                BEQ -> arg1 == arg2
                BNE -> arg1 /= arg2
                BLT -> (unpack arg1 :: Signed 32) < unpack arg2
                BLTU -> arg1 < arg2
                BGE -> (unpack arg1 :: Signed 32) >= unpack arg2
                BGEU -> arg1 >= arg2
          in if taken then
               let (offset,align) = split (signExtend imm `shiftL` 1 :: Word32)
               in  (pc + offset,align /= 0)
             else
               (pc + 1,False)
        JumpInstr jinstr -> case jinstr of
          JAL {imm} ->
            let (pcOffset,align) = split (signExtend imm `shiftL` 1 :: Word32)
            in  (pc + pcOffset,align /=0)
          JALR {offset,base} ->
            let arg1 = readRegisterFile registers base
                (pcN,align) = split (arg1 + signExtend offset)
            in  (pcN,testBit align 1)
        MemoryInstr {} | not (acknowledge dBusS2M) ->
          (pc,False)
        EnvironmentInstr einstr -> case einstr of
          ECALL  -> (pc,True)
          EBREAK -> (pc,True)
          MRET   -> (mepc machineState,False)
        _ -> (pc+1,False)

      (loadAddres,loadMask,loadAlign,loadTrap) = case instruction of
        MemoryInstr (LOAD {offset,base,loadWidth}) ->
          let addr = readRegisterFile registers base + signExtend offset
              align = slice d1 d0 addr
              mask = loadWidthSelect loadWidth
              trapped = case mask of
                          0b1111 -> align /= 0
                          0b0011 -> testBit align 0
                          _      -> False
          in  (addr,mask `shiftL` fromEnum align,align,trapped)
        _ -> (0,0,0,False)

      (storeAddress,storeMask,storeWData,storeTrap) = case instruction of
        MemoryInstr (STORE {base,offset,width,src}) ->
          let addr = readRegisterFile registers base + signExtend offset
              align = slice d1 d0 addr
              mask = loadWidthSelect (Width width)
              wdata = readRegisterFile registers src
              trapped = case width of
                          Word -> align /= 0
                          Half -> testBit align 0
                          Byte -> False
          in  ( addr
              , mask `shiftL` fromEnum align
              , wdata `shiftL` (fromEnum align * 8)
              , trapped )
        _ -> (0,0,0,False)

      trap = pcTrap || loadTrap || storeTrap

      pcN1
        | trap
        = unpack (zeroExtend (trapBase (mtvec machineState)))
        | otherwise
        = pcN0

      machineStateN = case instruction of
        EnvironmentInstr einstr -> case einstr of
          ECALL ->
            machineState
              { mtval = 0
              , mstatus = MStatus { mpie = mie (mstatus machineState)
                                  , mie  = False
                                  }
              , mepc = resize (pack pc) `shiftL` 2
              , mcause = MCause { interrupt = False, code = 11 }
              }
          EBREAK ->
            machineState
              { mtval = 0
              , mstatus = MStatus { mpie = mie (mstatus machineState)
                                  , mie  = False
                                  }
              , mepc = resize (pack pc) `shiftL` 2
              , mcause = MCause { interrupt = False, code = 3 }
              }
          MRET ->
            machineState
              { mstatus = MStatus { mpie = True
                                  , mie  = mpie (mstatus (machineState))
                                  }
              }
        CSRInstr csrInstr ->
          let writeValue = case csrInstr of
                CSRRInstr {src=X0} -> Nothing
                CSRRInstr {src} -> Just (readRegisterFile registers src)
                CSRIInstr {imm=0} -> Nothing
                CSRIInstr {imm} -> Just (zeroExtend imm)

              oldValue = case csr csrInstr of
                MIE -> undefined
                MTVEC -> pack (mtvec machineState)
                MSCRATCH -> mscratch machineState
                MEPC -> resize (mepc machineState) `shiftL` 2
                MCAUSE -> case mcause machineState of
                  MCause {interrupt, code} ->
                    pack interrupt ++# 0 ++# code
                MTVAL -> mtval machineState
                _ -> 0

              newValue = csrWrite (csrType csrInstr) oldValue writeValue
          in  case csr csrInstr of
                MSTATUS ->
                  machineState
                    { mstatus = MStatus { mpie = testBit newValue 7
                                        , mie  = testBit newValue 3
                                        }
                    }
                MTVEC ->
                  machineState
                    { mtvec = case unpack (clearBit newValue 1) of
                        Direct base | slice d1 d0 base /= 0 -> mtvec machineState
                        Vectored base | slice d1 d0 base /= 0 -> mtvec machineState
                        val -> val
                    }
                MSCRATCH ->
                  machineState
                    { mscratch = newValue
                    }
                MEPC ->
                  machineState
                    { mepc = resize (newValue `shiftR` 2)
                    }
                MCAUSE ->
                  machineState
                    { mcause = MCause { interrupt = testBit newValue 31
                                      , code      = truncateB newValue
                                      }
                    }
                MTVAL ->
                  machineState
                    { mtval = newValue
                    }
                _ ->
                  machineState
        _ ->
          if trap then
            machineState
              { mtval =
                  if loadTrap then
                    loadAddres
                  else if storeTrap then
                    storeAddress
                  else
                    pcN0 ++# 0

              , mstatus = MStatus { mpie = mie (mstatus machineState)
                                  , mie  = False
                                  }
              , mepc = pc
              , mcause = MCause { interrupt = False
                                , code =
                                    if loadTrap then
                                      4
                                    else if storeTrap then
                                      6
                                    else
                                      0
                                }
              }
          else
            machineState

      dBusM2S = case instruction of
        MemoryInstr minstr | not trap -> case minstr of
          LOAD {} ->
            (defM2S @4 @30)
              { addr   = slice d31 d2 loadAddres
              , select = loadMask
              , cycle  = True
              , strobe = True
              }
          STORE {} ->
            (defM2S @4 @30)
              { addr        = slice d31 d2 storeAddress
              , writeData   = storeWData
              , select      = storeMask
              , cycle       = True
              , strobe      = True
              , writeEnable = True
              }
        _ -> defM2S
  in
    ( s { stage = case instruction of
            MemoryInstr {}
              | not (trap || acknowledge dBusS2M)
              -> Execute
            _ -> InstructionFetch
        , pc = pcN1
        , registers = writeRegisterFile registers ((,) <$> dstReg <*> pure aluResult)
        , machineState = machineStateN
        , rvfiOrder = case instruction of
            MemoryInstr {}
              | not (trap || acknowledge dBusS2M)
              -> rvfiOrder
            _ -> rvfiOrder + 1
        }
    , ( defM2S
      , dBusM2S
      , toRVFI instruction
               rvfiOrder
               registers
               dstReg
               aluResult
               pc
               pcN1
               trap
               dBusM2S
               dBusS2M
      ) )

loadWidthSelect :: LoadWidth -> BitVector 4
loadWidthSelect lw = case lw of
  Width Byte -> 0b0001
  Width Half -> 0b0011
  Width Word -> 0b1111
  HalfUnsigned -> 0b0011
  ByteUnsigned -> 0b0001
{-# INLINE loadWidthSelect #-}

toRVFI ::
  -- | Current decoded instruction
  Instr ->
  -- | Order
  Unsigned 64 ->
  -- | Registers
  RegisterFile ->
  -- | Destination register
  Maybe Register ->
  -- | Destination register write data
  BitVector 32 ->
  -- | Current PC
  BitVector 30 ->
  -- | Next PC
  BitVector 30 ->
  -- | Trap
  Bool ->
  -- | Data Bus M2S
  WishBoneM2S 4 30 ->
  -- | Data Bus S2M
  WishBoneS2M 4 ->
  RVFI
toRVFI instruction rvfiOrder registers dstReg aluResult pc pcN trap dBusM2S dBusS2M =
  let rs1AddrN = case instruction of
                   BranchInstr (Branch {src1}) -> src1
                   CSRInstr (CSRRInstr {src}) -> src
                   JumpInstr (JALR {base}) -> base
                   MemoryInstr (LOAD {base}) -> base
                   MemoryInstr (STORE {base}) -> base
                   RRInstr (RInstr {src1}) -> src1
                   RIInstr (IInstr {src}) -> src
                   RIInstr (ShiftInstr {src}) -> src
                   _ -> X0

      rs2AddrN = case instruction of
                   BranchInstr (Branch {src2}) -> src2
                   MemoryInstr (STORE {src}) -> src
                   RRInstr (RInstr {src2}) -> src2
                   _ -> X0
  in  defRVFI
        { valid = case instruction of
            MemoryInstr {} | not trap -> acknowledge dBusS2M
            _ -> True
        , order    = rvfiOrder
        , insn     = encodeInstruction instruction
        , trap     = trap
        , rs1Addr  = rs1AddrN
        , rs2Addr  = rs2AddrN
        , rs1RData = readRegisterFile registers rs1AddrN
        , rs2RData = readRegisterFile registers rs2AddrN
        , rdAddr   = fromMaybe X0 dstReg
          -- Since we take the tail for the written to registers, this is okay
        , rdWData  = case fromMaybe X0 dstReg of
                       X0 -> 0
                       _ -> aluResult
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
        }

csrWrite ::
  CSRType ->
  Word32 ->
  Maybe Word32 ->
  Word32
csrWrite ReadWrite oldValue Nothing = oldValue
csrWrite ReadWrite _ (Just newValue) = newValue

csrWrite ReadSet oldValue Nothing = oldValue
csrWrite ReadSet oldValue (Just newValue) = oldValue .|. newValue

csrWrite ReadClear oldValue Nothing = oldValue
csrWrite ReadClear oldValue (Just newValue) = oldValue .&. newValue

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
  = Direct (BitVector 30)
  | Vectored (BitVector 30)
  deriving (Generic, NFDataX, AutoReg)

{-# ANN module (DataReprAnn
                  $(liftQ [t|InterruptMode|])
                  32
                  [ ConstrRepr 'Direct   (1 `downto` 0) 0 [31 `downto` 2]
                  , ConstrRepr 'Vectored (1 `downto` 0) 1 [31 `downto` 2]
                  ]) #-}
deriveBitPack [t| InterruptMode |]

interruptAddress ::
  InterruptMode ->
  BitVector 4 ->
  Unsigned 32
interruptAddress (Direct base) _ = unpack (resize base)
interruptAddress (Vectored base) cause = unpack (resize base + resize cause * 4)

data MachineState
  = MachineState
  { mstatus :: MStatus
  , mcause :: MCause
  , mtvec :: InterruptMode
  , mscratch :: Word32
  , mepc :: Word32
  , mtval :: Word32
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''MachineState

data CoreState
  = CoreState
  { stage :: CoreStage
  , pc :: Unsigned 32
  , instruction :: Instr
  , registers :: RegisterFile
  , machineState :: MachineState
  }
  deriving (Generic, NFDataX)

deriveAutoReg ''CoreState

core ::
  HiddenClockResetEnable dom =>
  ( Signal dom (WishBoneS2M 4)
  , Signal dom (WishBoneS2M 4) ) ->
  ( Signal dom (WishBoneM2S 4 30)
  , Signal dom (WishBoneM2S 4 32)
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
    , WishBoneM2S 4 32
    , RVFI ))
transition s@(CoreState { stage = InstructionFetch, pc }) (iBus,_)
  = ( s { stage = if acknowledge iBus then
                    Execute
                  else
                    InstructionFetch
        , instruction = decodeInstruction (readData iBus)
        }
    , ( (defM2S @4 @30)
               { addr   = slice d31 d2 pc
               , cycle  = True
               , strobe = True
               }
      , defM2S
      , defRVFI ) )

transition s@(CoreState { stage = Execute, instruction, pc, registers, machineState }) (_,dBusS2M)
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
          LOAD {dest} | acknowledge dBusS2M -> Just dest
          _ -> Nothing
        JumpInstr jinstr -> case jinstr of
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
            pack pc + (imm20 ++# 0)
        CSRInstr csrInstr -> case csr csrInstr of
          MSTATUS -> case mstatus machineState of
            MStatus {mpie, mie} ->
              shiftL (boolToBitVector mpie) 7 .|.
              shiftL (boolToBitVector mie) 3
          MTVEC -> pack (mtvec machineState)
          MSCRATCH -> mscratch machineState
          MEPC -> mepc machineState
          MCAUSE -> case mcause machineState of
            MCause {interrupt, code} ->
              pack interrupt ++# 0 ++# code
          MTVAL -> mtval machineState
          _ -> 0

        MemoryInstr minstr -> case minstr of
          LOAD {loadWidth} -> case loadWidth of
            Width Byte -> signExtend (slice d7 d0 (readData dBusS2M))
            Width Half -> signExtend (slice d15 d0 (readData dBusS2M))
            HalfUnsigned -> zeroExtend (slice d15 d0 (readData dBusS2M))
            ByteUnsigned -> zeroExtend (slice d7 d0 (readData dBusS2M))
            _ -> readData dBusS2M
          _ -> 0
        JumpInstr {} -> pack (pc + 4)
        _ -> 0

      pcN0 = case instruction of
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
               pc + unpack (signExtend imm `shiftL` 1)
             else
               pc + 4
        JumpInstr jinstr -> case jinstr of
          JAL {imm} ->
            pc + unpack (signExtend imm `shiftL` 1)
          JALR {offset,base} ->
            let arg1 = readRegisterFile registers base
            in  unpack (slice d31 d1 (arg1 + signExtend offset) ++# 0)
        MemoryInstr {} | not (acknowledge dBusS2M) -> pc
        EnvironmentInstr einstr -> case einstr of
          ECALL  -> interruptAddress (mtvec machineState) 11
          EBREAK -> interruptAddress (mtvec machineState) 3
        _ -> pc+4

      addressMisaligned = slice d1 d0 pcN0 /= 0
      pcN1 =
        if addressMisaligned then
          interruptAddress (mtvec machineState) 0
        else
          pcN0

      machineStateN = case instruction of
        CSRInstr csrInstr ->
          let writeValue = case csrInstr of
                CSRRInstr {src} | src /= X0 -> Just (readRegisterFile registers src)
                CSRIInstr {imm} | imm /= 0 -> Just (zeroExtend imm)
                _ -> Nothing

              oldValue = case csr csrInstr of
                MIE -> undefined
                MTVEC -> pack (mtvec machineState)
                MSCRATCH -> mscratch machineState
                MEPC -> mepc machineState
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
                    { mtvec = unpack (clearBit newValue 1)
                    }
                MSCRATCH ->
                  machineState
                    { mscratch = newValue
                    }
                MEPC ->
                  machineState
                    { mepc = newValue
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
          if addressMisaligned then
            machineState
              { mtval = pack pcN0
              , mstatus = MStatus { mpie = mie (mstatus machineState)
                                  , mie  = False
                                  }
              , mepc = pack pc
              , mcause = MCause { interrupt = False, code = 0 }
              }
          else
            machineState

      dBusM2S = case instruction of
        MemoryInstr minstr -> case minstr of
          LOAD {loadWidth,offset,base} ->
            (defM2S @4 @32)
              { addr   = readRegisterFile registers base + signExtend offset
              , select = loadWidthSelect loadWidth
              , cycle  = True
              , strobe = True
              }
          STORE {width,offset,src,base} ->
            (defM2S @4 @32)
              { addr = readRegisterFile registers base + signExtend offset
              , writeData = readRegisterFile registers src
              , select = loadWidthSelect (Width width)
              , cycle = True
              , strobe = True
              , writeEnable = True
              }
        _ -> defM2S
  in
    ( s { stage = case instruction of
            MemoryInstr {}
              | not (acknowledge dBusS2M)
              -> Execute
            _ -> InstructionFetch
        , pc = pcN1
        , registers = writeRegisterFile registers ((,) <$> dstReg <*> pure aluResult)
        , machineState = machineStateN
        }
    , ( defM2S
      , dBusM2S
      , toRVFI instruction
               registers
               dstReg
               aluResult
               pc
               pcN1
               addressMisaligned
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
  -- | Registers
  RegisterFile ->
  -- | Destination register
  Maybe Register ->
  -- | Destination register write data
  BitVector 32 ->
  -- | Current PC
  Unsigned 32 ->
  -- | Next PC
  Unsigned 32 ->
  -- | Trap
  Bool ->
  -- | Data Bus M2S
  WishBoneM2S 4 32 ->
  -- | Data Bus S2M
  WishBoneS2M 4 ->
  RVFI
toRVFI instruction registers dstReg aluResult pc pcN branchTrap dBusM2S dBusS2M =
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
            MemoryInstr {} -> acknowledge dBusS2M
            _ -> True
        , order    = 0
        , insn     = encodeInstruction instruction
        , trap     = branchTrap
        , rs1Addr  = rs1AddrN
        , rs2Addr  = rs2AddrN
        , rs1RData = readRegisterFile registers rs1AddrN
        , rs2RData = readRegisterFile registers rs2AddrN
        , rdAddr   = fromMaybe X0 dstReg
          -- Since we take the tail for the written to registers, this is okay
        , rdWData  = case fromMaybe X0 dstReg of
                       X0 -> 0
                       _ -> aluResult
        , pcRData  = pc
        , pcWData  = pcN
        , memAddr  = addr dBusM2S
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
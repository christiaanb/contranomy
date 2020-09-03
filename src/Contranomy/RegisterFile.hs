module Contranomy.RegisterFile where

import Clash.Class.AutoReg (AutoReg)
import Clash.Prelude

import Contranomy.Instruction

newtype RegisterFile = RegisterFile (Vec 31 MachineWord)
  deriving (Generic, NFDataX)

instance AutoReg RegisterFile

emptyRegisterFile :: RegisterFile
emptyRegisterFile = RegisterFile (repeat 0)

writeRegisterFile ::
  RegisterFile ->
  Maybe (Register, MachineWord) ->
  RegisterFile
writeRegisterFile (RegisterFile registers) (Just (r,w)) =
  RegisterFile (tail (replace r w (0 :> registers)))

writeRegisterFile registers _ = registers

readRegisterFile ::
  RegisterFile ->
  Register ->
  Register ->
  (MachineWord,MachineWord)
readRegisterFile (RegisterFile registers) r1 r2 =
  let registers0 = (0 :> registers)
  in  (registers0 !! r1, registers0 !! r2)

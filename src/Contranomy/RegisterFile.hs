module Contranomy.RegisterFile where

import Clash.Class.AutoReg (AutoReg)
import Clash.Prelude

import Contranomy.RV32IM

newtype RegisterFile = RegisterFile (Vec 31 Word32)
  deriving (Generic, NFDataX)

instance AutoReg RegisterFile

emptyRegisterFile :: RegisterFile
emptyRegisterFile = RegisterFile (repeat 0)

writeRegisterFile ::
  RegisterFile ->
  Maybe (Register, Word32) ->
  RegisterFile
writeRegisterFile (RegisterFile registers) (Just (r,w)) =
  RegisterFile (tail (replace r w (0 :> registers)))

writeRegisterFile registers _ = registers

readRegisterFile ::
  RegisterFile ->
  Register ->
  Word32
readRegisterFile (RegisterFile registers) r = (0 :> registers) !! r

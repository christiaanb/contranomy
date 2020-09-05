module Contranomy.RVFI where

import Clash.Prelude

import Contranomy.Instruction

data RVFI
  = RVFI
  { valid    :: "rvfi_valid"     ::: Bool
  , order    :: "rvfi_order"     ::: Unsigned 64
  , insn     :: "rvfi_insn"      ::: BitVector 32
  , trap     :: "rvfi_trap"      ::: Bool
  , halt     :: "rvfi_halt"      ::: Bool
  , intr     :: "rvfi_intr"      ::: Bool
  , mode     :: "rvfi_mode"      ::: BitVector 2
  , ixl      :: "rvfi_ixl"       ::: BitVector 2
  , rs1Addr  :: "rvfi_rs1_addr"  ::: Register
  , rs2Addr  :: "rvfi_rs2_addr"  ::: Register
  , rs1RData :: "rvfi_rs1_rdata" ::: BitVector 32
  , rs2RData :: "rvfi_rs2_rdata" ::: BitVector 32
  , rdAddr   :: "rvfi_rd_addr"   ::: Register
  , rdWData  :: "rvfi_rd_wdata"  ::: BitVector 32
  , pcRData  :: "rvfi_pc_rdata"  ::: BitVector 32
  , pcWData  :: "rvfi_pc_wdata"  ::: BitVector 32
  , memAddr  :: "rvfi_mem_addr"  ::: BitVector 32
  , memRMask :: "rvfi_mem_rmask" ::: BitVector 4
  , memWMask :: "rvfi_mem_wmask" ::: BitVector 4
  , memRData :: "rvfi_mem_rdata" ::: BitVector 32
  , memWData :: "rvfi_mem_wdata" ::: BitVector 32
  }
  deriving Show

defRVFI :: RVFI
defRVFI
  = RVFI
  { valid    = False
  , order    = 0
  , insn     = 0
  , trap     = False
  , halt     = False
  , intr     = False
  , mode     = 3
  , ixl      = 1
  , rs1Addr  = X0
  , rs2Addr  = X0
  , rs1RData = 0
  , rs2RData = 0
  , rdAddr   = X0
  , rdWData  = 0
  , pcRData  = 0
  , pcWData  = 0
  , memAddr  = 0
  , memRMask = 0
  , memWMask = 0
  , memRData = 0
  , memWData = 0
  }

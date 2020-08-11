module Contranomy.RVFI where

import Clash.Prelude

import Contranomy.RV32IM

data RVFI
  = RVFI
  { valid    :: "rvfi_valid"     ::: Bool
  , order    :: "rvfi_order"     ::: BitVector 64
  , insn     :: "rvfi_insn"      ::: BitVector 32
  , trap     :: "rvfi_trap"      ::: Bool
  , halt     :: "rvfi_halt"      ::: Bool
  , intr     :: "rvfi_intr"      ::: Bool
  , rs1Addr  :: "rvfi_rs1_addr"  ::: Register
  , rs2Addr  :: "rvfi_rs2_addr"  ::: Register
  , rs1RData :: "rvfi_rs1_rdata" ::: BitVector 32
  , rs2RData :: "rvfi_rs2_rdata" ::: BitVector 32
  , rdAddr   :: "rvfi_rd_addr"   ::: Register
  , rdWData  :: "rvfi_rd_wdata"  ::: BitVector 32
  , pcRData  :: "rvfi_pc_rdata"  ::: Unsigned 32
  , pcWData  :: "rvfi_pc_wdata"  ::: Unsigned 32
  , memAddr  :: "rvfi_mem_addr"  ::: BitVector 32
  , memRMask :: "rvfi_mem_rmask" ::: BitVector 4
  , memWMask :: "rvfi_mem_wmask" ::: BitVector 4
  , memRData :: "rvfi_mem_rdata" ::: BitVector 32
  , memWData :: "rvfi_mem_wdata" ::: BitVector 32
  }

defRVFI :: RVFI
defRVFI
  = RVFI
  { valid    = False
  , order    = 0
  , insn     = 0
  , trap     = False
  , halt     = False
  , intr     = False
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

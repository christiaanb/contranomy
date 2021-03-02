## Introduction
This repository hosts a *non*-pipelined RV32I RISC-V core written in Clash.
Some relevant details:

* Support for the full unprivileged RV32I Base Integer Instruction set [1]:
  * `FENCE` is treated as a NOP
* Support for the following privileged instructions [1]:
  * `ECALL`
  * `EBREAK`
  * `MRET`
* Zifencei extension `FENCE.I` is treated as a NOP
* Support for the Zicsr CSR instructions
* Implements the following machine-level CSRs as specified in the privileged architecture[1]:
  * `mstatus`
  * `mcause`
  * `mtvec`
  * `mie`
  * `mscratch`
  * `mepc`
  * `mtval`
* Supports timer, software, and 32 external interrupts:
  * External IRQ mask CSR has address `0x330`
  * External IRQ pending CSR has address `0x360`
* According to spec: the core traps on all unsupported instructions with exception code 2, illegal instruction, loaded into the `mcause` CSR.
* Full trap support in accordance to the privileged architecture[1] specification.
* Fetches instructions and data over a Wishbone Classic bus: minimum of 2-cycle round-trip time for both busses.

[1] https://riscv.org/technical/specifications/

## Workshop video
https://www.youtube.com/watch?v=NFguFKbuB_c is a 3 hour workshop video
describing the core in light detail and demonstrating the effects of the
instructions given below. It also shows the core running on an actual FPGA.

## Initial setup

Create a directory `contranomy-tutorial`

```
mkdir contranomy-tutorial
```

Change directory into `contranomy-tutorial` and clone the following repositories:

```
cd contranomy-tutorial
git clone https://github.com/christiaanb/contranomy.git
git clone -b contranomy https://github.com/christiaanb/litex
git clone https://github.com/christiaanb/pythondata-cpu-contranomy.git
git clone -b contranomy https://github.com/christiaanb/riscv-formal.git
```

Create a python virtual env and enter/activate it:

```
python3 -m virtualenv .
source bin/activate
```

Install the `pythondata-cpu-contranomy` package into the virtual environment

```
cd pythondata-cpu-contranomy
python3 setup.py develop
```

Change directory into `litex` and run the following commands

```
cd ..
cd litex
./litex_setup.py init
./litex_setup.py install
./litex_setup.py gcc
```

Go back up to the `contranomy-tutorial` directory and add the installed RISC-V toolchain to your PATH

```
cd ..
PATH=$PATH:$(echo $PWD/riscv64-*/bin/)
```

And check that everything works by running:

```
lxsim --cpu-type contranomy
```

You can exit the simulator by pressing `Ctrl+C` and the close the virtual environment with

```
deactivate
```

## Coming back

Whenever you're coming back you simply need to source the python virtual env:

```
source bin/activate
```

and add the RISC-V toolchain to your PATH:

```
PATH=$PATH:$(echo $PWD/riscv64-*/bin/)
```

## Running the RISC-V Formal Verification Framework on the Contranomy core

First, [install Yosys, SymbiYosys, and Boolector](http://symbiyosys.readthedocs.io/en/latest/quickstart.html#installing) and make sure those tools are in your PATH.

Then go to `contranomy` and generate the Verilog:

```
cd contranomy
stack run clash -- --verilog Contranomy
```

Next, go to the `contranomy` cores directory in `riscv-formal`:

```
cd ..
cd riscv-formal/cores/contranomy
```

And copy the relevant `.v` and `.inc` file

```
cp ../../../contranomy/verilog/Contranomy/contranomyRVFI/contranomyRVFI.v ./
cp ../../../contranomy/verilog/Contranomy/contranomyRVFI/ContranomyCoreMachineStateDirect.inc ./
```

Then create all the tests:

```
python3 ../../checks/genchecks.py
```

Run the tests:

```
make -C checks -j$(nproc)
```

Once the tests are finished, collect the results:

```
bash cexdata.sh
```

and inspect the results:

```
cat cexdata/status.txt
```

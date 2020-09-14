## Initial setup

Create a directory `contranomy-tutorial`

```
mkdir contranomy-tutorial
```

Change directory into `contranomy-tutorial` and clone the following repositories:

```
cd contranomy-tutorial
git clone -b tutorial https://github.com/christiaanb/contranomy.git
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

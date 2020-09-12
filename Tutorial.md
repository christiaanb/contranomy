Create a directory `contranomy-tutorial`

```
mkdir contranomy-tutorial
```

Change directory into it and clone the following repositories:

```
cd contranomy-tutorial
git clone -b tutorial https://github.com/christiaanb/contranomy.git
git clone -b contranomy https://github.com/christiaanb/litex
git clone -b contranomy https://github.com/christiaanb/riscv-formal.git
```

Change directory into `litex` and run the following commands

```
cd litex
python3 -m virtualenv .
source bin/activate
./litex_setup.py init
./litex_setup.py install
./litex_setup.py gcc
```

Go up one directory, and clone and install the following:

```
cd ..
git clone https://github.com/christiaanb/pythondata-cpu-contranomy.git
cd pythondata-cpu-contranomy
python3 setup.py develop
```

Go up one directory and add the installed RISC-V toolchain to you PATH

```
cd ..
PATH=$PATH:$(echo $PWD/riscv64-*/bin/)
```

And check that everything works by running:

```
lxsim
```

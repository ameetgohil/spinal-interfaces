spinal-interfaces
============
Contains interfaces such as jtag, apb, and other written in SpinalHDL

Just one important note, you need a java JDK >= 8

On debian : 

```sh
sudo add-apt-repository -y ppa:openjdk-r/ppa
sudo apt-get update
sudo apt-get install openjdk-8-jdk -y

#To set the default java
sudo update-alternatives --config java
sudo update-alternatives --config javac
```

## Dependencies

You need to install SBT

```sh
echo "deb https://dl.bintray.com/sbt/debian /" | sudo tee -a /etc/apt/sources.list.d/sbt.list
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
sudo apt-get update
sudo apt-get install sbt
```

If you want to run the scala written testbench, you have to be on linux and have Verilator installed (a recent version) :

```sh
sudo apt-get install git make autoconf g++ flex bison -y  # First time prerequisites
git clone http://git.veripool.org/git/verilator   # Only first time
unsetenv VERILATOR_ROOT  # For csh; ignore error if on bash
unset VERILATOR_ROOT  # For bash
cd verilator
git pull        # Make sure we're up-to-date
git checkout verilator_3_916
autoconf        # Create ./configure script
./configure
make -j$(nproc)
sudo make install
cd ..
echo "DONE"

```

Open a terminal in the root of it and run "sbt run". At the first execution, the process could take some seconds

```sh
cd SpinalTemplateSbt

//If you want to generate the Verilog of your design
sbt "runMain jtag.SimpleJtagTapVerilog"

```



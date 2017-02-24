# Installation of the CLAW Fortran compiler

### Requirements

The CLAW Fortran compiler has the followings dependencies:
* Java 1.7 or greater
* Ant 1.7.1 or greater
* yacc, lex (For OMNI Compiler)
* C/C++ compiler (supports C99) (For OMNI Compiler)
* Fortran compiler (supports Fortran 90) (For OMNI Compiler)
* MPI-2 or greater (For OMNI Compiler)
* libxml2 (For OMNI Compiler)
* cmake and make



### Build & install

CLAW Fortran compiler (clawfc) is built on the top of the
[OMNI Compiler](http://www,omni-compiler.org). It is currently tested with
version [1.0.1](http://omni-compiler.org/download/stable/omnicompiler-1.0.1.tar.bz2).
OMNI Compiler is packaged and built together with the CLAW Compiler.


To build the and install the CLAW Fortran Compiler, use the followings commands.

In source build:
```bash
git clone git@github.com:C2SM-RCM/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
cmake -DCMAKE_INSTALL_PREFIX=<install_path> .
make
make install
```

Out-of-source build:
```bash
git clone git@github.com:C2SM-RCM/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=<install_path> ..
make
make install
```

##### Specific steps for Piz Daint
On Piz Daint, specific steps as to be performed in order to have a successful
compilation.

First of all, Ant is not available on Piz Daint. To install it, follow the
instruction [here](./INSTALL_Ant.md).

On Piz Daint, the Cray MPI wrapper must be used regardless of the selected
programming environment. So if you are compiling with PGI or GNU, use the
following commands:

```bash
git clone git@github.com:C2SM-RCM/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
mkdir build
cd build
FC=ftn CC=cc CXX=CC cmake -DCMAKE_INSTALL_PREFIX=<install_path> -DOMNI_MPI_FC="MPI_FC=ftn" -DOMNI_MPI_CC="MPI_CC=cc" ..
make
make install
```

It will use the PGI compiler or the GNU compiler going through the Cray MPI
wrapper.


#### Test your installation with an example
##### Source code
File: `simple_sample.f90`
```Fortran
PROGRAM simple_sample
  CALL my_simple_subroutine
END PROGRAM simple_sample

SUBROUTINE my_simple_subroutine
  INTEGER :: i
  !$claw loop-fusion
  DO i=1,2
    PRINT *, 'First loop body:',i
  END DO

  !$claw loop-fusion
  DO i=1,2
    PRINT *, 'Second loop body:',i
  END DO

  !$claw loop-fusion
  DO i=1,2
    PRINT *, 'Third loop body:',i
  END DO
END
```

##### Compilation
Compile the original source code to compare the output
```bash
gfortran -o simple_sample1 simple_sample1.f90
```

Apply code transformation and compile the transformed source file
```bash
clawfc -o transformed_code.f90 simple_sample.f90  # Generate transformed_code
gfortran -o simple_sample2 transformed_code.f90   # Compile with std compiler
```

##### simple_sample1's output:
```bash
$ ./simple_sample1
First loop body:           1
First loop body:           2
Second loop body:          1
Second loop body:          2
Third loop body:           1
Third loop body:           2
```

##### simple_sample2's output:
```bash
$ ./simple_sample2
First loop body:           1
Second loop body:          1
Third loop body:           1
First loop body:           2
Second loop body:          2
Third loop body:           2
```

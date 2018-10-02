# Installation of the CLAW Compiler

### Requirements

The CLAW Compiler has the followings dependencies:
* Java 1.7 or greater
* Ant 1.9 or greater
* yacc, lex (For OMNI Compiler)
* C/C++ compiler (supports C99) (For OMNI Compiler)
* Fortran compiler (supports Fortran 90) (For OMNI Compiler)
* libxml2 (For OMNI Compiler)
* cmake and make



### Build & install

CLAW Compiler (clawfc) is built on the top of the [OMNI Compiler](http://www.omni-compiler.org).
OMNI Compiler is packaged and built together with the CLAW Compiler.


To build the and install the CLAW Compiler, use the followings commands.

In source build:
```bash
git clone git@github.com:claw-project/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
cmake -DCMAKE_INSTALL_PREFIX=<install_path> .
make
make install
```

Out-of-source build:
```bash
git clone git@github.com:claw-project/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=<install_path> ..
make
make install
```

##### Specific Java version
If you have several Java compiler version installed on your machine, you must
export the `JAVA_HOME` environment variable. This information will be picked
by the build system for the entire project.

##### Offline build steps
If your system has no network connection to the Internet, you need to get the
submodule and the ANT dependencies for the repository. In order to gather all
the dependencies for offline build, you can use the following command:

```bash
cd claw-compiler
./scripts/offline.sh
```

Once you have all the dependencies for the repository, you can copy all the data
to your target system and add `OFFLINE` as a CMake option as follows:

```bash
cmake -DOFFLINE=ON .
```

##### Switch git submodule from `https` to `ssh`
OMNI Compiler is referenced to this repository as a git submodule. The link
to the repository is the `https` link to the official OMNI repository. If your
network only allow ssh connection, you should change the `.gitmodules` file at
the root of this repo.

Git submodule configuration with `https` link to the OMNI Compiler repository:
```
[submodule "omni-compiler"]
	path = omni-compiler
	url = https://github.com/omni-compiler/omni-compiler.git
	branch = master
```

Git submodule configuration with `ssh` link to the OMNI Compiler repository:
```
[submodule "omni-compiler"]
	path = omni-compiler
	url = git@github.com:omni-compiler/omni-compiler.git
	branch = master
```

This must be done before the execution of the `git submodule` commands.


##### Specific steps for Piz Daint
On Piz Daint, specific steps as to be performed in order to have a successful
compilation.

First of all, Ant is not available on Piz Daint. To install it, follow the
instruction [here](./INSTALL_Ant.md).

On Piz Daint, the Cray MPI wrapper must be used regardless of the selected
programming environment. So if you are compiling with PGI or GNU, use the
following commands:

```bash
git clone git@github.com:claw-project/claw-compiler.git
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

# Installation of the CLAW Compiler

### Requirements

The CLAW Compiler has the followings dependencies:
* Java 1.8 or greater
* Ant 1.9 or greater
* cmake and make

Additional dependencies, if built with XCodeML tools from OMNI Compiler: 
* yacc, lex
* C/C++ compiler (supports C99)
* Fortran compiler (supports Fortran 90)
* libxml2



### Build & install

CLAW Compiler (clawfc) uses XCodeML tools (Fortran frontend and backend) from [OMNI Compiler](http://www.omni-compiler.org).
They can be packaged and built together with the CLAW Compiler.


To build and install the CLAW Compiler, use the followings commands.

In source build:
```bash
git clone git@github.com:claw-project/claw-compiler.git
cd claw-compiler
cmake -DCMAKE_INSTALL_PREFIX=<install_path> .
make
make install
```

Out-of-source build:
```bash
git clone git@github.com:claw-project/claw-compiler.git
export SRC_DIR="$(pwd)/claw-compiler"
mkdir <build_path>
cd <build_path>
cmake -DCMAKE_INSTALL_PREFIX=<install_path> -S ${SRC_DIR}
make
make install
```

##### Specific Java version
If you have several Java compiler version installed on your machine, you must
set the cmake variable `JAVA_HOME` environment variable. E.g. 
```cmake -DJAVA_HOME=<install_path> ... ```

##### External XCodeML tools at build-time
It is possible to build CLAW Compiler with already installed version of XCodeML tools. To do this, 
set the cmake option `BUILD_OMNI_XCODEML_TOOLS` to `OFF` and specify the path to the XCodeML tools installation 
directory in cmake variable `OMNI_HOME`.
```cmake -DBUILD_OMNI_XCODEML_TOOLS=OFF -DOMNI_HOME=<xcodeml_tools_install_dir> ... ```

Note however that each version of the CLAW Compiler is tightly bound to the specific version of the XCodeML tools, 
therefore only the version from [CLAW repo](https://github.com/claw-project/xcodeml-tools) with the same GIT commit
as the one specified in cmake variable 'OMNI_GIT_COMMIT_HASH' will be compatible.

##### External XCodeML tools at runtime
By default CLAW Compiler will package XCodeML tools in its install directory. This can be disabled by setting cmake 
option `ADD_OMNI_XCODEML_TOOLS_TO_INSTALL` to `OFF`. Additionally, runtime path to the XCodeML tools can be overridden
by setting the environment variable `OMNI_HOME` before running the CLAW compiler.

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

##### Switch git checkout from `https` to `ssh`
If you are building without external OMNI XCodeML tools and your network only allow ssh connection, set the cmake 
variable `OMNI_GIT_REPOSITORY` to `git@github.com:claw-project/xcodeml-tools.git`

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

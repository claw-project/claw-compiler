# Installation of the CLAW Fortran compiler

### Dependencies

CLAW Fortran compiler (clawfc) is built on the top of the
[OMNI Compiler](http://www,omni-compiler.org). It is currently tested with
version [1.0.1](http://omni-compiler.org/download/stable/omnicompiler-1.0.1.tar.bz2).
OMNI Compiler is packaged and built together with the CLAW Compiler. 


To build the and install the CLAW Fortran Compiler, use the followings commands.
_NOTE_: OMNI Compiler is packaged with the CLAW Compiler so you don't need to
install a separated version.

```bash
git clone git@github.com:C2SM-RCM/claw-compiler.git
cd claw-compiler
git submodule init
git submodule update --remote
cmake -DCMAKE_INSTALL_PREFIX=<install_path> .
make
make install
```

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

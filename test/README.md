# CLAW functional tests

This directory contains a set of functional tests of the CLAW Fortran compiler.
All tests have the same structure as follows:

* `CMakeLists.txt`: CMake file for the test (see below the format).
* `original_code.f90`: The original code with CLAW directives.
* `reference.f90`: A reference code that will be compared to the transformed
  code.

#####


##### CMakeLists structure
Each test has a `CMakeLists.txt` file that set up few variables to generate
the test targets. Here is the format of this file.
```cmake
set(TEST_NAME <test_name>) # test_name must be replaced by a relevant test name
set(TEST_DEBUG ON)         # optional, run clawfc with debug flag
set(OUTPUT_TEST ON)        # optional, execute executable output comparison
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake) # always like this
```

The file `base_test.cmake` is common for all tests and does the following
actions:

1. Run `clawfc` on the file `original_code.f90` and produce a new file
`transformed_code.f90`.
2. Compile `original_code.f90` and `transformed_code.f90` with a standard
Fortran compiler to check their correctness.
3. Execute a `diff` between `transformed_code.f90` and `reference.f90` to check
the correctness of the transformations applied.
4. If the `OUTPUT_TEST` option is set to `ON`, execute a `diff` between the
output of the executable `original_code` and `transformed_code`.

#### Execution
##### All test suite
The test suites can be executed from the root directory (`claw-compiler/`) with
the following command:

```bash
make clean-transformation transformation test
```

The CLAW Fortran compiler must be compiled once before executing the test.

##### One test
Each test has its specific set of target to be run individually. For example,
the test `fusion1` located in `test/loops/fusion1` can be executed with the
following command:

```bash
cd test/loops/fusion1
make clean-fusion1 transform-fusion1
```

More generally, use the following command and modify `test_path`and `test_name`
accordingly.
```bash
cd <test_path>
make clean-<test_name> transform-<test_name>
```

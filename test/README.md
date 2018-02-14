# CLAW functional tests

This directory contains a set of functional tests for the CLAW Fortran compiler.
All tests have the same structure described below:

###### Low level transformation tests
* `original_code.f90`: The original Fortran code with CLAW directives.
* `reference.f90`: A reference Fortran code that will be compared to the
  transformed code.

###### Test case with module and targets for abstraction
* `CMakeLists.txt`: `CMake` file including specific information for the test
  case for the test (see below the format for this file).
* `main.f90`: The test driver including the `PROGRAM` subroutine.
* `reference_main_gpu.f90`: A reference Fortran code that will be compared to the
  transformed code for the GPU target for the main file.
* `reference_main_cpu.f90`: A reference Fortran code that will be compared to the
  transformed code for the CPU target for the main file.
* `mo_column.f90`: The "compute" part of the test. Includes a subroutine which
  abstracts the column/box model with dedicated CLAW directives.
* `reference_gpu.f90`: A reference Fortran code that will be compared to the
  transformed code for the GPU target for the first module file.
* `reference_cpu.f90`: A reference Fortran code that will be compared to the
  transformed code for the CPU target for the first module file.
* `mo_column_extra.f90`: optional module file.
* `reference_extra_gpu.f90`: A reference Fortran code that will be compared to
  the transformed code for the GPU target of the extra module.
* `reference_extra_cpu.f90`: A reference Fortran code that will be compared to
  the transformed code for the CPU target of the extra module.

##### CMakeLists.txt structure
Test are added using a `cmake` function described below.

###### For low level transformation tests
```cmake
claw_add_basic_test(
  NAME <test_name> # Name of the test case
  [ORIGINAL <original_code.f90>] # Original source code
  [TRANSFORMED <transformed_code.f90>] # Transformed code
  [REFERENCE <reference.f90>] # Reference code for compare test
  [WORKING_DIRECTORY <path>] # Directory where code is located
  [CLAW_FLAGS <flags>] # Additional flags passed to clawfc
  [DEBUG]   # Enable debug
  [COMPILE] # Compile original and transformed code
  [COMPARE] # Compare the result of execution between the original and
            # the transformed code
  [IGNORE]  # Ignore tests
)
```

The basic test is doing the following actions:
1. Run `clawfc` on the original source code and produce the transformed code.
2. Compile the original code and the transformed code with a standard
Fortran compiler to check their correctness.
3. Execute a `diff` between the transformed code and the reference to check
the correctness of the transformations applied.
4. If the `COMPARE` option is set enabled, execute a `diff` between the
output of the original and transformed executables.

To ease addition of several test case, the function `claw_add_basic_test_set`
can be used on a directory.
```cmake
claw_add_basic_test_set(
  NAME <set-name>  # Name of the test set
  DIRECTORY <path> # Directory in which subdirectories are listed
  [EXCLUDE <dir1> <dir2>] # Excluded directory
)
```


###### For higher abstraction transformation tests
```cmake
set(TEST_NAME <test_name>)  # test_name must be replaced by a relevant test name
                            # for the test case
set(TEST_DEBUG ON)          # optional, run clawfc with debug flag
set(OUTPUT_TEST ON)         # optional, execute executable output comparison
set(IGNORE_TEST ON)         # optional, does not perform the test but apply
                            # transformations
set(HAS_EXTRA_MOD ON)       # optional, enable the second module file      
                            # transformation in the test case.
set(OPTIONAL_FLAGS <flags>) # pass additional flags to clawfc
set(DIRECTIVE_GPU "--directive=opemacc") # Define the directive language for GPU
set(DIRECTIVE_CPU "--directive=opemmp")  # Define the directive language for CPU
set(OPENACC_ENABLE ON)      # Activate/deactivate OpenACC compilation if available
set(OPENMP_ENABLE OFF)      # Activate/deactivate OpenMP compilation if available
include(${CMAKE_SOURCE_DIR}/test/module_test.cmake) # base cmake file
```

#### Execution
##### All test suite
The test suites can be executed from the root directory (`claw-compiler/`) with
the following command:

```bash
make clean-transformation transformation test
```

The CLAW Fortran compiler must be compiled once before executing the test.

Make sure to use the `clean-transformation` target in order to get rid of
previous code transformations. As the source files do not change, test cases
cannot rely on `make` dependency.

##### Only one test
Each test has its specific set of targets to be run individually. For example,
the test case `fusion1` located in `test/loops/fusion1` can be executed with the
following command:

```bash
cd test/loops/fusion1
make clean-fusion1 transform-fusion1 test
```

More generally, use the following command and modify `test_path`and `test_name`
accordingly.
```bash
cd <test_path>
make clean-<test_name> transform-<test_name> test
```

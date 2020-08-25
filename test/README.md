# CLAW functional tests

This directory contains a set of functional tests for the CLAW Compiler. Each
test applies the CLAW Compiler on a set of input files and compare the output
with references.
They are two category of tests as described below:

#### Basic test: low-level directives
* `original_code.f90`: The original Fortran code with CLAW directives.
* `reference.f90`: A reference Fortran code that will be compared to the
  transformed code.


###### cmake function to add a single test
```cmake
claw_add_basic_test(
  NAME <test_name> # Name of the test case
  [ORIGINAL <original_code.f90>] # Original source code
  [TRANSFORMED <transformed_code.f90>] # Transformed code
  [REFERENCE <reference.f90>] # Reference code for compare test
  [INPUT_DIRECTORY <path>] # Directory where the input files are located
  [WORKING_DIRECTORY <path>] # Directory where intermideate files will be placed
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

###### cmake function to add a test set
To ease addition of several test case, the function `claw_add_basic_test_set`
can be used on a directory.
```cmake
claw_add_basic_test_set(
  NAME <set-name>  # Name of the test set
  DIRECTORY <path> # Directory in which subdirectories are listed
  [EXCLUDE <dir1> <dir2>] # Excluded directory
)
```

#### Advanced test: high-level directive with specific targets
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


###### For higher abstraction transformation tests
```cmake
claw_add_advanced_test(
  NAME <test-name>  # Name of the test case
  [WORKING_DIRECTORY <path>] # Directory in which source files are
)
```

#### Execution
##### All test suite
The test suites can be executed from the root directory (`claw-compiler/`) with
the following command:

```bash
make clean-transformation transformation test
```

The CLAW Compiler must be compiled once before executing the test.

Make sure to use the `clean-transformation` target in order to get rid of
previous code transformations. As the source files do not change, test cases
cannot rely on `make` dependency.

##### Only one test
Each test has its specific set of targets to be run individually. For example,
the test case `fusion1` located in `test/loops/fusion1` can be executed with the
following command:

```bash
cd test/loops/fusion1
make clean-loops-fusion1 transform-loops-fusion1 test
```

More generally, use the following command and modify `test-set` and `test_name`
accordingly.
```bash
cd <test_path>
make clean-<test_set>-<test_name> transform-<test_set>-<test_name> test
```

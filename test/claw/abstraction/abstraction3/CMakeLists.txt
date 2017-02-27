set(TEST_NAME abstraction3)
set(TEST_DEBUG OFF)            # If ON, clawfc is executed with the debig flag
set(OUTPUT_TEST ON)            # If ON, test the output of executables with original
set(IGNORE_TEST ON)            # If ON, ignore all test targets
set(DIRECTIVE_GPU "-d=openmp") # Set directive language to OpenMP
set(OPENACC_ENABLE OFF)        # If ON, try to compile with OpenACC
set(OPENMP_ENABLE OFF)         # If ON, try to compile with OpenMP
include(${CMAKE_SOURCE_DIR}/test/module_test.cmake)

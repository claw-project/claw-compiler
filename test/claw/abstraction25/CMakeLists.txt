set(TEST_NAME abstraction25)
set(TEST_DEBUG ON)    # If ON, clawfc is executed with the debig flag
set(OUTPUT_TEST OFF)   # If ON, test the output of executables with original
set(IGNORE_TEST OFF)   # If ON, ignore all test targets
set(OPENACC_ENABLE ON) # If ON, try to compile with OpenACC
set(OPENMP_ENABLE OFF) # If ON, try to compile with OpenMP
set(DIRECTIVE_CPU "--directive=none")  # Define the directive language for CPU  
include(${CMAKE_SOURCE_DIR}/test/module_test.cmake)

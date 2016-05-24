set(TEST_NAME abstraction3)
set(TEST_DEBUG OFF)  # If ON, clawfc is executed with the debig flag
set(OUTPUT_TEST ON)  # If ON, test the output of executables with original
set(IGNORE_TEST ON) # If ON, ignore all test targets
set(DIRECTIVE_GPU "-d=openmp")
include(${CMAKE_SOURCE_DIR}/test/module_test.cmake)

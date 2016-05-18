set(TEST_NAME abstraction1)
set(TEST_DEBUG OFF)  # If ON, clawfc is executed with the debig flag
set(OUTPUT_TEST OFF) # If ON, test the output of executables with original
set(IGNORE_TEST OFF) # If ON, ignore all test targets
include(${CMAKE_SOURCE_DIR}/test/module_test.cmake)

set(TEST_NAME fusion13)
set(OPTIONAL_FLAGS "--directive=openmp") # Set accelerator target OpemMP
set(TEST_DEBUG OFF)
set(OUTPUT_TEST OFF)
set(IGNORE_TEST OFF)
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake)

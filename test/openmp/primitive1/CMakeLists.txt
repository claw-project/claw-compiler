set(TEST_NAME omp-primitive1)
set(OPTIONAL_FLAGS "--target=cpu" "--directive=openmp")
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake)

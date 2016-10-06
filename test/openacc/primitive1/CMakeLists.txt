set(TEST_NAME acc-primitive1)
set(OPTIONAL_FLAGS "--target=gpu" "--directive=openacc")
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake)

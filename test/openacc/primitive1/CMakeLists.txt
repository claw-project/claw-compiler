set(TEST_NAME acc-primitive1)
set(OPTIONAL_FLAGS "--target=gpu" "--directive=openacc")
set(NO_COMPILE ON)
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake)

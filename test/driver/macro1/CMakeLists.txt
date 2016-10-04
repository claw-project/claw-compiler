set(TEST_NAME macro1)
set(OPTIONAL_FLAGS "--force" "--target=gpu" "--directive=openacc")
include(${CMAKE_SOURCE_DIR}/test/base_test.cmake)

set(TEST_NAME abstraction30)
set(TEST_DEBUG OFF)    # If ON, clawfc is executed with the debug flag
set(CLAW_TARGET gpu)
set(CLAW_DIRECTIVE openacc)
include(${CMAKE_SOURCE_DIR}/test/failure_test.cmake)

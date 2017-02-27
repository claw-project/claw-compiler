enable_language(Fortran)

# Set up some common env variable for the transformation flow
set(CLAWFC ${CMAKE_SOURCE_DIR}/driver/clawfc_test)
set(BUILD_TEST_TARGET transformation)
set(CLEAN_TEST_TARGET clean-transformation)

add_custom_target(${BUILD_TEST_TARGET})
add_custom_target(${CLEAN_TEST_TARGET})

# Define some test case sets
add_custom_target(${BUILD_TEST_TARGET}-claw-directive)
add_custom_target(${CLEAN_TEST_TARGET}-claw-directive)
add_custom_target(${BUILD_TEST_TARGET}-claw-abstraction)
add_custom_target(${CLEAN_TEST_TARGET}-claw-abstraction)
add_custom_target(${BUILD_TEST_TARGET}-driver)
add_custom_target(${CLEAN_TEST_TARGET}-driver)
add_custom_target(${BUILD_TEST_TARGET}-loops)
add_custom_target(${CLEAN_TEST_TARGET}-loops)
add_custom_target(${BUILD_TEST_TARGET}-openacc)
add_custom_target(${CLEAN_TEST_TARGET}-openacc)
add_custom_target(${BUILD_TEST_TARGET}-openmp)
add_custom_target(${CLEAN_TEST_TARGET}-openmp)
add_custom_target(${BUILD_TEST_TARGET}-utilities)
add_custom_target(${CLEAN_TEST_TARGET}-utilities)

# List of test directories
add_subdirectory(claw)
add_subdirectory(driver)
add_subdirectory(loops)
add_subdirectory(utilities)
add_subdirectory(openacc)
add_subdirectory(openmp)


# Custom target to run the full transformation test suite
add_custom_target(test-suite
  COMMAND ${CMAKE_MAKE_PROGRAM} ${CLEAN_TEST_TARGET} ${BUILD_TEST_TARGET} test
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

enable_language(Fortran)

# Set up some common env variable for the transformation flow
set(CLAWFC ${CMAKE_SOURCE_DIR}/driver/clawfc_test)
set(BUILD_TEST_TARGET transformation)
set(CLEAN_TEST_TARGET clean-transformation)

add_custom_target(${BUILD_TEST_TARGET})
add_custom_target(${CLEAN_TEST_TARGET})

# List of test directories
add_subdirectory(claw)
add_subdirectory(driver)
add_subdirectory(fortran)
add_subdirectory(loops)
add_subdirectory(utilities)
add_subdirectory(openacc)

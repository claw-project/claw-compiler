# This file is released under terms of BSD license
# See LICENSE file for more information

enable_language(Fortran)

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS) # Problem on some cray system with -rdynamic
endif()

# Set up some common env variable for the transformation flow
set(CLAWFC ${CMAKE_BINARY_DIR}/driver/clawfc_test)
set(BUILD_TEST_TARGET transformation)
set(CLEAN_TEST_TARGET clean-transformation)

add_custom_target(${BUILD_TEST_TARGET})
add_custom_target(${CLEAN_TEST_TARGET})

# Define some test case sets
foreach(TCSET claw-directive claw-sca driver loops openacc openmp utilities omni)
  add_custom_target(${BUILD_TEST_TARGET}-${TCSET})
  add_custom_target(${CLEAN_TEST_TARGET}-${TCSET})
endforeach()

include(claw_reference_test.cmake)

# List of test directories
add_subdirectory(claw)
add_subdirectory(driver)
add_subdirectory(loops)
add_subdirectory(omni)
add_subdirectory(openacc)
add_subdirectory(openmp)
add_subdirectory(utilities)

add_test(
  NAME library-test-claw_f_lib
  COMMAND ${CMAKE_COMMAND} -E env
    CMAKE_SOURCE_DIR=${CMAKE_SOURCE_DIR}
    CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR}
     ./test.claw_f_lib
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test/lib
)

# Custom target to run the full transformation test suite
add_custom_target(test-suite
  COMMAND ${CMAKE_MAKE_PROGRAM} ${CLEAN_TEST_TARGET} ${BUILD_TEST_TARGET} test
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

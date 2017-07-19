# List of test cases targeting the driver
set(TEST_SET driver)
add_subdirectory(backslash_preprocessor)
add_subdirectory(dependencies1)
add_subdirectory(dependencies2)
add_subdirectory(dependencies3)
add_subdirectory(dependencies4)
add_subdirectory(dependencies5)
add_subdirectory(dependencies6)
add_subdirectory(force)   # --force option
add_subdirectory(ignore1) # test the ignore directive (driver directive)
add_subdirectory(ignore2) # test the ignore directive (driver directive)
add_subdirectory(ignore3) # test the ignore directive (driver directive)
add_subdirectory(macro1)  # test of automatic OpenACC macro
add_subdirectory(macro2)  # test of automatic OpenMP macro
add_subdirectory(macro3)  # test user defined macro passed to the driver
add_subdirectory(target1) # check automatic passing of _OPENACC macro
add_subdirectory(target2) # check automatic passing of _OPENMP macro
add_subdirectory(verbatim1) # test the verbatim directive (driver directive)


add_test(
  NAME library-test-claw_f_lib
  COMMAND ./test.claw_f_lib
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib
)

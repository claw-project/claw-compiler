# List of test cases targeting the driver
set(TEST_SET driver)
add_subdirectory(force)   # --force option
add_subdirectory(ignore1) # test the ignore directive (driver directive)
add_subdirectory(ignore2) # test the ignore directive (driver directive)
add_subdirectory(ignore3) # test the ignore directive (driver directive)
add_subdirectory(macro1)  # test the passing of OpenACC macro
add_subdirectory(macro2)  # test the passing of OpenMP macro
add_subdirectory(target1) # check automatic passing of _OPENACC macro
add_subdirectory(target2) # check automatic passing of _OPENMP macro
add_subdirectory(verbatim1) # test the verbatim directive (driver directive)
add_subdirectory(dependencies1)
add_subdirectory(dependencies2)
add_subdirectory(dependencies3)
add_subdirectory(dependencies4)
add_subdirectory(dependencies5)

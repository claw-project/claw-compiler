# List of test cases targeting the driver
add_subdirectory(force)   # --force option
add_subdirectory(ignore1) # test the ignore directives (driver directives)
add_subdirectory(ignore2) # test the ignore directives (driver directives)
add_subdirectory(ignore3) # test the ignore directives (driver directives)
add_subdirectory(target1) # check automatic passing of _OPENACC macro
add_subdirectory(target2) # check automatic passing of _OPENMP macro

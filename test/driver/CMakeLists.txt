# List of test cases targeting the driver
add_subdirectory(force)   # --force option
add_subdirectory(target1) # check automatic passing of _OPENACC macro
add_subdirectory(target2) # check automatic passing of _OPENMP macro

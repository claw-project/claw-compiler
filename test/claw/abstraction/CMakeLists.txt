# List of test directories
set(TEST_SET claw-abstraction)
add_subdirectory(abstraction1) # simple 1d
add_subdirectory(abstraction2) # simple 2d
add_subdirectory(abstraction3) # openmp
add_subdirectory(abstraction4) # simple with forward
add_subdirectory(abstraction5) # cross-module
add_subdirectory(abstraction6) # cross-module multiple forward
add_subdirectory(abstraction7)
add_subdirectory(abstraction8)
add_subdirectory(abstraction9)  # Private function
add_subdirectory(abstraction10) # Private function, same as 9 but fct reorder
add_subdirectory(abstraction11) # data over with assumed shaped arrays
add_subdirectory(abstraction12) # data over with non-assumed shaped arrays
add_subdirectory(abstraction13) # data over with non-assumed shaped arrays and multiple over clauses
add_subdirectory(abstraction14) # adding dimension in the middle of already defined ones
add_subdirectory(abstraction15) # data-over clause on a scalar
add_subdirectory(abstraction16) # parallelize forward over type-bound procedure call
add_subdirectory(abstraction17) # parallelize in a subroutine with contains
add_subdirectory(abstraction18) # parallelize with automatic promotion std
add_subdirectory(abstraction19) # parallelize in a subroutine with contains
add_subdirectory(abstraction20) # parallelize/forward with optional not set
add_subdirectory(abstraction21) # parallelize/forward with optional set
add_subdirectory(abstraction22) # parallelize with automatic promotion placed
add_subdirectory(abstraction23) # parallelize with promotion of result var
add_subdirectory(abstraction24) # parallelize forward with namedValue
add_subdirectory(abstraction25) # parallelize forward non fct field
add_subdirectory(abstraction26) # parallelize forward with scalar to 1d propagation
add_subdirectory(abstraction27) # pure keyword

# List of test directories
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
add_subdirectory(call1)
add_subdirectory(kcache1) # assign cache
add_subdirectory(kcache2) # assign cache with init clause
add_subdirectory(kcache3) # assign cache with init clause and data cache
add_subdirectory(kcache4) # assign cache with init, data and private clauses
add_subdirectory(compile-guard1) # Check removal of the compile guard OpenACC
add_subdirectory(compile-guard2) # Check removal of the compile guard OpenMP

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
add_subdirectory(call1)
add_subdirectory(kcache1) # assign cache
add_subdirectory(kcache2) # assign cache with init clause
add_subdirectory(kcache3) # assign cache with init clause and data cache
add_subdirectory(kcache4) # assign cache with init, data and private clauses
add_subdirectory(compile-guard1) # Check removal of the compile guard OpenACC
add_subdirectory(compile-guard2) # Check removal of the compile guard OpenMP

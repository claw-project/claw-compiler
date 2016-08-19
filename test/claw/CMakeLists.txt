# List of test directories
add_subdirectory(abstraction1)
add_subdirectory(abstraction2)
add_subdirectory(abstraction3)
add_subdirectory(abstraction4)
add_subdirectory(abstraction5)
add_subdirectory(abstraction6)
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

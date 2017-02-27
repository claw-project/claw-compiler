# List of test directories
set(TEST_SET claw-directive)

add_subdirectory(call1)
add_subdirectory(call2)   # with target clause
add_subdirectory(kcache1) # assign cache
add_subdirectory(kcache2) # assign cache with init clause
add_subdirectory(kcache3) # assign cache with init clause and data cache
add_subdirectory(kcache4) # assign cache with init, data and private clauses
add_subdirectory(kcache5) # with target clause
add_subdirectory(compile-guard1) # Check removal of the compile guard OpenACC
add_subdirectory(compile-guard2) # Check removal of the compile guard OpenMP

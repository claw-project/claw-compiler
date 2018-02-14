#
# Low-level directives tests
#

# Information for specific test cases
# -----------------------------------
# call2: with target clause
# kcache1: assign cache
# kcache2: assign cache with init clause
# kcache3: assign cache with init clause and data cache
# kcache4: assign cache with init, data and private clauses
# kcache5: with target clause
# compile-guard1: Check removal of the compile guard OpenACC
# compile-guard2: Check removal of the compile guard OpenMP
# compile-guard3: Compile guard without other claw directives
# compile-guard4: Compile guard without other claw directives

# Set some specific CLAW Compiler flag for some tests
set(CLAW_FLAGS_compile-guard2 --directive=openmp)
set(CLAW_FLAGS_compile-guard4 --directive=openmp)

claw_add_basic_test_set(
  NAME claw-directive DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

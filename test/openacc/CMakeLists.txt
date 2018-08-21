#
# OpenACC related tests
#

# Information for specific test cases
# -----------------------------------
# array1: # simple transformation with 1 dimension
# array2: # simple transformation with 2 dimension
# array4: # Block transformation without dependency
# array5: # Block transformation without dependency, allocatable OUTPUT=ON
# array6: # Lower/upper bounds with offset
# array7: # Induction clause
# array8: # Block transformation with different index ranges
# array10: # With target clause

# Set some specific CLAW Compiler flag for some tests
set(CLAW_FLAGS_primitive1 --target=gpu --directive=openacc)
set(CLAW_FLAGS_primitive2 --target=gpu --directive=openacc)
set(CLAW_FLAGS_primitive3 --target=gpu --directive=openacc)
set(CLAW_FLAGS_primitive4 --target=gpu --directive=openacc)

# Set some specific test options
set(NOCOMPILE_lib ON)
set(NOCOMPILE_primitive2 ON)
set(COMPARE_array5 ON)

claw_add_basic_test_set(NAME openacc DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

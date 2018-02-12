# List of test directories to be executed
set(TEST_SET openacc)

# array1: # simple transformation with 1 dimension
# array2: # simple transformation with 2 dimension
# array4: # Block transformation without dependency
# array5: # Block transformation without dependency, allocatable OUTPUT=ON
# array6: # Lower/upper bounds with offset
# array7: # Induction clause
# array8: # Block transformation with different index ranges
# array10: # With target clause

# Set some specific CLAW Compiler flag for some tests
set(CLAW_FLAGS_primitive1 "--target=gpu --directive=openacc")
set(CLAW_FLAGS_primitive2 "--target=gpu --directive=openacc")
set(CLAW_FLAGS_primitive3 "--target=gpu --directive=openacc")

# Set some specific test options
set(NOCOMPILE_lib ON)
set(NOCOMPILE_primitive2 ON)
set(COMPARE_array5 ON)

subdirlist(tests_dirs ${CMAKE_CURRENT_SOURCE_DIR})
foreach(t_name ${tests_dirs})

  if(NO_COMPILE_${t_name})
    set(test_option_compile COMPILE)
  else()
    set(test_option_compile "")
  endif()

  if(COMPARE_${t_name})
    set(test_option_compare COMPARE)
  else()
    set(test_option_compare "")
  endif()

  add_basic_test(
    NAME ${TEST_SET}-${t_name}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${t_name}
    CLAW_FLAGS ${CLAW_FLAGS_${t_name}}
    ${test_option_compile}
    ${test_option_compare}
  )
endforeach()

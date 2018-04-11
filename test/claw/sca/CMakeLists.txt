#
# CLAW SCA related tests
#

# Information for specific test cases
# -----------------------------------
# abstraction1: simple 1d
# abstraction2: simple 2d
# abstraction3: openmp
# abstraction4: simple with forward
# abstraction5: cross-module
# abstraction6: cross-module multiple forward
# abstraction9: Private function
# abstraction10: Private function, same as 9 but fct reorder
# abstraction11: data over with assumed shaped arrays
# abstraction12: data over with non-assumed shaped arrays
# abstraction13: data over with non-assumed shaped arrays and multiple over clauses
# abstraction14: adding dimension in the middle of already defined ones
# abstraction15: data-over clause on a scalar
# abstraction16: parallelize forward over type-bound procedure call
# abstraction17: parallelize in a subroutine with contains
# abstraction18: parallelize with automatic promotion std
# abstraction19: parallelize in a subroutine with contains
# abstraction20: parallelize/forward with optional not set
# abstraction21: parallelize/forward with optional set
# abstraction22: parallelize with automatic promotion placed
# abstraction23: parallelize with promotion of result var
# abstraction24: parallelize forward with namedValue
# abstraction25: parallelize forward non fct field
# abstraction26: parallelize forward with scalar to 1d propagation
# abstraction27: pure keyword
# abstraction29: claw nodep on k loop (openacc_collapse = false)
# abstraction30: unsupported statement
# abstraction31: parallelize forward with type%member slicing
# abstraction32: parallel region start/end after/before unsupported + update/create
# abstraction33: claw nodep on k loop (openacc_collapse = true)
# abstraction34: (#355) CPU specific categorization for assign statement

foreach(loop_var RANGE 1 35)
  if(NOT ${loop_var} EQUAL 30)
    set(CLAW_FLAGS_TARGET_CPU_abstraction${loop_var} --directive=none)
  endif()
endforeach()

set(CLAW_FLAGS_TARGET_CPU_abstraction3 --directive=openmp)
set(CLAW_FLAGS_abstraction32 --config=promote.xml)
set(CLAW_FLAGS_abstraction33 --config=enable_collapse.xml)

claw_add_advanced_test_set(
  NAME claw-sca
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  EXCLUDE abstraction30
)

# Specific "fail" test to check correct failure
claw_add_failure_test(
  NAME claw-sca-abstraction30
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/abstraction30
  SET claw-sca
  CLAW_TARGET gpu
  CLAW_DIRECTIVE openacc
)

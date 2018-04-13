#
# CLAW SCA related tests
#

# Information for specific test cases
# -----------------------------------
# sca1: simple 1d
# sca2: simple 2d
# sca3: openmp
# sca4: simple with forward
# sca5: cross-module
# sca6: cross-module multiple forward
# sca9: Private function
# sca10: Private function, same as 9 but fct reorder
# sca11: data over with assumed shaped arrays
# sca12: data over with non-assumed shaped arrays
# sca13: data over with non-assumed shaped arrays and multiple over clauses
# sca14: adding dimension in the middle of already defined ones
# sca15: data-over clause on a scalar
# sca16: parallelize forward over type-bound procedure call
# sca17: parallelize in a subroutine with contains
# sca18: parallelize with automatic promotion std
# sca19: parallelize in a subroutine with contains
# sca20: parallelize/forward with optional not set
# sca21: parallelize/forward with optional set
# sca22: parallelize with automatic promotion placed
# sca23: parallelize with promotion of result var
# sca24: parallelize forward with namedValue
# sca25: parallelize forward non fct field
# sca26: parallelize forward with scalar to 1d propagation
# sca27: pure keyword
# sca29: claw nodep on k loop (openacc_collapse = false)
# sca30: unsupported statement
# sca31: parallelize forward with type%member slicing
# sca32: parallel region start/end after/before unsupported + update/create
# sca33: claw nodep on k loop (openacc_collapse = true)
# sca34: (#355) CPU specific categorization for assign statement

# sca36: Correct handling of indirect promotion

foreach(loop_var RANGE 1 36)
  if(NOT ${loop_var} EQUAL 30)
    set(CLAW_FLAGS_TARGET_CPU_sca${loop_var} --directive=none)
  endif()
endforeach()

set(CLAW_FLAGS_TARGET_CPU_sca3 --directive=openmp)
set(CLAW_FLAGS_sca32 --config=promote.xml)
set(CLAW_FLAGS_sca33 --config=enable_collapse.xml)

claw_add_advanced_test_set(
  NAME claw-sca
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  EXCLUDE sca30
)

# Specific "fail" test to check correct failure
claw_add_failure_test(
  NAME claw-sca-sca30
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/sca30
  SET claw-sca
  CLAW_TARGET gpu
  CLAW_DIRECTIVE openacc
)

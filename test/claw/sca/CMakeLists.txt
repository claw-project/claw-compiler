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
# sca7: forward with function result
# sca8: forward dependencies
# sca9: Private function
# sca10: Private function, same as 9 but fct reorder
# sca11: data over with assumed shaped arrays
# sca12: data over with non-assumed shaped arrays
# sca13: data over with non-assumed shaped arrays and multiple over clauses
# sca14: adding dimension in the middle of already defined ones
# sca15: data-over clause on a scalar
# sca16: sca forward over type-bound procedure call
# sca17: sca in a subroutine with contains
# sca18: sca with automatic promotion std
# sca19: sca in a subroutine with contains
# sca20: sca/forward with optional not set
# sca21: sca/forward with optional set
# sca22: sca with automatic promotion placed
# sca23: sca with promotion of result var
# sca24: sca forward with namedValue
# sca25: sca forward non fct field
# sca26: sca forward with scalar to 1d propagation
# sca27: pure keyword
# sca29: claw nodep on k loop (openacc_collapse = false)
# sca30: unsupported statement
# sca31: sca forward with type%member slicing
# sca32: promotion instead of privatization of values + parallel region start/end after/before unsupported + update/create
# sca33: claw nodep on k loop (openacc_collapse = true)
# sca34: (#355) CPU specific categorization for assign statement
# sca35: CPU handle "if" correcly
# sca36: Correct handling of indirect promotion
# sca37: Check not mixed "!$acc loop seq" order directive generation
# sca38: Check independent "if" handling for CPU target
# sca39: Same as SCA1 with override option passed to the driver -x
# sca40: SCA with model config
# sca41: SCA on ELEMENTAL function
# sca42: Bug for #499
# sca43: SCA on ELEMENTAL function with present intrinsic

foreach(loop_var RANGE 1 43)
  if(NOT ${loop_var} EQUAL 30)
    set(CLAW_FLAGS_TARGET_CPU_sca${loop_var} --directive=none)
  endif()
endforeach()

set(CLAW_FLAGS_TARGET_CPU_sca3 --directive=openmp)
set(CLAW_FLAGS_sca32 --config=promote.xml)
set(CLAW_FLAGS_sca33 --config=enable_collapse.xml)
set(CLAW_FLAGS_sca39 -x=cpu_trans_strategy:single)
set(
  CLAW_FLAGS_sca40
  --model-config=${CMAKE_CURRENT_SOURCE_DIR}/basic_model.toml
)
set(
  CLAW_FLAGS_sca41
  --model-config=${CMAKE_CURRENT_SOURCE_DIR}/sca41/model.toml
)
set(
  CLAW_FLAGS_sca43
  --model-config=${CMAKE_CURRENT_SOURCE_DIR}/sca43/model.toml
)

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

# CLAW driver related test set

set(CLAW_FLAGS_backslash_preprocessor "--force")
set(NOCOMPILE_backslash_preprocessor ON)

set(CLAW_FLAGS_dependencies1 "--force -J subfolder/")
set(NOCOMPILE_dependencies1 ON)

set(CLAW_FLAGS_dependencies2 "--force -J subfolder/")
set(NOCOMPILE_dependencies2 ON)

set(CLAW_FLAGS_dependencies3 "--force -J subfolder/")
set(NOCOMPILE_dependencies3 ON)

set(CLAW_FLAGS_dependencies4 "--force")
set(NOCOMPILE_dependencies4 ON)
set(CLAW_FLAGS_dependencies5 "--force")
set(NOCOMPILE_dependencies5 ON)
set(CLAW_FLAGS_dependencies6 "--force")
set(NOCOMPILE_dependencies6 ON)
set(CLAW_FLAGS_dependencies7 "--force")
set(NOCOMPILE_dependencies7 ON)
set(CLAW_FLAGS_dependencies8 "--force")
set(NOCOMPILE_dependencies8 ON)

set(CLAW_FLAGS_force "--force")

set(CLAW_FLAGS_macro1 "--force --target=gpu --directive=openacc")
set(CLAW_FLAGS_macro2 "--force --directive=openmp")
set(CLAW_FLAGS_macro3 "--force -DUSER_MACRO1 -DUSER_MACRO2")
set(CLAW_FLAGS_target1 "--target=gpu --directive=openacc")
set(CLAW_FLAGS_target2 "--target=gpu --directive=openmp")

add_basic_test_set(NAME driver DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} EXCLUDE lib)


# backslash_preprocessor: Workaround for \ at end of line
# dependencies1: multiple dependencies with subfolder
# dependencies2: 1 dependency in a subfolder
add_subdirectory(dependencies3)
add_subdirectory(dependencies4)
add_subdirectory(dependencies5)
add_subdirectory(dependencies6) # intrinsic module dependency iso_c_binding
add_subdirectory(dependencies7) # intrinsic module dependency iso_fortran_env
add_subdirectory(dependencies8) # intrinsic module dependency ieee_*

add_subdirectory(force)     # --force option

add_subdirectory(ignore1)   # test the ignore directive (driver directive)
add_subdirectory(ignore2)   # test the ignore directive (driver directive)
add_subdirectory(ignore3)   # test the ignore directive (driver directive)

add_subdirectory(macro1)    # test of automatic OpenACC macro
add_subdirectory(macro2)    # test of automatic OpenMP macro
add_subdirectory(macro3)    # test user defined macro passed to the driver

add_subdirectory(target1)   # check automatic passing of _OPENACC macro
add_subdirectory(target2)   # check automatic passing of _OPENMP macro

add_subdirectory(verbatim1) # test the verbatim directive (driver directive)

add_subdirectory(external1) # external transformation set

# Test for bash library functions
add_test(
  NAME library-test-claw_f_lib
  COMMAND ./test.claw_f_lib
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/lib
)

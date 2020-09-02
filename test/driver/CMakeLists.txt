#
# CLAW driver related tests
#

# Information for specific test cases
# -----------------------------------
# add_paren: Test --add-paren driver option
# backslash_preprocessor: Workaround for \ at end of line
# dependencies1: multiple dependencies with subfolder
# dependencies2: one dependency in a subfolder
# dependencies6: intrinsic module dependency iso_c_binding
# dependencies7: intrinsic module dependency iso_fortran_env
# dependencies8: intrinsic module dependency ieee_*
# force: --force option
# ignore1: test the ignore directive (driver directive)
# ignore2: test the ignore directive (driver directive)
# ignore3: test the ignore directive (driver directive)
# macro1: test of automatic OpenACC macro
# macro2: test of automatic OpenMP macro
# macro3: test user defined macro passed to the driver
# target1: check automatic passing of _OPENACC macro
# target2: check automatic passing of _OPENMP macro
# verbatim1: test the verbatim directive (driver directive)
# external1: external transformation set

set(CLAW_FLAGS_backslash_preprocessor --force)
set(NOCOMPILE_backslash_preprocessor ON)

set(CLAW_FLAGS_dependencies1 --force -J subfolder/ -D__DEP2__ --verbose)
set(NOCOMPILE_dependencies1 ON)

set(CLAW_FLAGS_dependencies2 --force -J subfolder/)
set(NOCOMPILE_dependencies2 ON)

set(CLAW_FLAGS_dependencies3 --force -J subfolder/)
set(NOCOMPILE_dependencies3 ON)

set(CLAW_FLAGS_dependencies4 --force)
set(NOCOMPILE_dependencies4 ON)
set(CLAW_FLAGS_dependencies5 --force)
set(NOCOMPILE_dependencies5 ON)
set(CLAW_FLAGS_dependencies6 --force)
set(NOCOMPILE_dependencies6 ON)
set(CLAW_FLAGS_dependencies7 --force)
set(NOCOMPILE_dependencies7 ON)
set(CLAW_FLAGS_dependencies8 --force)
set(NOCOMPILE_dependencies8 ON)

set(CLAW_FLAGS_force --force)

set(CLAW_FLAGS_macro1 --force --target=gpu --directive=openacc)
set(CLAW_FLAGS_macro2 --force --directive=openmp)
set(CLAW_FLAGS_macro3 --force -DUSER_MACRO1 -DUSER_MACRO2)
set(CLAW_FLAGS_target1 --target=gpu --directive=openacc)
set(CLAW_FLAGS_target2 --target=gpu --directive=openmp)

set(CLAW_FLAGS_external1 --force --config=external_config.xml)
set(NOCOMPILE_external1 ON)
set(CLAW_TRANS_SET_PATH "${CX2T_DIST_DIR}")

set(CLAW_FLAGS_keep_comment --force --keep-comment)

set(CLAW_FLAGS_add_paren --force --add-paren)

claw_add_basic_test_set(NAME driver DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

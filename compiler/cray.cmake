# This file is released under terms of BSD license
# See LICENSE file for more information

# To avoid multiple include of different compiler
if(__claw_base_compiler)
  return()
endif()
set(__claw_base_compiler YES)

# For Cray Compiler
set(FPPFLAGS "-e P -hnoomp")          # for preprocessing only
set(FPP_REDIRECT false)               # cannot use redirection > to save file
set(CLAW_TEST_FFP_FLAGS "-e Z")       # for preprocessing and compilation
set(TEST_BASE_FLAGS "-h noacc,noomp") # Unactivate OpenACC and OpenMP
set(OPENACC_FLAGS "-h acc,noomp")     # flags to compile with OpenACC support
set(OPENMP_FLAGS "-h noacc,omp")      # flags to compile with OpenMP support
set(OMNI_TARGET "")                   # Compilation of OMNI compiler
set(COMPILER_MACRO "-D_CRAYFTN")      # predefined macro by compiler
set(FC_VENDOR "cray")
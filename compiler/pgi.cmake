# This file is released under terms of BSD license
# See LICENSE file for more information

# To avoid multiple include of different compiler
if(__claw_base_compiler)
  return()
endif()
set(__claw_base_compiler YES)

# For PGI compiler
set(FPPFLAGS "-E")                      # for preprocessing only
set(FPP_REDIRECT true)                  # use redirection > to save file
set(CLAW_TEST_FFP_FLAGS "-Mpreprocess") # force preprocessing
set(TEST_BASE_FLAGS "")                 # Base flags for test case compilation
set(OPENACC_FLAGS "-acc")               # flags to compile with OpenACC support
                                        # TODO maybe need -ta=<target>
set(OPENMP_FLAGS "-mp")                 # flags to compile with OpenMP support
set(OMNI_CONF_OPTION ${OMNI_CONF_OPTION} "CPP=pgcc -E")
set(COMPILER_MACRO "-D_PGI")            # predefined macro by compiler
set(FC_VENDOR "portland")

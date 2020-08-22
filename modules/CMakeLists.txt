# This file is released under terms of BSD license
# See LICENSE file for more information

# A list of .xmod files to be installed
set(xmodFiles "")

# Generate .xmod file module for common intrinsic libraries
omni_generate_xmod(
  TARGET xmod-openacc SOURCE openacc_lib.f90 DEPENDS omni-compiler)
list(APPEND xmodFiles ${CMAKE_CURRENT_BINARY_DIR}/openacc.xmod)

omni_generate_xmod(
  TARGET xmod-ieee_features SOURCE ieee_features.f90 DEPENDS omni-compiler)
list(APPEND xmodFiles ${CMAKE_CURRENT_BINARY_DIR}/ieee_features.xmod)

omni_generate_xmod(
  TARGET xmod-ieee_exceptions SOURCE ieee_exceptions.f90 DEPENDS omni-compiler)
list(APPEND xmodFiles ${CMAKE_CURRENT_BINARY_DIR}/ieee_exceptions.xmod)

omni_generate_xmod(
  TARGET xmod-ieee_arithmetic SOURCE ieee_arithmetic.f90 DEPENDS omni-compiler)
add_dependencies(xmod-ieee_arithmetic xmod-ieee_exceptions)
list(APPEND xmodFiles ${CMAKE_CURRENT_BINARY_DIR}/ieee_arithmetic.xmod)

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  omni_generate_xmod(
    TARGET xmod-ifcore SOURCE intel/ifcore.f90 DEPENDS omni-compiler)
  list(APPEND xmodFiles ${CMAKE_CURRENT_BINARY_DIR}/ifcore.xmod)
endif()

# Install in the intrinsic mod directory
install(FILES ${xmodFiles} DESTINATION ${CMAKE_INSTALL_PREFIX}/fincludes)

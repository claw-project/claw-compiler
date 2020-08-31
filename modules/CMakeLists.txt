# This file is released under terms of BSD license
# See LICENSE file for more information

# Generate .xmod file module for common intrinsic libraries
omni_generate_xmod(
  TARGET xmod-openacc SOURCE openacc_lib.f90 DEPENDS omni-compiler OUTPUT "openacc.xmod")
omni_generate_xmod(
  TARGET xmod-ieee_features SOURCE ieee_features.f90 DEPENDS omni-compiler OUTPUT "ieee_features.xmod")
omni_generate_xmod(
  TARGET xmod-ieee_exceptions SOURCE ieee_exceptions.f90 DEPENDS omni-compiler OUTPUT "ieee_exceptions.xmod")
omni_generate_xmod(
  TARGET xmod-ieee_arithmetic SOURCE ieee_arithmetic.f90 DEPENDS omni-compiler OUTPUT "ieee_arithmetic.xmod")

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  omni_generate_xmod(
    TARGET xmod-ifcore SOURCE intel/ifcore.f90 DEPENDS omni-compiler OUTPUT "ifcore.xmod")
endif()

add_dependencies(xmod-ieee_arithmetic xmod-ieee_exceptions)

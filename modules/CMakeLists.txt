# This file is released under terms of BSD license
# See LICENSE file for more information

# Generate .xmod file module for common intrinsic libraries
omni_generate_xmod(
  TARGET xmod-openacc SOURCE openacc_lib.f90 DEPENDS omni-compiler)
omni_generate_xmod(
  TARGET xmod-ieee_features SOURCE ieee_features.f90 DEPENDS omni-compiler)
omni_generate_xmod(
  TARGET xmod-ieee_exceptions SOURCE ieee_exceptions.f90 DEPENDS omni-compiler)
omni_generate_xmod(
  TARGET xmod-ieee_arithmetic SOURCE ieee_arithmetic.f90 DEPENDS omni-compiler)

add_dependencies(xmod-ieee_arithmetic xmod-ieee_exceptions)

# Install in the intrinsic mod directory
install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/openacc.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_arithmetic.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_exceptions.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_features.xmod
  DESTINATION
    ${CMAKE_INSTALL_PREFIX}/fincludes
)

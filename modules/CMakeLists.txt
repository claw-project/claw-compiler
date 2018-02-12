# Generate .xmod file for dummy OpenACC library module
add_custom_target(xmod-openacc ALL)
add_custom_target(xmod-ieee_exceptions ALL)
add_custom_target(xmod-ieee_arithmetic ALL)
add_custom_target(xmod-ieee_features ALL)

omni_generate_xmod(xmod-openacc openacc_lib.f90)
omni_generate_xmod(xmod-ieee_features ieee_features.f90)
omni_generate_xmod(xmod-ieee_exceptions ieee_exceptions.f90)
omni_generate_xmod(xmod-ieee_arithmetic ieee_arithmetic.f90)
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

# Generate .xmod file for dummy OpenACC library module
add_custom_target(xmod-openacc ALL)
add_custom_command(
  TARGET xmod-openacc
  COMMAND ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
    ${CMAKE_CURRENT_SOURCE_DIR}/openacc_lib.f90 > /dev/null
  COMMENT "Generating .xmod file for OpenACC runtime library"
)
install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/openacc.xmod
  DESTINATION ${CMAKE_INSTALL_PREFIX}/fincludes
)

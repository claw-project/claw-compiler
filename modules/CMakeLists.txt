# Generate .xmod file for dummy OpenACC library module
add_custom_target(xmod-openacc ALL)
add_custom_target(xmod-ieee_exceptions ALL)
add_custom_target(xmod-ieee_arithmetic ALL)
add_custom_target(xmod-ieee_features ALL)

# Dedicated target for OpenACC
add_custom_command(
  TARGET xmod-openacc
  COMMAND ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
    ${CMAKE_CURRENT_SOURCE_DIR}/openacc_lib.f90 > /dev/null
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/openacc_lib.f90
  COMMENT "Generating .xmod file for OpenACC runtime library"
)

set(FPP_ARG_LIST ${FPPFLAGS})
separate_arguments(FPP_ARG_LIST)

# Dedicated target for temporary intrinsic module
# TODO shift this to OMNI Compiler
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  add_custom_command(
    TARGET xmod-ieee_exceptions
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_exceptions.f90
    COMMAND ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
      ${CMAKE_CURRENT_BINARY_DIR}/ieee_exceptions.i
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_exceptions.f90
    COMMENT "Generating .xmod file for IEEE_EXCEPTIONS intrinsic module"
  )
  add_custom_command(
    TARGET xmod-ieee_arithmetic
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_arithmetic.f90
    COMMAND
      ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
      -M${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_CURRENT_BINARY_DIR}/ieee_arithmetic.i
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_arithmetic.f90
    COMMENT "Generating .xmod file for IEEE_ARITHMETIC intrinsic module"
  )
  add_custom_command(
    TARGET xmod-ieee_features
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_features.f90
    COMMAND
      ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
      ${CMAKE_CURRENT_BINARY_DIR}/ieee_features.i
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_features.f90
    COMMENT "Generating .xmod file for IEEE_ARITHMETIC intrinsic module"
  )
else()
  add_custom_command(
    TARGET xmod-ieee_exceptions
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_exceptions.f90 |
      ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front > /dev/null
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_exceptions.f90
    COMMENT "Generating .xmod file for IEEE_EXCEPTIONS intrinsic module"
  )
  add_custom_command(
    TARGET xmod-ieee_arithmetic
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_arithmetic.f90 |
      ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front
      -M${CMAKE_CURRENT_BINARY_DIR} > /dev/null
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_arithmetic.f90
    COMMENT "Generating .xmod file for IEEE_ARITHMETIC intrinsic module"
  )
  add_custom_command(
    TARGET xmod-ieee_features
    COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
      ${CMAKE_CURRENT_SOURCE_DIR}/ieee_features.f90 |
      ${CMAKE_SOURCE_DIR}/omni-compiler/F-FrontEnd/src/F_Front > /dev/null
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ieee_features.f90
    COMMENT "Generating .xmod file for IEEE_ARITHMETIC intrinsic module"
  )
endif()
add_dependencies(xmod-ieee_arithmetic xmod-ieee_exceptions)

# Install in the intrinsic mod directory
install(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/openacc.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_arithmetic.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_exceptions.xmod
    ${CMAKE_CURRENT_BINARY_DIR}/ieee_features.xmod
  DESTINATION ${CMAKE_INSTALL_PREFIX}/fincludes
)

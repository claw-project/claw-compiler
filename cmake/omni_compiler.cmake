# This file is released under terms of BSD license
# See LICENSE file for more information

if(__omni_compiler)
	return()
endif()
set(__omni_compiler YES)

#
# Generate the .xmod file for a given source file
#
function(omni_generate_xmod)
  set(oneValueArgs TARGET SOURCE DEPENDS OUTPUT)
  cmake_parse_arguments(omni_generate_xmod "" "${oneValueArgs}" "" ${ARGN})

  if(NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE})
    message(FATAL "Input file ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE} does not exists !")
  endif()

  if(${FPPFLAGS} STREQUAL "")
    message(FATAL "Pre-processor flags not set for ${CMAKE_Fortran_COMPILER_ID}")
  endif()

  add_custom_target(${omni_generate_xmod_TARGET})
  add_dependencies(generate-xmods ${omni_generate_xmod_TARGET})

  add_dependencies(${omni_generate_xmod_TARGET} create_int_install_dir)

  set(FPP_ARG_LIST ${FPPFLAGS})
  separate_arguments(FPP_ARG_LIST)

  if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
    string(REGEX REPLACE "\\.[^.]*$" "" CRAY_PP_OUTPUT ${omni_generate_xmod_SOURCE})
    add_custom_command(
      TARGET ${omni_generate_xmod_TARGET}
      COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
        ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE} > /dev/null
      COMMAND
        ${OMNI_F_FRONT} -M${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/"${CRAY_PP_OUTPUT}.i" > /dev/null
      COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/fincludes
      COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/${omni_generate_xmod_OUTPUT} ${INT_CLAW_HOME}/fincludes/
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE}
      COMMENT "Generating .xmod file for ${omni_generate_xmod_SOURCE}"
    )
  else()
    add_custom_command(
      TARGET ${omni_generate_xmod_TARGET}
      COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
        ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE} |
        ${OMNI_F_FRONT} > /dev/null
      COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/fincludes
      COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/${omni_generate_xmod_OUTPUT} ${INT_CLAW_HOME}/fincludes/
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${omni_generate_xmod_SOURCE}
      COMMENT "Generating .xmod file for ${omni_generate_xmod_SOURCE}"
    )
  endif()

  add_dependencies(${omni_generate_xmod_TARGET} ${omni_generate_xmod_DEPENDS})
endfunction()

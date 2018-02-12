if(__omni_compiler)
	return()
endif()
set(__omni_compiler YES)

function(omni_generate_xmod _target _input)
  if(NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/${_input})
    message(FATAL "Input file ${CMAKE_CURRENT_SOURCE_DIR}/${_input} does not exists !")
  endif()

  set(FPP_ARG_LIST ${FPPFLAGS})
  separate_arguments(FPP_ARG_LIST)

  if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
    string(REGEX REPLACE "\\.[^.]*$" "" CRAY_PP_OUTPUT ${_input})
    add_custom_command(
      TARGET ${_target}
      COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
        ${CMAKE_CURRENT_SOURCE_DIR}/${_input}
      COMMAND
        ${OMNI_F_FRONT} -M${CMAKE_CURRENT_BINARY_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/"${CRAY_PP_OUTPUT}.i"
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_input}
      COMMENT "Generating .xmod file for ${_input}"
    )
  else()
    add_custom_command(
      TARGET ${_target}
      COMMAND ${CMAKE_Fortran_COMPILER} ${FPP_ARG_LIST}
        ${CMAKE_CURRENT_SOURCE_DIR}/${_input} |
        ${OMNI_F_FRONT} > /dev/null
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_input}
      COMMENT "Generating .xmod file for ${_input}"
    )
  endif()
endfunction()

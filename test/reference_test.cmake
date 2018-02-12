# This file is released under terms of BSD license
# See LICENSE file for more information

if(__reference_test)
	return()
endif()
set(__reference_test YES)


function(add_basic_test)
  set(options DEBUG COMPILE COMPARE IGNORE)
  set(oneValueArgs NAME ORIGINAL TRANSFORMED REFERENCE WORKING_DIRECTORY CLAW_FLAGS)
  set(multiValueArgs TARGETS CONFIGURATIONS)
  cmake_parse_arguments(add_basic_test "${options}" "${oneValueArgs}"
    "${multiValueArgs}" ${ARGN})

  if(${add_basic_test_WORKING_DIRECTORY} STREQUAL "")
    set(add_basic_test_WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  endif()

  # Define input and output file name
  if("${add_basic_test_ORIGINAL}" STREQUAL "")
    set(original_file ${add_basic_test_WORKING_DIRECTORY}/original_code.f90)
  else()
    set(original_file ${add_basic_test_WORKING_DIRECTORY}/${add_basic_test_ORIGINAL})
  endif()

  if("${add_basic_test_TRANSFORMED}" STREQUAL "")
    set(output_file ${add_basic_test_WORKING_DIRECTORY}/transformed_code.f90)
  else()
    set(output_file ${add_basic_test_WORKING_DIRECTORY}/${add_basic_test_TRANSFORMED})
  endif()

  if("${add_basic_test_TRANSFORMED}" STREQUAL "")
    set(reference_file ${add_basic_test_WORKING_DIRECTORY}/reference.f90)
  else()
    set(reference_file ${add_basic_test_WORKING_DIRECTORY}/${add_basic_test_REFERENCE})
  endif()

  # Define executable file name
  set(executable_original original_code_${add_basic_test_NAME})
  set(executable_transformed transformed_code_${add_basic_test_NAME})

  # Define directory for build
  set(XMOD_DIR "${CMAKE_CURRENT_BINARY_DIR}/__xmod__")
  set(OMNI_TMP_DIR "${CMAKE_CURRENT_BINARY_DIR}/__omni_tmp__")

  # Directory where OMNI Compiler xmod files will be placed
  if (NOT EXISTS ${XMOD_DIR})
    file(MAKE_DIRECTORY ${XMOD_DIR})
  endif()


  # Create intermediate representation in XcodeML Fortran format
  if(${add_basic_test_DEBUG}) # with debug option
    add_custom_command(
      OUTPUT  ${output_file}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${add_basic_test_CLAW_FLAGS} --debug-omni --debug -J ${XMOD_DIR}
        -o ${output_file} ${original_file}
      WORKING_DIRECTORY ${add_basic_test_WORKING_DIRECTORY}
      DEPENDS ${original_file}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  else() # without debug option
    add_custom_command(
      OUTPUT  ${output_file}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${add_basic_test_CLAW_FLAGS} -J ${XMOD_DIR}
        -o ${output_file} ${original_file}
      WORKING_DIRECTORY ${add_basic_test_WORKING_DIRECTORY}
      DEPENDS ${original_file}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  endif()



  if(COMPILE)
    # Target for the transformation
    add_custom_target(
      transform-${add_basic_test_NAME}
      DEPENDS ${output_file} ${executable_original} ${executable_transformed}
    )
    # Build the original code and the transformed code
    add_executable (${executable_original} EXCLUDE_FROM_ALL ${original_file})
    add_executable (${executable_transformed} EXCLUDE_FROM_ALL ${output_file})
  else()
    # Target for the transformation
    add_custom_target(
      transform-${add_basic_test_NAME}
      DEPENDS ${output_file}
    )
  endif()

  # Target to clean the generated file (Output of clawfc)
  add_custom_target(
    clean-${add_basic_test_NAME}
    COMMAND rm -f ${output_file}
  )

  # Add target to the global build/clean target
  if(TARGET ${BUILD_TEST_TARGET})
    add_dependencies(${BUILD_TEST_TARGET} transform-${add_basic_test_NAME})
  endif()

  if(TARGET ${CLEAN_TEST_TARGET})
    add_dependencies(${CLEAN_TEST_TARGET} clean-${add_basic_test_NAME})
  endif()

  if(TARGET ${BUILD_TEST_TARGET}-${TEST_SET})
    add_dependencies(${BUILD_TEST_TARGET}-${TEST_SET} transform-${add_basic_test_NAME})
  endif()

  if(TARGET ${CLEAN_TEST_TARGET}-${TEST_SET})
    add_dependencies(${CLEAN_TEST_TARGET}-${TEST_SET} clean-${add_basic_test_NAME})
  endif()

  # Define additional compilation flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")

  if(NOT add_basic_test_IGNORE)
    # Compare reference transformed code and output of the transformation
    add_test(
      NAME ast-transform-${add_basic_test_NAME}
      COMMAND
        "${CMAKE_COMMAND}"  --build ${CMAKE_BINARY_DIR}
        --target transform-${add_basic_test_NAME}
    )
    add_test(
      NAME ast-compare-${add_basic_test_NAME}
      COMMAND diff --ignore-blank-lines ${output_file} ${reference_file}
    )
    set_tests_properties(ast-compare-${add_basic_test_NAME}
      PROPERTIES DEPENDS ast-transform-${add_basic_test_NAME})

    # Compare the output of both executable
    if(add_basic_test_COMPARE)
      set(test_parameters "<(./${executable_original}) <(./${executable_transformed})")
      add_test(NAME compare-output-${add_basic_test_NAME} COMMAND bash -c "diff ${test_parameters}")
    endif()
  endif()

  # Add build directory to be removed with clean target
  set_property(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${XMOD_DIR} ${OMNI_TMP_DIR}
  )

endfunction()

macro(subdirlist result curdir)
  file(GLOB children RELATIVE ${curdir} ${curdir}/*)
  set(dirlist "")
  foreach(child ${children})
    if(IS_DIRECTORY ${curdir}/${child})
      LIST(APPEND dirlist ${child})
    endif()
  endforeach()
  set(${result} ${dirlist})
endmacro()

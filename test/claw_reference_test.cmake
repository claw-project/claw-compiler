# This file is released under terms of BSD license
# See LICENSE file for more information

#
# Set of utility function to add reference tests
#

if(__reference_test)
	return()
endif()
set(__reference_test YES)


function(claw_add_basic_test)
  set(options DEBUG COMPILE COMPARE IGNORE)
  set(oneValueArgs NAME ORIGINAL TRANSFORMED REFERENCE WORKING_DIRECTORY INPUT_DIRECTORY)
  set(multiValueArgs CLAW_FLAGS)
  cmake_parse_arguments(claw_add_basic_test "${options}" "${oneValueArgs}"
    "${multiValueArgs}" ${ARGN})

  if("${claw_add_basic_test_NAME}" STREQUAL "")
    message(FATAL_ERROR "claw_add_basic_test NAME is required")
  endif()

  if("${claw_add_basic_test_INPUT_DIRECTORY}" STREQUAL "")
    message(FATAL_ERROR "claw_add_basic_test INPUT_DIRECTORY is required")
  endif()

  if("${claw_add_basic_test_WORKING_DIRECTORY}" STREQUAL "")
    message(FATAL_ERROR "claw_add_basic_test WORKING_DIRECTORY is required")
  endif()

  file(COPY ${claw_add_basic_test_INPUT_DIRECTORY} DESTINATION ${claw_add_basic_test_WORKING_DIRECTORY}/../)

  # Define input and output file name
  if("${claw_add_basic_test_ORIGINAL}" STREQUAL "")
    set(original_file
      ${claw_add_basic_test_WORKING_DIRECTORY}/original_code.f90)
  else()
    set(original_file
      ${claw_add_basic_test_WORKING_DIRECTORY}/${claw_add_basic_test_ORIGINAL})
  endif()

  if("${claw_add_basic_test_TRANSFORMED}" STREQUAL "")
    set(output_file
      ${claw_add_basic_test_WORKING_DIRECTORY}/transformed_code.f90)
  else()
    set(output_file
      ${claw_add_basic_test_WORKING_DIRECTORY}/${claw_add_basic_test_TRANSFORMED})
  endif()

  if("${claw_add_basic_test_TRANSFORMED}" STREQUAL "")
    set(reference_file ${claw_add_basic_test_WORKING_DIRECTORY}/reference.f90)
  else()
    set(reference_file
      ${claw_add_basic_test_WORKING_DIRECTORY}/${claw_add_basic_test_REFERENCE})
  endif()

  # Define executable file name
  set(executable_original original_code_${claw_add_basic_test_NAME})
  set(executable_transformed transformed_code_${claw_add_basic_test_NAME})

  # Define directory for build
  set(XMOD_DIR "${claw_add_basic_test_WORKING_DIRECTORY}/__xmod__")
  set(OMNI_TMP_DIR "${claw_add_basic_test_WORKING_DIRECTORY}/__omni_tmp__")

  # Directory where OMNI Compiler xmod files will be placed
  if (NOT EXISTS ${XMOD_DIR})
    file(MAKE_DIRECTORY ${XMOD_DIR})
  endif()

  # Create intermediate representation in XcodeML Fortran format
  if(${claw_add_basic_test_DEBUG}) # with debug option
    add_custom_command(
      OUTPUT  ${output_file}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_basic_test_CLAW_FLAGS} --debug-omni --debug
        -J ${XMOD_DIR}
        -o ${output_file} ${original_file}
      WORKING_DIRECTORY ${claw_add_basic_test_WORKING_DIRECTORY}
      DEPENDS ${original_file}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  else() # without debug option
    add_custom_command(
      OUTPUT  ${output_file}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_basic_test_CLAW_FLAGS} -J ${XMOD_DIR}
        -o ${output_file} ${original_file}
      WORKING_DIRECTORY ${claw_add_basic_test_WORKING_DIRECTORY}
      DEPENDS ${original_file}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  endif()

  if(COMPILE)
    # Target for the transformation
    add_custom_target(
      transform-${claw_add_basic_test_NAME}
      DEPENDS ${output_file} ${executable_original} ${executable_transformed}
    )
    # Build the original code and the transformed code
    add_executable (${executable_original} EXCLUDE_FROM_ALL ${original_file})
    add_executable (${executable_transformed} EXCLUDE_FROM_ALL ${output_file})
  else()
    # Target for the transformation
    add_custom_target(
      transform-${claw_add_basic_test_NAME}
      DEPENDS ${output_file}
    )
  endif()

  # Target to clean the generated file (Output of clawfc)
  add_custom_target(
    clean-${claw_add_basic_test_NAME}
    COMMAND rm -f ${output_file}
  )

  # Add target to the global build/clean target
  if(TARGET ${BUILD_TEST_TARGET})
    add_dependencies(${BUILD_TEST_TARGET} transform-${claw_add_basic_test_NAME})
  endif()

  if(TARGET ${CLEAN_TEST_TARGET})
    add_dependencies(${CLEAN_TEST_TARGET} clean-${claw_add_basic_test_NAME})
  endif()

  if(TARGET ${BUILD_TEST_TARGET}-${TEST_SET})
    add_dependencies(${BUILD_TEST_TARGET}-${TEST_SET}
      transform-${claw_add_basic_test_NAME})
  endif()

  if(TARGET ${CLEAN_TEST_TARGET}-${TEST_SET})
    add_dependencies(${CLEAN_TEST_TARGET}-${TEST_SET}
      clean-${claw_add_basic_test_NAME})
  endif()

  # Define additional compilation flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")

  if(NOT claw_add_basic_test_IGNORE)
    # Compare reference transformed code and output of the transformation
    add_test(
      NAME ast-transform-${claw_add_basic_test_NAME}
      COMMAND
        "${CMAKE_COMMAND}"  --build ${CMAKE_BINARY_DIR}
        --target transform-${claw_add_basic_test_NAME}
    )
    add_test(
      NAME ast-compare-${claw_add_basic_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${output_file} ${reference_file}
    )
    set_tests_properties(ast-compare-${claw_add_basic_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_basic_test_NAME})

    # Compare the output of both executable
    if(claw_add_basic_test_COMPARE)
      set(test_parameters
        "<(./${executable_original}) <(./${executable_transformed})")
      add_test(
        NAME compare-output-${claw_add_basic_test_NAME}
        COMMAND bash -c "diff ${test_parameters}"
      )
    endif()
  endif()

  # Add build directory to be removed with clean target
  set_property(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${XMOD_DIR} ${OMNI_TMP_DIR}
  )
endfunction()


#
# Add a basic test for each subdirectory
#
function(claw_add_basic_test_set)
  set(oneValueArgs NAME DIRECTORY)
  cmake_parse_arguments(claw_add_basic_test_set "" "${oneValueArgs}" "" ${ARGN})

  if("${claw_add_basic_test_set_NAME}" STREQUAL "")
    message(FATAL_ERROR "claw_add_basic_test_set NAME is required")
  endif()

  if("${claw_add_basic_test_set_DIRECTORY}" STREQUAL "")
    message(FATAL_ERROR "claw_add_basic_test_set DIRECTORY is required")
  endif()

  set(TEST_SET ${claw_add_basic_test_set_NAME})

  subdirlist(tests_dirs ${claw_add_basic_test_set_DIRECTORY})
  foreach(t_name ${tests_dirs})

    if(NO_COMPILE_${t_name})
      set(test_option_compile COMPILE)
    else()
      set(test_option_compile "")
    endif()

    if(COMPARE_${t_name})
      set(test_option_compare COMPARE)
    else()
      set(test_option_compare "")
    endif()

    claw_add_basic_test(
      NAME ${TEST_SET}-${t_name}
      INPUT_DIRECTORY ${claw_add_basic_test_set_DIRECTORY}/${t_name}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${t_name}
      CLAW_FLAGS ${CLAW_FLAGS_${t_name}}
      ${test_option_compile}
      ${test_option_compare}
    )
  endforeach()
endfunction()


#
# Add a module test for each subdirectory
#
function(claw_add_advanced_test_set)
  set(oneValueArgs NAME DIRECTORY)
  set(multiValueArgs EXCLUDE)
  cmake_parse_arguments(claw_add_advanced_test_set "" "${oneValueArgs}"
    "${multiValueArgs}" ${ARGN})

  if("${claw_add_advanced_test_set_NAME}" STREQUAL "")
    message(FATAL_ERROR "claw_add_advanced_test_set NAME is required")
  endif()

  if("${claw_add_advanced_test_set_DIRECTORY}" STREQUAL "")
    message(FATAL_ERROR "claw_add_advanced_test_set DIRECTORY is required")
  endif()

  set(TEST_SET ${claw_add_advanced_test_set_NAME})

  subdirlist(tests_dirs ${claw_add_advanced_test_set_DIRECTORY})
  foreach(t_name ${tests_dirs})
    set(notskipped ON)
    if(claw_add_advanced_test_set_EXCLUDE)
      list(FIND claw_add_advanced_test_set_EXCLUDE ${t_name} _index)
      if(_index GREATER -1)
        set(notskipped OFF)
      endif()
    endif()

    if(notskipped)
      if(NO_COMPILE_${t_name})
         set(test_option_compile COMPILE)
      else()
         set(test_option_compile "")
      endif()

      if(COMPARE_${t_name})
        set(test_option_compare COMPARE)
      else()
        set(test_option_compare "")
      endif()

      claw_add_advanced_test(
        NAME ${TEST_SET}-${t_name}
        WORKING_DIRECTORY ${claw_add_advanced_test_set_DIRECTORY}/${t_name}
        CLAW_FLAGS ${CLAW_FLAGS_${t_name}}
        CLAW_FLAGS_TARGET_CPU ${CLAW_FLAGS_TARGET_CPU_${t_name}}
        CLAW_FLAGS_TARGET_ACC ${CLAW_FLAGS_TARGET_ACC_${t_name}}
        CLAW_FLAGS_TARGET_OMP ${CLAW_FLAGS_TARGET_OMP_${t_name}}
        ${test_option_compile}
        ${test_option_compare}
      )
    endif()
  endforeach()
endfunction()


function(claw_add_advanced_test)
  set(options DEBUG COMPILE COMPARE IGNORE)
  set(oneValueArgs NAME WORKING_DIRECTORY)
  set(multiValueArgs CLAW_FLAGS CLAW_FLAGS_TARGET_CPU CLAW_FLAGS_TARGET_ACC CLAW_FLAGS_TARGET_OMP)
  cmake_parse_arguments(claw_add_advanced_test "${options}" "${oneValueArgs}"
    "${multiValueArgs}" ${ARGN})

  if("${claw_add_advanced_test_NAME}" STREQUAL "")
    message(FATAL_ERROR "claw_add_advanced_test NAME is required")
  endif()

  if("${claw_add_advanced_test_WORKING_DIRECTORY}" STREQUAL "")
    set(claw_add_advanced_test_WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  endif()

  # Specifiy the output directory if the module files
  set(CMAKE_Fortran_MODULE_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY})

  # Define input and output file name
  set(MAIN_F90 ${claw_add_advanced_test_WORKING_DIRECTORY}/main.f90)
  set(ORIGINAL_FILE ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column.f90)
  set(OUTPUT_FILE_CPU
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_cpu.f90)
  set(OUTPUT_FILE_ACC
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_acc.f90)
  set(OUTPUT_FILE_OMP
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_omp.f90)
  set(REFERENCE_FILE_CPU
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_cpu.f90)
  set(REFERENCE_FILE_ACC
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_acc.f90)
  set(REFERENCE_FILE_OMP
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_omp.f90)
  set(OUTPUT_MAIN_CPU
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_main_cpu.f90)
  set(OUTPUT_MAIN_ACC
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_main_acc.f90)
  set(OUTPUT_MAIN_OMP
    ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_main_omp.f90)
  set(REFERENCE_MAIN_CPU
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_main_cpu.f90)
  set(REFERENCE_MAIN_ACC
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_main_acc.f90)
  set(REFERENCE_MAIN_OMP
    ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_main_omp.f90)

  # If the test case has an extra module.
  if(EXISTS ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column_extra.f90)
    set(ORIGINAL_FILE_EXTRA
      ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column_extra.f90)
    set(OUTPUT_FILE_EXTRA_CPU
      ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_extra_cpu.f90)
    set(OUTPUT_FILE_EXTRA_ACC
      ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_extra_acc.f90)
    set(OUTPUT_FILE_EXTRA_OMP
      ${claw_add_advanced_test_WORKING_DIRECTORY}/transformed_code_extra_omp.f90)
    set(REFERENCE_FILE_EXTRA_CPU
      ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_extra_cpu.f90)
    set(REFERENCE_FILE_EXTRA_ACC
      ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_extra_acc.f90)
    set(REFERENCE_FILE_EXTRA_OMP
      ${claw_add_advanced_test_WORKING_DIRECTORY}/reference_extra_omp.f90)
  endif()

  # Define executable file name
  set(EXECUTABLE_ORIGINAL original_code_${claw_add_advanced_test_NAME})
  set(EXECUTABLE_TRANSFORMED_CPU transformed_code_cpu_${claw_add_advanced_test_NAME})
  set(EXECUTABLE_TRANSFORMED_ACC transformed_code_acc_${claw_add_advanced_test_NAME})
  set(EXECUTABLE_TRANSFORMED_OMP transformed_code_omp_${claw_add_advanced_test_NAME})

  # Define directory for build
  set(XMOD_DIR ${claw_add_advanced_test_WORKING_DIRECTORY}/__xmod__)
  set(OMNI_TMP_DIR ${claw_add_advanced_test_WORKING_DIRECTORY}/__omni_tmp__)

  # Directory where OMNI xmod files will be placed
  if (NOT EXISTS ${XMOD_DIR})
    file(MAKE_DIRECTORY ${XMOD_DIR})
  endif()

  # Create intermediate representation in XcodeML Fortran format
  if(${DEBUG}) # with debug option
    set(DEBUG_FLAG --debug --debug-omni)
  endif()

  # Execute the CLAW Compiler for CPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE_CPU}
    COMMAND touch ${ORIGINAL_FILE} # to force new compilation
    COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
      ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=cpu
      ${claw_add_advanced_test_CLAW_FLAGS_TARGET_CPU}
      ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_CPU} ${ORIGINAL_FILE}
    WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
    DEPENDS ${ORIGINAL_FILE}
    COMMENT "${CLAWFC} for CPU target on file ${ORIGINAL_FILE}"
  )

  # Execute the CLAW Compiler for GPU target with ACC directive
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE_ACC}
    COMMAND touch ${ORIGINAL_FILE} # to force new compilation
    COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
      ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openacc
      ${claw_add_advanced_test_CLAW_FLAGS_TARGET_ACC}
      ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_ACC} ${ORIGINAL_FILE}
    WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
    DEPENDS ${ORIGINAL_FILE}
    COMMENT "${CLAWFC} for GPU target with ACC directive on file ${ORIGINAL_FILE}"
  )

  # Execute the CLAW Compiler for GPU target with OMP directive
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE_OMP}
    COMMAND touch ${ORIGINAL_FILE} # to force new compilation
    COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
      ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openmp
      ${claw_add_advanced_test_CLAW_FLAGS_TARGET_OMP}
      ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_OMP} ${ORIGINAL_FILE}
    WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
    DEPENDS ${ORIGINAL_FILE}
    COMMENT "${CLAWFC} for GPU target with OMP directive on file ${ORIGINAL_FILE}"
  )

  if(EXISTS ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column_extra.f90)
    # Execute the CLAW Compiler for CPU target
    add_custom_command(
      OUTPUT  ${OUTPUT_FILE_EXTRA_CPU}
      COMMAND touch ${ORIGINAL_FILE_EXTRA} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=cpu
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_CPU}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_EXTRA_CPU}
        ${ORIGINAL_FILE_EXTRA}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${ORIGINAL_FILE_EXTRA} ${OUTPUT_FILE_CPU}
      COMMENT "${CLAWFC} for CPU target on file ${ORIGINAL_FILE_EXTRA}"
    )

    # Execute the CLAW Compiler for GPU target with ACC directive
    add_custom_command(
      OUTPUT  ${OUTPUT_FILE_EXTRA_ACC}
      COMMAND touch ${ORIGINAL_FILE_EXTRA} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openacc
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_ACC}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_EXTRA_ACC}
        ${ORIGINAL_FILE_EXTRA}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${ORIGINAL_FILE_EXTRA} ${OUTPUT_FILE_ACC}
      COMMENT "${CLAWFC} for GPU target with ACC directive on file ${ORIGINAL_FILE_EXTRA}"
    )

    # Execute the CLAW Compiler for GPU target with OMP directive
    add_custom_command(
      OUTPUT  ${OUTPUT_FILE_EXTRA_OMP}
      COMMAND touch ${ORIGINAL_FILE_EXTRA} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openmp
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_OMP}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_EXTRA_OMP}
        ${ORIGINAL_FILE_EXTRA}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${ORIGINAL_FILE_EXTRA} ${OUTPUT_FILE_OMP}
      COMMENT "${CLAWFC} for GPU target with OMP directive on file ${ORIGINAL_FILE_EXTRA}"
    )

    # Execute the CLAW Compiler for CPU target
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_CPU}
      #COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=cpu
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_CPU}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_CPU} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_EXTRA_CPU}
      COMMENT "${CLAWFC} for CPU target on file ${MAIN_F90}"
    )

    # Execute the CLAW Compiler for GPU target with ACC directive
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_ACC}
      COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openacc
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_ACC}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_ACC} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_ACC} ${OUTPUT_FILE_EXTRA_ACC}
      COMMENT "${CLAWFC} for GPU target with ACC directive on file ${MAIN_F90}"
    )

    # Execute the CLAW Compiler for GPU target with OMP directive
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_OMP}
      COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openmp
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_OMP}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_OMP} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_OMP} ${OUTPUT_FILE_EXTRA_OMP}
      COMMENT "${CLAWFC} for GPU target with OMP directive on file ${MAIN_F90}"
    )
  else()

    # Execute the CLAW Compiler for CPU target
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_CPU}
      #COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=cpu
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_CPU}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_CPU} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_CPU}
      COMMENT "${CLAWFC} for CPU target on file ${MAIN_F90}"
    )

    # Execute the CLAW Compiler for GPU target with ACC directive
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_ACC}
      #COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openacc
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_ACC}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_ACC} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_ACC}
      COMMENT "${CLAWFC} for GPU target with ACC directive on file ${MAIN_F90}"
    )

    # Execute the CLAW Compiler for GPU target with OMP directive
    add_custom_command(
      OUTPUT  ${OUTPUT_MAIN_OMP}
      #COMMAND touch ${MAIN_F90} # to force new compilation
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_advanced_test_CLAW_FLAGS} --target=gpu --directive=openmp
        ${claw_add_advanced_test_CLAW_FLAGS_TARGET_OMP}
        ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_OMP} ${MAIN_F90}
      WORKING_DIRECTORY ${claw_add_advanced_test_WORKING_DIRECTORY}
      DEPENDS ${MAIN_F90} ${OUTPUT_FILE_OMP}
      COMMENT "${CLAWFC} for GPU target with OMP directive on file ${MAIN_F90}"
    )
  endif()

  # Target to clean the generated file (Output of clawfc)
  add_custom_target(
    clean-${claw_add_advanced_test_NAME}
    COMMAND rm -f
    ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_ACC} ${OUTPUT_FILE_OMP}
    ${OUTPUT_MAIN_CPU} ${OUTPUT_MAIN_ACC} ${OUTPUT_MAIN_OMP}
    ${OUTPUT_FILE_EXTRA_CPU} ${OUTPUT_FILE_EXTRA_ACC} ${OUTPUT_FILE_EXTRA_OMP}
    ${XMOD_DIR}/*
  )

  # Add target to the global build/clean target
  add_dependencies(${BUILD_TEST_TARGET}
    transform-${claw_add_advanced_test_NAME})
  add_dependencies(${CLEAN_TEST_TARGET}
    clean-${claw_add_advanced_test_NAME})
  add_dependencies(${BUILD_TEST_TARGET}-${TEST_SET}
    transform-${claw_add_advanced_test_NAME})
  add_dependencies(${CLEAN_TEST_TARGET}-${TEST_SET}
    clean-${claw_add_advanced_test_NAME})

  # Define additional compilation flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")

  # Build of the executables is different if there is one or two modules
  if(EXISTS ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column_extra.f90)
    # Build the original code and the transformed code
    add_executable(${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL
      "${ORIGINAL_FILE}" "${ORIGINAL_FILE_EXTRA}" "${MAIN_F90}")
    add_executable(${EXECUTABLE_TRANSFORMED_CPU} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_CPU}" "${OUTPUT_FILE_EXTRA_CPU}" "${OUTPUT_MAIN_CPU}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_CPU} PRIVATE -D_CLAW)
    add_executable(${EXECUTABLE_TRANSFORMED_ACC} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_ACC}" "${OUTPUT_FILE_EXTRA_ACC}" "${OUTPUT_MAIN_ACC}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_ACC} PRIVATE -D_CLAW)
    add_executable(${EXECUTABLE_TRANSFORMED_OMP} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_OMP}" "${OUTPUT_FILE_EXTRA_OMP}" "${OUTPUT_MAIN_OMP}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_OMP} PRIVATE -D_CLAW)
  else()
    # Build the original code and the transformed code
    add_executable(${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL
      "${ORIGINAL_FILE}" "${MAIN_F90}")
    add_executable(${EXECUTABLE_TRANSFORMED_CPU} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_CPU}" "${OUTPUT_MAIN_CPU}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_CPU} PRIVATE -D_CLAW)
    add_executable(${EXECUTABLE_TRANSFORMED_ACC} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_ACC}" "${OUTPUT_MAIN_ACC}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_ACC} PRIVATE -D_CLAW)
    add_executable(${EXECUTABLE_TRANSFORMED_OMP} EXCLUDE_FROM_ALL
      "${OUTPUT_FILE_OMP}" "${OUTPUT_MAIN_OMP}")
    target_compile_definitions(${EXECUTABLE_TRANSFORMED_OMP} PRIVATE -D_CLAW)
  endif()

  # Target for the transformation
  add_custom_target(
    transform-${claw_add_advanced_test_NAME}
    DEPENDS ${EXECUTABLE_TRANSFORMED_CPU} ${EXECUTABLE_TRANSFORMED_ACC} ${EXECUTABLE_TRANSFORMED_OMP}
  )

  add_dependencies(${EXECUTABLE_TRANSFORMED_ACC} ${EXECUTABLE_ORIGINAL})
  add_dependencies(${EXECUTABLE_TRANSFORMED_OMP} ${EXECUTABLE_ORIGINAL})
  add_dependencies(${EXECUTABLE_TRANSFORMED_CPU} ${EXECUTABLE_ORIGINAL})
  add_dependencies(${EXECUTABLE_TRANSFORMED_CPU} ${EXECUTABLE_TRANSFORMED_ACC})
  add_dependencies(${EXECUTABLE_TRANSFORMED_ACC} ${EXECUTABLE_TRANSFORMED_OMP})

  # Set target specific compilation options
  if(OPENACC_ENABLE)
    target_compile_options(${EXECUTABLE_TRANSFORMED_ACC} PUBLIC
      ${OPENACC_FLAGS})
    target_link_libraries(${EXECUTABLE_TRANSFORMED_ACC} ${OPENACC_FLAGS})
  elseif(OPENMP_ENABLE)
    target_compile_options(${EXECUTABLE_TRANSFORMED_OMP} PUBLIC
      ${OPENMP_FLAGS})
  else()
    target_compile_options(${EXECUTABLE_TRANSFORMED_OMP} PUBLIC
      ${TEST_BASE_FLAGS})
  endif()

  if(NOT IGNORE)
    # Compare reference transformed code and output of the transformation
    add_test(NAME ast-compare-cpu-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_CPU}
      ${REFERENCE_FILE_CPU})
    add_test(NAME ast-compare-acc-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_ACC}
      ${REFERENCE_FILE_ACC})
    add_test(NAME ast-compare-omp-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_OMP}
      ${REFERENCE_FILE_OMP})
    add_test(NAME ast-compare-main-cpu-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_MAIN_CPU}
      ${REFERENCE_MAIN_CPU})
    add_test(NAME ast-compare-main-acc-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_MAIN_ACC}
      ${REFERENCE_MAIN_ACC})
    add_test(NAME ast-compare-main-omp-${claw_add_advanced_test_NAME}
      COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_MAIN_OMP}
      ${REFERENCE_MAIN_OMP})
    set_tests_properties(ast-compare-cpu-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    set_tests_properties(ast-compare-acc-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    set_tests_properties(ast-compare-omp-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    set_tests_properties(ast-compare-main-cpu-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    set_tests_properties(ast-compare-main-acc-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    set_tests_properties(ast-compare-main-omp-${claw_add_advanced_test_NAME}
      PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})

    # Test the extra module file with its reference
    if(EXISTS ${claw_add_advanced_test_WORKING_DIRECTORY}/mo_column_extra.f90)
      add_test(NAME ast-compare-cpu-extra-${claw_add_advanced_test_NAME}
        COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_EXTRA_CPU}
        ${REFERENCE_FILE_EXTRA_CPU})
      add_test(NAME ast-compare-acc-extra-${claw_add_advanced_test_NAME}
        COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_EXTRA_ACC}
        ${REFERENCE_FILE_EXTRA_ACC})
      add_test(NAME ast-compare-omp-extra-${claw_add_advanced_test_NAME}
        COMMAND diff --ignore-all-space --ignore-blank-lines ${OUTPUT_FILE_EXTRA_OMP}
        ${REFERENCE_FILE_EXTRA_OMP})
      set_tests_properties(ast-compare-cpu-extra-${claw_add_advanced_test_NAME}
        PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
      set_tests_properties(ast-compare-acc-extra-${claw_add_advanced_test_NAME}
        PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
      set_tests_properties(ast-compare-omp-extra-${claw_add_advanced_test_NAME}
        PROPERTIES DEPENDS ast-transform-${claw_add_advanced_test_NAME})
    endif()

    # Compare the output of both executable
    if(claw_add_advanced_test_COMPARE)
      set(TEST_PARAMETERS_CPU
        "<(./${EXECUTABLE_ORIGINAL}) <(./${EXECUTABLE_TRANSFORMED_CPU})")
      add_test(NAME compare-output-cpu-${claw_add_advanced_test_NAME}
        COMMAND bash -c "diff ${TEST_PARAMETERS_CPU}")
      set(TEST_PARAMETERS_ACC
        "<(./${EXECUTABLE_ORIGINAL}) <(./${EXECUTABLE_TRANSFORMED_ACC})")
      add_test(NAME compare-output-acc-${claw_add_advanced_test_NAME}
        COMMAND bash -c "diff ${TEST_PARAMETERS_ACC}")
      set(TEST_PARAMETERS_OMP
        "<(./${EXECUTABLE_ORIGINAL}) <(./${EXECUTABLE_TRANSFORMED_OMP})")
      add_test(NAME compare-output-omp-${claw_add_advanced_test_NAME}
        COMMAND bash -c "diff ${TEST_PARAMETERS_OMP}")
    endif()
  endif()

  # Add build directory to be removed with clean target
  set_property(
    DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${XMOD_DIR} ${OMNI_TMP_DIR}
  )
endfunction()

function(claw_add_failure_test)
  set(options DEBUG)
  set(oneValueArgs NAME WORKING_DIRECTORY CLAW_TARGET CLAW_DIRECTIVE SET)
  set(multiValueArgs CLAW_FLAGS)
  cmake_parse_arguments(claw_add_failure_test "${options}" "${oneValueArgs}"
    "${multiValueArgs}" ${ARGN})

  if("${claw_add_failure_test_NAME}" STREQUAL "")
    message(FATAL_ERROR "claw_add_failure_test NAME is required")
  endif()

  if("${claw_add_failure_test_WORKING_DIRECTORY}" STREQUAL "")
    set(claw_add_failure_test_WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
  endif()

  if(claw_add_failure_test_CLAW_TARGET)
    set(claw_add_failure_test_CLAW_TARGET
      "--target=${claw_add_failure_test_CLAW_TARGET}")
  endif()

  if(claw_add_failure_test_CLAW_DIRECTIVE)
    set(claw_add_failure_test_CLAW_DIRECTIVE
      "--directive=${claw_add_failure_test_CLAW_DIRECTIVE}")
  endif()

  # Define input and output file name
  set(ORIGINAL_FILE
    ${claw_add_failure_test_WORKING_DIRECTORY}/original_code.f90)
  set(OUTPUT_FILE
    ${claw_add_failure_test_WORKING_DIRECTORY}/transformed_code.f90)

  # Define executable file name
  set(EXECUTABLE_ORIGINAL original_code_${claw_add_failure_test_NAME})

  # Define directory for build
  set(XMOD_DIR ${claw_add_failure_test_WORKING_DIRECTORY}/__xmod__)
  set(OMNI_TMP_DIR ${claw_add_failure_test_WORKING_DIRECTORY}/__omni_tmp__)

  # Directory where OMNI xmod files will be placed
  if (NOT EXISTS ${XMOD_DIR})
    file(MAKE_DIRECTORY ${XMOD_DIR})
  endif()

  # Create intermediate representation in XcodeML Fortran format
  if(DEBUG) # with debug option
    add_custom_command(
      OUTPUT  ${OUTPUT_FILE}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_failure_test_CLAW_FLAGS} --debug-omni --debug
        -J ${XMOD_DIR} ${claw_add_failure_test_CLAW_TARGET}
        ${claw_add_failure_test_CLAW_DIRECTIVE} -o ${OUTPUT_FILE}
        ${ORIGINAL_FILE}
      WORKING_DIRECTORY ${claw_add_failure_test_WORKING_DIRECTORY}
      DEPENDS ${ORIGINAL_FILE}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  else() # without debug option
    add_custom_command(
      OUTPUT  ${OUTPUT_FILE}
      COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
        ${CLAWFC} ${claw_add_failure_test_CLAW_FLAGS} -J ${XMOD_DIR}
        ${claw_add_failure_test_CLAW_TARGET}
        ${claw_add_failure_test_CLAW_DIRECTIVE} -o ${OUTPUT_FILE}
        ${ORIGINAL_FILE}
      WORKING_DIRECTORY ${claw_add_failure_test_WORKING_DIRECTORY}
      DEPENDS ${ORIGINAL_FILE}
      COMMENT "Translating CLAW directive with ${CLAWFC}"
    )
  endif()

  add_custom_target(
    transform-${claw_add_failure_test_NAME}
    DEPENDS ${OUTPUT_FILE}
  )

  # Build the original code and the transformed code
  add_executable (${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL ${ORIGINAL_FILE})

  # Target to clean the generated file (Output of clawfc)
  add_custom_target(
    clean-${claw_add_failure_test_NAME}
    COMMAND rm -f ${OUTPUT_FILE}
  )

  # Add target to the global build/clean target
  add_dependencies(${CLEAN_TEST_TARGET} clean-${claw_add_failure_test_NAME})
  if(${claw_add_failure_test_SET})
    add_dependencies(${CLEAN_TEST_TARGET}-${claw_add_failure_test_SET}
      clean-${claw_add_failure_test_NAME})
  endif()

  # Define additional compilation flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")
  add_test(
    NAME transform-failure-${claw_add_failure_test_NAME}
    COMMAND "${CMAKE_COMMAND}"  --build ${CMAKE_BINARY_DIR}
    --target transform-${claw_add_failure_test_NAME}
  )
  # Check that the command fails
  set_tests_properties(transform-failure-${claw_add_failure_test_NAME}
    PROPERTIES WILL_FAIL TRUE)

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
    if(IS_DIRECTORY ${curdir}/${child} AND NOT "${child}" STREQUAL "CMakeFiles"
      AND NOT "${child}" STREQUAL "__xmod__")
      list(APPEND dirlist ${child})
    endif()
  endforeach()
  set(${result} ${dirlist})
endmacro()

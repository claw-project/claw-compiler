# Base test definition for test case with a main file and one or two modules.
# Perform code transformation and compare the output with a reference

# Define input and output file name
set (MAIN_F90 ${CMAKE_CURRENT_SOURCE_DIR}/main.f90)
set (ORIGINAL_FILE ${CMAKE_CURRENT_SOURCE_DIR}/mo_column.f90)
set (OUTPUT_FILE_CPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_cpu.f90)
set (OUTPUT_FILE_GPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_gpu.f90)
set (REFERENCE_FILE_CPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_cpu.f90)
set (REFERENCE_FILE_GPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_gpu.f90)
set (OUTPUT_MAIN_CPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_main_cpu.f90)
set (OUTPUT_MAIN_GPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_main_gpu.f90)
set (REFERENCE_MAIN_CPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_main_cpu.f90)
set (REFERENCE_MAIN_GPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_main_gpu.f90)

# If the test case has an extra module.
if(HAS_EXTRA_MOD)
  set (ORIGINAL_FILE_EXTRA ${CMAKE_CURRENT_SOURCE_DIR}/mo_column_extra.f90)
  set (OUTPUT_FILE_EXTRA_CPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_extra_cpu.f90)
  set (OUTPUT_FILE_EXTRA_GPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_extra_gpu.f90)
  set (REFERENCE_FILE_EXTRA_CPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_extra_cpu.f90)
  set (REFERENCE_FILE_EXTRA_GPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_extra_gpu.f90)
endif()

# Define executable file name
set (EXECUTABLE_ORIGINAL original_code_${TEST_NAME})
set (EXECUTABLE_TRANSFORMED_CPU transformed_code_cpu_${TEST_NAME})
set (EXECUTABLE_TRANSFORMED_GPU transformed_code_gpu_${TEST_NAME})

# Define directory for build
set (XMOD_DIR __xmod__)
set (OMNI_TMP_DIR __omni_tmp__)

# Directory where OMNI xmod files will be placed
if (NOT EXISTS ${XMOD_DIR})
  file(MAKE_DIRECTORY ${XMOD_DIR})
endif()

# Create intermediate representation in XcodeML Fortran format
if(${TEST_DEBUG}) # with debug option
  set(DEBUG_FLAG --debug)
endif()

# Execute the CLAW compiler for CPU target
add_custom_command(
  OUTPUT  ${OUTPUT_FILE_CPU}
  COMMAND touch ${ORIGINAL_FILE} # to force new compilation
  COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=cpu ${DIRECTIVE_CPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_CPU} ${ORIGINAL_FILE}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Translating CLAW directive with ${CLAWFC} for CPU target on file ${ORIGINAL_FILE}"
)

# Execute the CLAW compiler for GPU target
add_custom_command(
  OUTPUT  ${OUTPUT_FILE_GPU}
  COMMAND touch ${ORIGINAL_FILE} # to force new compilation
  COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=gpu ${DIRECTIVE_GPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_GPU} ${ORIGINAL_FILE}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Translating CLAW directive with ${CLAWFC} for GPU target on file ${ORIGINAL_FILE}"
)

if(HAS_EXTRA_MOD)
  # Execute the CLAW compiler for CPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE_EXTRA_CPU}
    COMMAND touch ${ORIGINAL_FILE_EXTRA} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=cpu ${DIRECTIVE_CPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_EXTRA_CPU} ${ORIGINAL_FILE_EXTRA}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${ORIGINAL_FILE_EXTRA} ${OUTPUT_FILE_CPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for CPU target on file ${ORIGINAL_FILE_EXTRA}"
  )

  # Execute the CLAW compiler for GPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE_EXTRA_GPU}
    COMMAND touch ${ORIGINAL_FILE_EXTRA} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=gpu ${DIRECTIVE_GPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_EXTRA_GPU} ${ORIGINAL_FILE_EXTRA}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${ORIGINAL_FILE_EXTRA} ${OUTPUT_FILE_GPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for GPU target on file ${ORIGINAL_FILE_EXTRA}"
  )

  # Execute the CLAW compiler for CPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_MAIN_CPU}
    COMMAND touch ${MAIN_F90} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=cpu ${DIRECTIVE_CPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_CPU} ${MAIN_F90}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${MAIN_F90} ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_EXTRA_CPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for CPU target on file ${MAIN_F90}"
  )

  # Execute the CLAW compiler for GPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_MAIN_GPU}
    COMMAND touch ${MAIN_F90} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=gpu ${DIRECTIVE_GPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_GPU} ${MAIN_F90}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${MAIN_F90} ${OUTPUT_FILE_GPU} ${OUTPUT_FILE_EXTRA_GPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for GPU target on file ${MAIN_F90}"
  )

  # Target for the transformation
  add_custom_target(
    transform-${TEST_NAME}
    DEPENDS
    ${OUTPUT_MAIN_CPU} ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_EXTRA_CPU} ${OUTPUT_MAIN_GPU}
    ${OUTPUT_FILE_GPU} ${OUTPUT_FILE_EXTRA_GPU} ${EXECUTABLE_ORIGINAL}
    ${EXECUTABLE_TRANSFORMED_CPU} ${EXECUTABLE_TRANSFORMED_GPU}
  )

else()

  # Execute the CLAW compiler for CPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_MAIN_CPU}
    COMMAND touch ${MAIN_F90} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=cpu ${DIRECTIVE_CPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_CPU} ${MAIN_F90}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${MAIN_F90} ${OUTPUT_FILE_CPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for CPU target on file ${MAIN_F90}"
  )

  # Execute the CLAW compiler for GPU target
  add_custom_command(
    OUTPUT  ${OUTPUT_MAIN_GPU}
    COMMAND touch ${MAIN_F90} # to force new compilation
    COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=gpu ${DIRECTIVE_GPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_MAIN_GPU} ${MAIN_F90}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${MAIN_F90} ${OUTPUT_FILE_GPU}
    COMMENT "Translating CLAW directive with ${CLAWFC} for GPU target on file ${MAIN_F90}"
  )

  # Target for the transformation
  add_custom_target(
    transform-${TEST_NAME}
    DEPENDS
    ${OUTPUT_MAIN_CPU} ${OUTPUT_FILE_CPU} ${OUTPUT_MAIN_GPU} ${OUTPUT_FILE_GPU}
    ${EXECUTABLE_ORIGINAL} ${EXECUTABLE_TRANSFORMED_CPU}
    ${EXECUTABLE_TRANSFORMED_GPU}
  )

endif()

# Target to clean the generated file (Output of clawfc)
add_custom_target(
  clean-${TEST_NAME}
  COMMAND rm -f
  ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_GPU}
  ${OUTPUT_MAIN_CPU} ${OUTPUT_MAIN_GPU}
  ${OUTPUT_FILE_EXTRA_CPU} ${OUTPUT_FILE_EXTRA_GPU}
  ${XMOD_DIR}/*
)

# Add target to the global build/clean target
add_dependencies(${BUILD_TEST_TARGET} transform-${TEST_NAME})
add_dependencies(${CLEAN_TEST_TARGET} clean-${TEST_NAME})
add_dependencies(${BUILD_TEST_TARGET}-${TEST_SET} transform-${TEST_NAME})
add_dependencies(${CLEAN_TEST_TARGET}-${TEST_SET} clean-${TEST_NAME})

# Define additional compilation flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")

# Build of the executables is different if there is one or two modules
if(HAS_EXTRA_MOD)

  # Build the original code and the transformed code
  add_executable (${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL "${ORIGINAL_FILE}" "${ORIGINAL_FILE_EXTRA}" "${MAIN_F90}")
  add_executable (${EXECUTABLE_TRANSFORMED_CPU} EXCLUDE_FROM_ALL "${OUTPUT_FILE_CPU}" "${OUTPUT_FILE_EXTRA_CPU}" "${OUTPUT_MAIN_CPU}")
  target_compile_definitions(${EXECUTABLE_TRANSFORMED_CPU} PRIVATE -D_CLAW)
  add_executable (${EXECUTABLE_TRANSFORMED_GPU} EXCLUDE_FROM_ALL "${OUTPUT_FILE_GPU}" "${OUTPUT_FILE_EXTRA_GPU}" "${OUTPUT_MAIN_GPU}")
  target_compile_definitions(${EXECUTABLE_TRANSFORMED_GPU} PRIVATE -D_CLAW)

else()

  # Build the original code and the transformed code
  add_executable (${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL "${ORIGINAL_FILE}" "${MAIN_F90}")
  add_executable (${EXECUTABLE_TRANSFORMED_CPU} EXCLUDE_FROM_ALL "${OUTPUT_FILE_CPU}" "${OUTPUT_MAIN_CPU}")
  target_compile_definitions(${EXECUTABLE_TRANSFORMED_CPU} PRIVATE -D_CLAW)
  add_executable (${EXECUTABLE_TRANSFORMED_GPU} EXCLUDE_FROM_ALL "${OUTPUT_FILE_GPU}" "${OUTPUT_MAIN_GPU}")
  target_compile_definitions(${EXECUTABLE_TRANSFORMED_GPU} PRIVATE -D_CLAW)

endif()

# Set target specific compilation options
if(OPENACC_ENABLE)
  target_compile_options(${EXECUTABLE_TRANSFORMED_GPU} PUBLIC ${OPENACC_FLAGS})
  target_link_libraries(${EXECUTABLE_TRANSFORMED_GPU} ${OPENACC_FLAGS})
elseif(OPENMP_ENABLE)
  target_compile_options(${EXECUTABLE_TRANSFORMED_GPU} PUBLIC ${OPENMP_FLAGS})
else()
  target_compile_options(${EXECUTABLE_TRANSFORMED_GPU} PUBLIC ${TEST_BASE_FLAGS})
endif()

if(NOT IGNORE_TEST)
  # Compare reference transformed code and output of the transformation
  add_test(NAME ast-compare-cpu-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_CPU} ${REFERENCE_FILE_CPU})
  add_test(NAME ast-compare-gpu-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_GPU} ${REFERENCE_FILE_GPU})
  add_test(NAME ast-compare-main-cpu-${TEST_NAME} COMMAND diff ${OUTPUT_MAIN_CPU} ${REFERENCE_MAIN_CPU})
  add_test(NAME ast-compare-main-gpu-${TEST_NAME} COMMAND diff ${OUTPUT_MAIN_GPU} ${REFERENCE_MAIN_GPU})
  set_tests_properties(ast-compare-cpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
  set_tests_properties(ast-compare-gpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
  set_tests_properties(ast-compare-main-cpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
  set_tests_properties(ast-compare-main-gpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})

  # Test the extra module file with its reference
  if(HAS_EXTRA_MOD)
    add_test(NAME ast-compare-cpu-extra-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_EXTRA_CPU} ${REFERENCE_FILE_EXTRA_CPU})
    add_test(NAME ast-compare-gpu-extra-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_EXTRA_GPU} ${REFERENCE_FILE_EXTRA_GPU})
    set_tests_properties(ast-compare-cpu-extra-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
    set_tests_properties(ast-compare-gpu-extra-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
  endif()

  # Compare the output of both executable
  if(OUTPUT_TEST)
    set (TEST_PARAMETERS_CPU "<(./${EXECUTABLE_ORIGINAL}) <(./${EXECUTABLE_TRANSFORMED_CPU})")
    add_test(NAME compare-output-cpu-${TEST_NAME} COMMAND bash -c "diff ${TEST_PARAMETERS_CPU}")
    set (TEST_PARAMETERS_GPU "<(./${EXECUTABLE_ORIGINAL}) <(./${EXECUTABLE_TRANSFORMED_GPU})")
    add_test(NAME compare-output-gpu-${TEST_NAME} COMMAND bash -c "diff ${TEST_PARAMETERS_GPU}")
  endif()
endif()

# Add build directory to be removed with clean target
set_property(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${XMOD_DIR} ${OMNI_TMP_DIR}
)

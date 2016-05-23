# Base test definition
# Perform code transformation and compare the output with a reference

# Define input and output file name
set (ORIGINAL_FILE ${CMAKE_CURRENT_SOURCE_DIR}/mo_column.f90)
set (OUTPUT_FILE_CPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_cpu.f90)
set (OUTPUT_FILE_GPU ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code_gpu.f90)
set (REFERENCE_FILE_CPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_cpu.f90)
set (REFERENCE_FILE_GPU ${CMAKE_CURRENT_SOURCE_DIR}/reference_gpu.f90)

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
  COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=cpu ${DIRECTIVE_CPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_CPU} ${ORIGINAL_FILE}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Translating CLAW directive with ${CLAWFC} for CPU target"
)

# Execute the CLAW compiler for GPU target
add_custom_command(
  OUTPUT  ${OUTPUT_FILE_GPU}
  COMMAND ${CLAWFC} ${OPTIONAL_FLAGS} --target=gpu ${DIRECTIVE_GPU} ${DEBUG_FLAG} -J ${XMOD_DIR} -o ${OUTPUT_FILE_GPU} ${ORIGINAL_FILE}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Translating CLAW directive with ${CLAWFC} for GPU target"
)

# Target for the transformation
add_custom_target(
  transform-${TEST_NAME}
  DEPENDS ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_GPU} ${EXECUTABLE_ORIGINAL} ${EXECUTABLE_TRANSFORMED_CPU} ${EXECUTABLE_TRANSFORMED_GPU}
)

# Target to clean the generated file (Output of clawfc)
add_custom_target(
  clean-${TEST_NAME}
  COMMAND rm -f ${OUTPUT_FILE_CPU} ${OUTPUT_FILE_GPU}
)

# Add target to the global build/clean target
add_dependencies(${BUILD_TEST_TARGET} transform-${TEST_NAME})
add_dependencies(${CLEAN_TEST_TARGET} clean-${TEST_NAME})

# Define additional compilation flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")

# Build the original code and the transformed code
add_executable (${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL ${ORIGINAL_FILE} main.f90)
add_executable (${EXECUTABLE_TRANSFORMED_CPU} EXCLUDE_FROM_ALL ${OUTPUT_FILE_CPU} main.f90)
target_compile_definitions(${EXECUTABLE_TRANSFORMED_CPU} PRIVATE -D_CLAW)
add_executable (${EXECUTABLE_TRANSFORMED_GPU} EXCLUDE_FROM_ALL ${OUTPUT_FILE_GPU} main.f90)
target_compile_definitions(${EXECUTABLE_TRANSFORMED_GPU} PRIVATE -D_CLAW)

if(NOT IGNORE_TEST)
  # Compare reference transformed code and output of the transformation
  add_test(NAME ast-transform-${TEST_NAME} COMMAND "${CMAKE_COMMAND}"  --build ${CMAKE_BINARY_DIR} --target transform-${TEST_NAME})
  add_test(NAME ast-compare-cpu-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_CPU} ${REFERENCE_FILE_CPU})
  add_test(NAME ast-compare-gpu-${TEST_NAME} COMMAND diff ${OUTPUT_FILE_GPU} ${REFERENCE_FILE_GPU})
  set_tests_properties(ast-compare-cpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})
  set_tests_properties(ast-compare-gpu-${TEST_NAME} PROPERTIES DEPENDS ast-transform-${TEST_NAME})

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

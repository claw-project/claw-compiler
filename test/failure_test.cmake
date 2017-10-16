# Failure test definition
# Perform code transformation and check that it fails as it should

# Define input and output file name
set (ORIGINAL_FILE ${CMAKE_CURRENT_SOURCE_DIR}/original_code.f90)
set (OUTPUT_FILE ${CMAKE_CURRENT_SOURCE_DIR}/transformed_code.f90)

# Define executable file name
set (EXECUTABLE_ORIGINAL original_code_${TEST_NAME})

# Define directory for build
set (XMOD_DIR __xmod__)
set (OMNI_TMP_DIR __omni_tmp__)

# Directory where OMNI xmod files will be placed
if (NOT EXISTS ${XMOD_DIR})
  file(MAKE_DIRECTORY ${XMOD_DIR})
endif()

# Create intermediate representation in XcodeML Fortran format
if(${TEST_DEBUG}) # with debug option
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE}
    COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
      ${CLAWFC} ${OPTIONAL_FLAGS} --debug-omni --debug -J ${XMOD_DIR}
      --target=${CLAW_TARGET} --directive=${CLAW_DIRECTIVE}
      -o ${OUTPUT_FILE} ${ORIGINAL_FILE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${ORIGINAL_FILE}
    COMMENT "Translating CLAW directive with ${CLAWFC}"
  )
else() # without debug option
  add_custom_command(
    OUTPUT  ${OUTPUT_FILE}
    COMMAND ${CMAKE_COMMAND} -E env CLAW_TRANS_SET_PATH=${CLAW_TRANS_SET_PATH}
      ${CLAWFC} ${OPTIONAL_FLAGS} -J ${XMOD_DIR}
      --target=${CLAW_TARGET} --directive=${CLAW_DIRECTIVE}
      -o ${OUTPUT_FILE} ${ORIGINAL_FILE}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS ${ORIGINAL_FILE}
    COMMENT "Translating CLAW directive with ${CLAWFC}"
  )
endif()

add_custom_target(
  transform-${TEST_NAME}
  DEPENDS ${OUTPUT_FILE}
)

# Build the original code and the transformed code
add_executable (${EXECUTABLE_ORIGINAL} EXCLUDE_FROM_ALL ${ORIGINAL_FILE})

# Target to clean the generated file (Output of clawfc)
add_custom_target(
  clean-${TEST_NAME}
  COMMAND rm -f ${OUTPUT_FILE}
)

# Add target to the global build/clean target
add_dependencies(${CLEAN_TEST_TARGET} clean-${TEST_NAME})
add_dependencies(${CLEAN_TEST_TARGET}-${TEST_SET} clean-${TEST_NAME})

# Define additional compilation flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${CLAW_TEST_FFP_FLAGS}")
add_test(
  NAME transform-failure-${TEST_NAME}
  COMMAND "${CMAKE_COMMAND}"  --build ${CMAKE_BINARY_DIR}
  --target transform-${TEST_NAME}
)
# Check that the command fails
set_tests_properties(transform-failure-${TEST_NAME} PROPERTIES WILL_FAIL TRUE)

# Add build directory to be removed with clean target
set_property(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  PROPERTY ADDITIONAL_MAKE_CLEAN_FILES ${XMOD_DIR} ${OMNI_TMP_DIR}
)

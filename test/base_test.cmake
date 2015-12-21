# Base test definition
# Perform code transformation and compare the output with a reference

# Define input and output file name
set (ORIGINAL_FILE original_code.f90)
set (OUTPUT_FILE transformed_code.f90)
set (REFERENCE_FILE reference.f90)

# Define executable file name
set (EXECUTABLE_ORIGINAL original_code_${TEST_NAME})
set (EXECUTABLE_TRANSFORMED transformed_code_${TEST_NAME})

# Create intermediate representation in XcodeML Fortran format
add_custom_command(
  OUTPUT  ${OUTPUT_FILE}
  COMMAND ${CLAWF90} -o ${OUTPUT_FILE} ${ORIGINAL_FILE}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Translating CLAW directive with ${CLAWF90}"
)

add_custom_target(transform-${TEST_NAME} ALL
  DEPENDS ${OUTPUT_FILE})

# Clean previous transformed code to trigger the transformation each time as
# original code doesn't change but we want to test the transformation
add_custom_command(
  TARGET transform-${TEST_NAME}
  PRE_BUILD
  COMMAND rm ${OUTPUT_FILE}
  COMMENT "Clean previous transformation"
)

# Build the original code and the transformed code
add_executable (${EXECUTABLE_ORIGINAL} ${ORIGINAL_FILE})
add_executable (${EXECUTABLE_TRANSFORMED} ${OUTPUT_FILE})

# Compare reference transformed code and output of the transformation
add_test(NAME ast-transform-${TEST_NAME} COMMAND diff ${OUTPUT_FILE} ${REFERENCE_FILE})

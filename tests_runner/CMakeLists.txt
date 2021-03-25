# Configure files with variables information
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Resources.java.in
  ${CLAW_TESTS_GEN_SRC_DIR}/claw/tests/Resources.java
  @ONLY
)

# Configure files with variables information
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Resources.java.in
  ${CLAWFC_TESTS_GEN_SRC_DIR}/clawfc/tests/Resources.java
  @ONLY
)

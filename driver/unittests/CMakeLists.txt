# Configure files with variables information
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/src/Resources.java.in
  ${CLAWFC_UT_GEN_SRC_DIR}/clawfc/ut/Resources.java
  @ONLY
)

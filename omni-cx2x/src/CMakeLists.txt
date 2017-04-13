# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW XcodeML manipulation library and translator library

# Generate file with version number for Configuration check
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cx2x/ClawVersion.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/cx2x/ClawVersion.java
  @ONLY
)

# Bootstrap the compilation process
add_custom_target(omni-cx2x-init ALL)
add_custom_command(
  TARGET omni-cx2x-init
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  init
  COMMENT "Resolving dependencies for the build"
)

# Compile the CX2X libraries
add_custom_target(omni-cx2x ALL)
add_custom_command(
  TARGET omni-cx2x
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dcx2x.xcodeml.jar="${OMNI_CX2X_XCODEML_NAME}.jar"
  -Dcx2x.translator.jar="${OMNI_CX2X_CLAW_NAME}.jar"
  -Dantlr4.jar="${ANTLR4_NAME}.jar"
  -Dcommoncli.jar="${COMMON_CLI_NAME}.jar"
  COMMENT "Building CLAW XcodeML/F to XcodeML/F libraries"
)
add_dependencies(omni-cx2x omni-cx2x-init)

# Install libraries
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${ANTLR4_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${COMMON_CLI_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)

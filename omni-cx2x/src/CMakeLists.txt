# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW XcodeML manipulation library and translator library

# Bootstrap the compilation process
add_custom_target(omni-cx2x-init ALL)
add_custom_command(TARGET omni-cx2x-init COMMAND ${Ant_EXECUTABLE} init)

# Compile the CX2X libraries
add_custom_target(omni-cx2x ALL)
add_custom_command(
  TARGET omni-cx2x
  COMMAND ${Ant_EXECUTABLE}
  -Dcx2x.xcodeml.jar="${OMNI_CX2X_XCODEML_NAME}.jar"
  -Dcx2x.translator.jar="${OMNI_CX2X_CLAW_NAME}.jar"
  -Dantlr.jar="${ANTLR_NAME}.jar"
  -Dcommoncli.jar="${COMMON_CLI_NAME}.jar"
)
add_dependencies(omni-cx2x omni-cx2x-init)

# Install libraries
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lib/${ANTLR_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lib/${COMMON_CLI_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)

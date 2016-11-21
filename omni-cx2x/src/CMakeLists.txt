# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW XcodeML manipulation library and translator library

# Compile libraries with ANT
add_custom_target(omni-cx2x ALL)
add_custom_command(
  TARGET omni-cx2x
  COMMAND ${Ant_EXECUTABLE}
  -Dcx2x.xcodeml.jar="${OMNI_CX2X_XCODEML_NAME}.jar"
  -Dcx2x.translator.jar="${OMNI_CX2X_CLAW_NAME}.jar"
  -Dantlr.jar="${ANTLR_NAME}.jar"
)

# Install libraries
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lib/${ANTLR_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/lib/${COMMON_CLI_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)

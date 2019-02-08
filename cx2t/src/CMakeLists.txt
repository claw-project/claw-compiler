# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW X2T libraries

# Generate file with version number for Configuration check
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java
  @ONLY
)
if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(claw-init ALL)
  add_custom_command(
    TARGET claw-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Compile the CX2T libraries
add_custom_target(claw-cx2t ALL)
add_custom_command(
  TARGET claw-cx2t
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dantlr4.jar="${ANTLR4_NAME}.jar"
  -Dcommoncli.jar="${COMMON_CLI_NAME}.jar"
  ${DISABLE_RESOLVE}
  COMMENT "Building CLAW X2T libraries"
)
if(NOT OFFLINE)
  add_dependencies(claw-cx2t claw-init)
  add_dependencies(claw-cx2t omni-compiler)
endif()

# Install JAR files
install(
  FILES
    ${PROJECT_BINARY_DIR}/build/${CLAW_X2T_TATSU}.jar
    ${PROJECT_BINARY_DIR}/build/${CLAW_X2T_SHENRON}.jar
    ${PROJECT_BINARY_DIR}/build/${CLAW_X2T_WANI}.jar
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${ANTLR4_NAME}.jar
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${ANTLR4_RUNTIME_NAME}.jar
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${ANTLR_RUNTIME_NAME}.jar
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${COMMON_CLI_NAME}.jar
    ${CMAKE_CURRENT_SOURCE_DIR}/../lib/${TOML_NAME}.jar
  DESTINATION
    ${CMAKE_INSTALL_PREFIX}/share/claw/
)

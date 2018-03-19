# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW X2T libraries

# Allow the user to specify the location of Jython by doing
# cmake -DJYTHON_DIR=/blah/jython.x
set(JYTHON_DIR "" CACHE FILEPATH "Path to dir containing Jython jar")

# Generate file with version number for Configuration check
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java
  @ONLY
)
if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(omni-claw-init ALL)
  add_custom_command(
    TARGET omni-claw-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Compile the CX2T libraries
add_custom_target(omni-claw ALL)
add_custom_command(
  TARGET omni-claw
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
  add_dependencies(omni-claw omni-claw-init)
endif()

if(EXISTS ${JYTHON_DIR}/jython.jar)
  # Compile Jython-dependent plugin (interfaces between java and Python)
  add_custom_target(jython-plugin ALL)
  add_custom_command(
    TARGET jython-plugin
    DEPENDS ${JYTHON_DIR}/jython.jar
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    -Djython.dir=${JYTHON_DIR} jython-plugin
    COMMENT "Building Jython plugin"
    )
  # Install the plugin
  install(
    FILES
      ${PROJECT_BINARY_DIR}/build/${CLAW_X2T_PYTHON}.jar
    DESTINATION
      ${CMAKE_INSTALL_PREFIX}/share/claw/
  )
else()
  message(WARNING
    "JYTHON_DIR not specified or incorrect - Jython plugin will not be built")
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
  DESTINATION
    ${CMAKE_INSTALL_PREFIX}/share/claw/
)

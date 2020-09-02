# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW X2T libraries

# Generate file with version number for Configuration check
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java.in
  ${CMAKE_CURRENT_BINARY_DIR}/claw/ClawVersion.java
  @ONLY
)

if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(claw-init ALL)
  add_custom_command(
    TARGET claw-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
    -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
    -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}/claw
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Compile the CX2T libraries
add_custom_target(claw-cx2t ALL)
add_dependencies(claw-cx2t create_int_install_dir)
add_custom_command(
  TARGET claw-cx2t
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
  -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}/claw
  -Dantlr4.jar="${ANTLR4_NAME}.jar"
  -Dcommoncli.jar="${COMMON_CLI_NAME}.jar"
  ${DISABLE_RESOLVE}
  COMMENT "Building CLAW X2T libraries"
  COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/share/claw
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
      ${CMAKE_BINARY_DIR}/build/${CLAW_X2T_TATSU}.jar
      ${CMAKE_BINARY_DIR}/build//${CLAW_X2T_SHENRON}.jar
      ${CMAKE_BINARY_DIR}/build/${CLAW_X2T_WANI}.jar
      ${3RDPARTY_LIB_DIR}/${ANTLR4_NAME}.jar
      ${3RDPARTY_LIB_DIR}/${ANTLR4_RUNTIME_NAME}.jar
      ${3RDPARTY_LIB_DIR}/${ANTLR_RUNTIME_NAME}.jar
      ${3RDPARTY_LIB_DIR}/${COMMON_CLI_NAME}.jar
      ${3RDPARTY_LIB_DIR}/${TOML_NAME}.jar
      ${INT_CLAW_HOME}/share/claw
)
if(NOT OFFLINE)
  add_dependencies(claw-cx2t claw-init)
  add_dependencies(claw-cx2t omni-compiler)
endif()

add_custom_target(copy_cx2t_libs_to_int_install ALL

    )


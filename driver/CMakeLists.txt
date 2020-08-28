# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file define the configuration and the installation steps of the
# claw compiler driver

if(APPLE)
  set(SHA1SUM_CMD "shasum")
else()
  set(SHA1SUM_CMD "sha1sum")
endif()

set(CLAW_X2T_CONFIG_PATH "${CMAKE_BINARY_DIR}/driver/etc/")
set(CLAW_X2T_DRIVER_LIB_DIR "${CMAKE_BINARY_DIR}/driver/libexec/")
set(CLAW_X2T_TATSU_JAR "${CMAKE_BINARY_DIR}/build/${CLAW_X2T_TATSU}.jar")
set(CLAW_X2T_SHENRON_JAR "${CMAKE_BINARY_DIR}/build/${CLAW_X2T_SHENRON}.jar")
set(CLAW_X2T_WANI_JAR "${CMAKE_BINARY_DIR}/build/${CLAW_X2T_WANI}.jar")
set(CLAW_XMOD_GENERIC "${CMAKE_BINARY_DIR}/modules/")

# Configure files with variables information
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_PY_CONF_FILE}.in
  ${CMAKE_CURRENT_BINARY_DIR}/etc/${CLAW_PY_CONF_FILE}
  @ONLY
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}.in
  ${CMAKE_CURRENT_BINARY_DIR}/bin/${CLAW_COMPILER_FILE}
  @ONLY
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/libexec/${CLAW_LIB_SH}.in
  ${CMAKE_CURRENT_BINARY_DIR}/libexec/${CLAW_LIB_SH}
  @ONLY
)
add_custom_target(copy_driver_files_to_int_install ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/bin ${INT_CLAW_HOME}/etc ${INT_CLAW_HOME}/libexec
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/bin/${CLAW_COMPILER_FILE} ${INT_CLAW_HOME}/bin
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/etc/${CLAW_PY_CONF_FILE} ${INT_CLAW_HOME}/etc
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/libexec/${CLAW_LIB_SH} ${INT_CLAW_HOME}/libexec
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw_config.xsd
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw-default.xml
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw-external-set.xml
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw-high-level-set.xml
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw-internal-set.xml
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw-low-level-set.xml
      ${CMAKE_CURRENT_SOURCE_DIR}/etc/claw_transformation_set.xsd ${INT_CLAW_HOME}/etc
    COMMAND ${Python_EXECUTABLE} ${INT_CLAW_HOME}/etc/${CLAW_PY_CONF_FILE}
    )

add_dependencies(copy_driver_files_to_int_install create_int_install_dir)

set(ANTLR4 "${CMAKE_BINARY_DIR}/cx2t/lib/${ANTLR4_NAME}.jar")
set(ANTLR4_RUNTIME "${CMAKE_BINARY_DIR}/cx2t/lib/${ANTLR4_RUNTIME_NAME}.jar")
set(COMMON_CLI "${CMAKE_BINARY_DIR}/cx2t/lib/${COMMON_CLI_NAME}.jar")
set(TOML "${CMAKE_BINARY_DIR}/cx2t/lib/${TOML_NAME}.jar")

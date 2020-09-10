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

add_custom_target(copy_driver_files_to_int_install ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/bin ${INT_CLAW_HOME}/etc ${INT_CLAW_HOME}/libexec
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/bin/${CLAW_COMPILER_FILE} ${INT_CLAW_HOME}/bin
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/etc/${CLAW_PY_CONF_FILE} ${INT_CLAW_HOME}/etc
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/libexec/${CLAW_LIB_SH} ${INT_CLAW_HOME}/libexec
    COMMAND ${Python_EXECUTABLE} ${INT_CLAW_HOME}/etc/${CLAW_PY_CONF_FILE}
    )

add_dependencies(copy_driver_files_to_int_install create_int_install_dir)

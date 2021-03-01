# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file define the configuration and the installation steps of the
# claw compiler driver

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}.in
  ${CMAKE_CURRENT_BINARY_DIR}/bin/tmp/${CLAW_COMPILER_FILE}
  @ONLY
)
file(COPY ${CMAKE_CURRENT_BINARY_DIR}/bin/tmp/${CLAW_COMPILER_FILE}
  DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/bin
  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ
  GROUP_EXECUTE)


configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/${CLAWFC_CONF_FILE}.in
  ${CLAW_X2T_CONFIG_DIR}/${CLAWFC_CONF_FILE}
  @ONLY
)

add_custom_target(copy_driver_files_to_int_install ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME}/bin
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_BINARY_DIR}/bin/${CLAW_COMPILER_FILE} ${INT_CLAW_HOME}/bin
    )
add_dependencies(copy_driver_files_to_int_install create_int_install_dir)

add_subdirectory(tests)
add_subdirectory(unittests)

# This file is released under terms of BSD license
# See LICENSE file for more information

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw.properties.in
  ${CX2T_PROPERTIES_DIR}/claw.properties
  @ONLY
)

add_subdirectory(src)
add_subdirectory(unittest)


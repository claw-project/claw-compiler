# This file is released under terms of BSD license
# See LICENSE file for more information

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw.properties.in
  ${CMAKE_CURRENT_SOURCE_DIR}/claw.properties
  @ONLY
)

if(OFFLINE)
  set(DISABLE_RESOLVE "-Doffline=true")
endif()

add_subdirectory(src)
add_subdirectory(unittest)

add_dependencies(claw-ut claw-cx2t)

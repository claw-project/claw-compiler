# This file is released under terms of BSD license
# See LICENSE file for more information

set(CX2T_COMMON_DIR "${CMAKE_CURRENT_SOURCE_DIR}/common")
set(CX2T_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/generated-src")
set(CX2T_UT_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/ut-generated-src")
set(CX2T_PROPERTIES_DIR "${CMAKE_CURRENT_BINARY_DIR}/common")

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/common/claw.properties.in
  ${CX2T_PROPERTIES_DIR}/claw.properties
  @ONLY
)

if(OFFLINE)
  set(DISABLE_RESOLVE "-Doffline=true")
endif()

add_subdirectory(src)
add_subdirectory(unittest)

add_dependencies(claw-ut claw-cx2t)

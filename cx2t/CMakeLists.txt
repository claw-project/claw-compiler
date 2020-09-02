# This file is released under terms of BSD license
# See LICENSE file for more information

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw.properties.in
  ${CX2T_PROPERTIES_DIR}/claw.properties
  @ONLY
)

if(OFFLINE)
  set(DISABLE_RESOLVE "-Doffline=true")
endif()

if(NOT OFFLINE)
  # Get 3rdparty dependencies
  add_custom_target(claw-deps ALL)
  add_custom_command(
    TARGET claw-deps
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/claw-dependencies.xml
    ${ANT_FLAGS}
    -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
    main
    COMMENT "Resolving dependencies for the build"
  )
endif()

add_subdirectory(src)
add_subdirectory(unittest)

add_dependencies(claw-ut claw-cx2t)

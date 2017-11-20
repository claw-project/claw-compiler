# This file is released under terms of BSD license
# See LICENSE file for more information

# Compilation and execution of JUnit test cases.

# Configure files depending on a path
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java
  @ONLY
)

if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(omni-claw-ut-init ALL)
  add_custom_command(
    TARGET omni-claw-ut-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Add target for the compilation of JUnit test cases with ANT
add_custom_target(omni-claw-ut ALL)
add_custom_command(
  TARGET omni-claw-ut
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  ${DISABLE_RESOLVE}
  COMMENT "Building CLAW XcodeML/F to XcodeML/F unit tests"
)
if(NOT OFFLINE)
  add_dependencies(omni-claw-ut omni-claw-ut-init)
endif()

# Add test cases
add_test(
  NAME junit
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  ${DISABLE_RESOLVE}
  junit
)

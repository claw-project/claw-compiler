# This file is released under terms of BSD license
# See LICENSE file for more information

# Compilation and execution of JUnit test cases.

# Configure files depending on a path
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java.in
  ${CX2T_UT_GEN_SRC_DIR}/helper/TestConstant.java
  @ONLY
)

if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(claw-ut-init ALL)
  add_custom_command(
    TARGET claw-ut-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Add test cases
add_test(
  NAME junit-tatsu
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  ${DISABLE_RESOLVE}
  junit-tatsu
)
set_tests_properties(junit-tatsu PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

add_test(
  NAME junit-shenron
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  ${DISABLE_RESOLVE}
  junit-shenron
)
set_tests_properties(junit-shenron PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

add_test(
  NAME junit-wani
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  ${DISABLE_RESOLVE}
  junit-wani
)
set_tests_properties(junit-wani PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

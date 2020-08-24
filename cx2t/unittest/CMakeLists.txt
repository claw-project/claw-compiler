# This file is released under terms of BSD license
# See LICENSE file for more information

# Compilation and execution of JUnit test cases.

# Configure files depending on a path
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java.in
  ${CMAKE_CURRENT_BINARY_DIR}/helper/TestConstant.java
  @ONLY
)

if(NOT OFFLINE)
  # Bootstrap the compilation process
  add_custom_target(claw-ut-init ALL)
  add_custom_command(
    TARGET claw-ut-init
    COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
    ${ANT_FLAGS}
    -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
    -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
    -Ddist.dir=${CMAKE_BINARY_DIR}/build
    -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
    -Dlib.dir.prefix=${CMAKE_CURRENT_BINARY_DIR}/..
    -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
    -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}
    init
    COMMENT "Resolving dependencies for the build"
  )
endif()

# Add target for the compilation of JUnit test cases with ANT
add_custom_target(claw-ut ALL)
add_custom_command(
  TARGET claw-ut
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  ${ANT_FLAGS}
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dlib.dir.prefix=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
  -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}
  ${DISABLE_RESOLVE}
  COMMENT "Building CLAW XcodeML/F to XcodeML/F unit tests"
)
if(NOT OFFLINE)
  add_dependencies(claw-ut claw-ut-init)
endif()

# Add test cases
add_test(
  NAME junit-tatsu
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dtest.reports=${CMAKE_BINARY_DIR}/reports
  -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dlib.dir.prefix=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
  -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}
  ${DISABLE_RESOLVE}
  junit-tatsu
)
set_tests_properties(junit-tatsu PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

add_test(
  NAME junit-shenron
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dtest.reports=${CMAKE_BINARY_DIR}/reports
  -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dlib.dir.prefix=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
  -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}
  ${DISABLE_RESOLVE}
  junit-shenron
)
set_tests_properties(junit-shenron PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

add_test(
  NAME junit-wani
  COMMAND ${Ant_EXECUTABLE} -f ${CMAKE_CURRENT_SOURCE_DIR}/build.xml
  -Dantfile.dir=${CMAKE_CURRENT_SOURCE_DIR}
  -Dbuild.dir=${CMAKE_BINARY_DIR}/classes
  -Ddist.dir=${CMAKE_BINARY_DIR}/build
  -Dtest.reports=${CMAKE_BINARY_DIR}/reports
  -Dclaw.properties.dir=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dlib.dir.prefix=${CMAKE_CURRENT_BINARY_DIR}/..
  -Dbin.dir=${CMAKE_CURRENT_BINARY_DIR}/bin
  -Dgen.src.dir=${CMAKE_CURRENT_BINARY_DIR}
  ${DISABLE_RESOLVE}
  junit-wani
)
set_tests_properties(junit-wani PROPERTIES
ENVIRONMENT CLASSPATH=${ASM_BUILD}:$ENV{CLASSPATH})

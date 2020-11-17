# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMakeLists.txt file is the entry point for the configuration and the
# compilation of the CLAW Compiler.

cmake_minimum_required(VERSION 3.0)

project("CLAW Compiler" VERSION 2.0.2)

# Add local cmake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/module")

# Enable needed modules and languages
enable_language(Fortran)
enable_testing()

find_package(Java 1.8 REQUIRED)
# Warning!!! After finding the first version, find_package(Java) does NOT look for others. CMAKE_FIND_PACKAGE_SORT_ORDER
# and CMAKE_FIND_PACKAGE_SORT_DIRECTION have no effect. To override the "default" java version, set cmake variable
# JAVA_HOME to desired java installation path.

if(Java_VERSION_MAJOR EQUAL 1)
  if(Java_VERSION_MINOR LESS 8)
    message(FATAL_ERROR "Java ${Java_VERSION_STRING} is not supported")
  endif()
else()
  if((Java_VERSION_MAJOR LESS 8) OR (Java_VERSION_MAJOR GREATER 13) )
    message(FATAL_ERROR "Java ${Java_VERSION_STRING} is not supported")
  endif()
endif()

find_package(Ant 1.8 REQUIRED)
find_package(Python REQUIRED)

if(Python_VERSION_MAJOR LESS 2)
    message(FATAL_ERROR "Python ${Python_VERSION} is not supported")
endif()

if(Python_VERSION_MAJOR EQUAL 2)
    if(Python_VERSION_MINOR LESS 6)
        message(FATAL_ERROR "Python ${Python_VERSION} is not supported")
    endif()
endif()

include(ExternalProject)
include(cmake/git.cmake)
include(cmake/omni_compiler.cmake)

option(BUILD_OMNI_XCODEML_TOOLS "Build OMNI Compiler Tools" ON)
option(ADD_OMNI_XCODEML_TOOLS_TO_INSTALL "Copy OMNI Compiler Tools to CLAW install directory" ON)
# Load all variables from file
include(properties.cmake)

if(OFFLINE)
  message(STATUS "CLAW Compiler offline compilation enabled")
endif()

# Recover commit hash for the top repositiory
git_get_rev_hash(${CMAKE_SOURCE_DIR} CLAWFC_GIT_HASH)
message(STATUS "CLAW Compiler version ${CLAWFC_GIT_HASH}")

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(
    CMAKE_INSTALL_PREFIX "/usr/local"
    CACHE PATH "default install path" FORCE
  )
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

# Set ant to be verbose
if(ANT_VERBOSE)
  set(ANT_FLAGS "-v")
endif()

# Define preprocessor information used in the driver and tests
# The driver uses preprocessed files only but the tests compile the code.
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  include(compiler/cray.cmake)
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI")
  include(compiler/pgi.cmake)
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel")
  include(compiler/intel.cmake)
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NAG")
  include(compiler/nag.cmake)
else()
  include(compiler/gnu.cmake)
endif()

message(
  STATUS
  "Configure preprocessor with ${CMAKE_Fortran_COMPILER_ID} compiler"
)

# Option to build OMNI Compiler as a submodule. Should be ON as CLAW uses a
# specific version of OMNI Compiler at the moment.
option(OMNI_ENABLE_GNU_INTRINSIC "OMNI Compiler - Enable GNU intrinsic extensions" ON)

if(OMNI_ENABLE_GNU_INTRINSIC)
  list(APPEND OMNI_CONF_OPTION "--enable-gnu-extension")
endif()

add_custom_target(create_int_install_dir ALL
    COMMAND ${CMAKE_COMMAND} -E make_directory ${INT_CLAW_HOME})

install(
    DIRECTORY ${INT_CLAW_HOME}/
    DESTINATION ${CMAKE_INSTALL_PREFIX}
    FILE_PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ)
install(CODE "execute_process(COMMAND ${Ant_EXECUTABLE} -f ${ANT_SOLUTION} ${ANT_FLAGS} -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR} install-driver)"
)
install(CODE "execute_process(COMMAND ${Python_EXECUTABLE} ${CMAKE_INSTALL_PREFIX}/etc/${CLAW_PY_CONF_FILE})")
install(CODE "execute_process(COMMAND rm -rf ${CMAKE_INSTALL_PREFIX}/etc/${CLAW_PY_CONF_FILE})")

# Build OMNI compiler
if(BUILD_OMNI_XCODEML_TOOLS)
  find_package(LibXml2 REQUIRED)
  get_filename_component(LIBXML2_LIB_DIR ${LIBXML2_LIBRARY} DIRECTORY)
  set(OMNI_COMPILER_SRC_DIR ${CMAKE_BINARY_DIR}/omni-compiler)
  ExternalProject_Add(
    omni-compiler
    GIT_REPOSITORY ${OMNI_GIT_REPOSITORY}
    GIT_TAG ${OMNI_GIT_COMMIT_HASH}
    GIT_SHALLOW ON
    GIT_PROGRESS ON
    GIT_REMOTE_UPDATE_STRATEGY CHECKOUT
    SOURCE_DIR ${OMNI_COMPILER_SRC_DIR}
    INSTALL_DIR ${OMNI_HOME}
    CONFIGURE_COMMAND
      CC=${CMAKE_C_COMPILER}
      ${OMNI_COMPILER_SRC_DIR}/configure
      --without-native-fortran-compiler
      --with-libxml2-include=${LIBXML2_INCLUDE_DIR}
      --with-libxml2-lib=${LIBXML2_LIB_DIR}
      --with-java=${Java_JAVA_EXECUTABLE}
      --with-javac=${Java_JAVAC_EXECUTABLE}
      --with-jar=${Java_JAR_EXECUTABLE}
      --with-version-tag=${OMNI_GIT_COMMIT_HASH}
      --prefix=${OMNI_HOME}
      ${OMNI_CONF_OPTION}
    BUILD_IN_SOURCE ON
    BUILD_COMMAND make -j 1 # OMNI build is not thread safe
    INSTALL_COMMAND make install
  )
else()
  function(get_xcodemltools_version_tag _output_version_tag)
      execute_process(
        COMMAND ${OMNI_F_FRONT} --version-tag
        ERROR_VARIABLE output_version_tag
        OUTPUT_VARIABLE output_version_tag
        RESULT_VARIABLE return_code
        OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      if(NOT(${return_code} EQUAL "0"))
            message(FATAL_ERROR "get_omni_version_tag failed")
      endif()
      set(${_output_version_tag} ${output_version_tag} PARENT_SCOPE)
  endfunction()
  get_xcodemltools_version_tag(xcodemltools_version)
  if(NOT (${xcodemltools_version} STREQUAL ${OMNI_VERSION_TAG}))
    message(FATAL_ERROR "OMNI XCodeMLTools version tag \"${xcodemltools_version}\" does not match expected \"${OMNI_VERSION_TAG}\"")
  endif()
  add_custom_target(omni-compiler
    COMMENT "Checking OMNI XCodeMLTools version"
  )
endif(BUILD_OMNI_XCODEML_TOOLS)

# translator library and xcodeml manipulation library
add_subdirectory(cx2t)

# compiler driver
add_subdirectory(driver)

# generate generic .xmod files
add_subdirectory(modules)

# Build functional test
add_subdirectory(test)

# Build documentation
add_subdirectory(documentation)

set(ADDITIONAL_CLEAN_FILES "")
LIST(APPEND ADDITIONAL_CLEAN_FILES ${INT_CLAW_HOME})
SET_DIRECTORY_PROPERTIES(PROPERTIES ADDITIONAL_MAKE_CLEAN_FILES "${ADDITIONAL_CLEAN_FILES}")

if(OFFLINE)
  set(DISABLE_RESOLVE "-Doffline=true")
else()
  set(DISABLE_RESOLVE "-Doffline=false")
endif()

add_custom_target(build-claw-solution ALL)
add_dependencies(build-claw-solution omni-compiler)
add_custom_command(
  TARGET build-claw-solution
  COMMAND ${Ant_EXECUTABLE} -f ${ANT_SOLUTION}
  ${ANT_FLAGS} ${OFFLINE}
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  COMMENT "Building CLAW solution"
)

add_custom_target(run-driver-unit-tests)
add_dependencies(run-driver-unit-tests build-claw-solution)
add_custom_command(
  TARGET run-driver-unit-tests
  COMMAND ${Ant_EXECUTABLE} -f ${ANT_SOLUTION}
  ${ANT_FLAGS} ${OFFLINE}
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  run-driver-unit-tests
  COMMENT "Running CLAWFC unit-tests"
)

add_custom_target(run-driver-tests)
add_dependencies(run-driver-tests build-claw-solution)
add_custom_command(
  TARGET run-driver-tests
  COMMAND ${Ant_EXECUTABLE} -f ${ANT_SOLUTION}
  ${ANT_FLAGS} ${OFFLINE}
  -Dclaw.properties.dir=${CX2T_PROPERTIES_DIR}
  run-driver-tests
  COMMENT "Running CLAWFC tests"
)

option(GENERATE_ECLIPSE_IDE_PROJECTS "Generate Eclipse IDE projects" ON)
if(GENERATE_ECLIPSE_IDE_PROJECTS)
  message(STATUS "Generating Eclipse projects")
  foreach(PROJECT_NAME cx2t clawfc clawfc-ut clawfc-tests)
      file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/ide/eclipse/${PROJECT_NAME})
      configure_file(${CMAKE_SOURCE_DIR}/resources/eclipse/${PROJECT_NAME}.project.in
                     ${CMAKE_BINARY_DIR}/ide/eclipse/${PROJECT_NAME}/.project
                     @ONLY)
      configure_file(${CMAKE_SOURCE_DIR}/resources/eclipse/${PROJECT_NAME}.classpath.in
                     ${CMAKE_BINARY_DIR}/ide/eclipse/${PROJECT_NAME}/.classpath
                     @ONLY)
      configure_file(${CMAKE_SOURCE_DIR}/resources/eclipse/${PROJECT_NAME}.ant.builder.in
                     ${CMAKE_BINARY_DIR}/ide/eclipse/${PROJECT_NAME}/.externalToolBuilders/ant.launch
                     @ONLY)
  endforeach()
endif()

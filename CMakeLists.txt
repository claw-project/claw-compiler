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
find_package(Ant 1.7.1 REQUIRED)
find_package(Java 1.8 REQUIRED)
include(ExternalProject)
include(cmake/git.cmake)
include(cmake/omni_compiler.cmake)

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
option(BUILD_OMNI "Build OMNI Compiler" ON)
option(OMNI_ENABLE_GNU_INTRINSIC "OMNI Compiler - Enable GNU intrinsic extensions" ON)

if(OMNI_ENABLE_GNU_INTRINSIC)
  list(APPEND OMNI_CONF_OPTION "--enable-gnu-extension")
endif()

option(BUILD_OMNI "Build OMNI Compiler" ON)

# Build OMNI compiler
if(BUILD_OMNI)
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
      --prefix=${OMNI_HOME}
      ${OMNI_CONF_OPTION}
    BUILD_IN_SOURCE ON
    BUILD_COMMAND make -j 1 # OMNI build is not thread safe
    INSTALL_COMMAND make install
  )
endif()

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

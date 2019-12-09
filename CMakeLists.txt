# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMakeLists.txt file is the entry point for the configuration and the
# compilation of the CLAW Compiler.

cmake_minimum_required(VERSION 3.0)

project("CLAW Compiler" VERSION 2.0.1)

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

set(OMNI_COMPILER_SUBMODULE ${CMAKE_SOURCE_DIR}/omni-compiler)

if(OFFLINE)
  message(STATUS "CLAW Compiler offline compilation enabled")
endif()

# Recover commit hash for the top repositiory
git_get_rev_hash(${CMAKE_SOURCE_DIR} CLAWFC_GIT_HASH)
message(STATUS "CLAW Compiler version ${CLAWFC_GIT_HASH}")

# Recover commit hash for the OMNI Compiler subumodule
git_get_rev_hash(${OMNI_COMPILER_SUBMODULE} OMNI_GIT_HASH)
message(STATUS "OMNI Compiler version ${OMNI_GIT_HASH}")

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

# Build OMNI compiler
if(BUILD_OMNI)
  ExternalProject_Add(
    omni-compiler
    SOURCE_DIR ${OMNI_COMPILER_SUBMODULE}
    CONFIGURE_COMMAND
      CC=${CMAKE_C_COMPILER} CXX=${CMAKE_CXX_COMPILER}
      FC=${CMAKE_Fortran_COMPILER} JAVAC=${Java_JAVAC_EXECUTABLE}
      CFLAGS=${CMAKE_C_FLAGS}
      ${OMNI_COMPILER_SUBMODULE}/configure
      --prefix=${CMAKE_INSTALL_PREFIX} ${OMNI_TARGET} ${OMNI_CONF_OPTION}
      ${OMNI_MPI_CC} ${OMNI_MPI_FC}
    BUILD_COMMAND make -j 1 # OMNI build is not thread safe
    BUILD_IN_SOURCE 1
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

# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMakeLists.txt file is the entry point for the configuration and the
# compilation of the CLAW Fortran compiler.

cmake_minimum_required(VERSION 3.0)

project("CLAW FORTRAN Compiler" VERSION 0.3.0)

# Set up the local module path
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake/module")

# Enable needed modules and languages
enable_language(Fortran)
enable_testing()
find_package(Ant 1.7.1 REQUIRED)
find_package(Java 1.7 REQUIRED)
include(ExternalProject)

# Load all variables from file
include(properties.cmake)

# Recover commit hash for the CLAW Compiler main repo
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
message(STATUS "CLAW Compiler version ${GIT_COMMIT_HASH}")

# Recover commit hash for the OMNI Compiler subumodule
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/omni-compiler/
  OUTPUT_VARIABLE GIT_OMNI_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)
message(STATUS "OMNI Compiler version ${GIT_OMNI_COMMIT_HASH}")

# Define CLAW repository last commit hash
set(CLAWFC_GIT_HASH "${GIT_COMMIT_HASH}")
set(OMNI_GIT_HASH "${GIT_OMNI_COMMIT_HASH}")

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "default install path" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

# Define preprocessor information used in the driver and tests
# The driver uses preprocessed files only but the tests compile the code.
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  include(compiler/cray.cmake)
elseif("${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI")
  include(compiler/pgi.cmake)
else()
  include(compiler/gnu.cmake)
endif()
message(STATUS "Configure preprocessor with ${CMAKE_Fortran_COMPILER_ID} compiler")

# Option to build OMNI Compiler as a submodule. Should be ON as CLAW uses a
# specific version of OMNI Compiler at the moment.
option(BUILD_OMNI "Build OMNI Compiler" ON)

# Build OMNI compiler
if(BUILD_OMNI)
  set(OMNIC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/omni-compiler)
  ExternalProject_Add(
    omni-compiler
    SOURCE_DIR ${OMNIC_DIR}
    CONFIGURE_COMMAND touch ${OMNIC_DIR}/configure.ac ${OMNIC_DIR}/aclocal.m4
    ${OMNIC_DIR}/configure ${OMNIC_DIR}/Makefile.am ${OMNIC_DIR}/Makefile.in
    && ${OMNIC_DIR}/configure --prefix=${CMAKE_INSTALL_PREFIX} ${OMNI_TARGET} ${OMNI_CONF_OPTION} ${OMNI_MPI_CC} ${OMNI_MPI_FC}
    BUILD_COMMAND make
    BUILD_IN_SOURCE 1
  )
endif()

# translator library and xcodeml manipulation library
add_subdirectory(omni-cx2x)

# compiler driver
add_subdirectory(driver)

# Build functional test
add_subdirectory(test)

# Build documentation
add_subdirectory(documentation)

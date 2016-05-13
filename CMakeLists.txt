# This file is released under terms of BSD license
# See LICENSE file for more information

cmake_minimum_required(VERSION 2.8)

project(clawfc)

find_package(Java REQUIRED)

enable_language(Fortran)
enable_testing()

include(UseJava)         # To compile Java code and create Jars
include(ExternalProject) # For autotools based compilation (OMNI Compiler)

# Recover the last commit hash from git to add it to the version
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Define CLAW compiler version
set(CLAWFC_VERSION "0.2a rev. ${GIT_COMMIT_HASH}")

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "default install path" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

# Option to build OMNI Compiler as a submodule. Should be ON as CLAW uses a
# specific version of OMNI Compiler at the moment.
option(BUILD_OMNI "Build OMNI Compiler" ON)

# Define the OMNI installation directory
set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")
set(OMNI_CLASSPATH "${OMNI_HOME}/share/xcalablemp")
set(BUILD_DIR "${CMAKE_SOURCE_DIR}/build")
set(OMNI_CX2X_CLAW_NAME om-cx2x-claw)
set(OMNI_CX2X_XCODEML_NAME om-cx2x-xcodeml)
set(ANTLR_NAME antlr-4.5.2-complete)

# Define OMNI Compiler executables and jar archives install location.
set(OMNI_F_FRONT "${OMNI_HOME}/bin/F_Front")
set(OMNI_C_FRONT "${OMNI_HOME}/bin/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-exc-tools.jar")
set(OMNI_JAR_COMMON "${OMNI_CLASSPATH}/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")

# Define OMNI Compiler jar archives build location.
set(LOCAL_OMNI_JAR_TOOLS "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
set(LOCAL_OMNI_JAR_COMMON "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar")
set(LOCAL_OMNI_JAR_F_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar")
set(LOCAL_OMNI_JAR_C_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar")

# Define CLAW jar archives build location.
set(OMNI_BUILD_JAR_CX2X_CLAW "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_BUILD_JAR_CX2X_XCODEML "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")

# Define CLAW jar archives install location.
set(OMNI_JAR_CX2X_CLAW "${CMAKE_INSTALL_PREFIX}/share/cx2x/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_XCODEML "${CMAKE_INSTALL_PREFIX}/share/cx2x/${OMNI_CX2X_XCODEML_NAME}.jar")

# Define default configuration path.
set(CLAW_CONFIG_FILE "claw-default.xml")
set(OMNI_CX2X_CONFIG_DEFAULT "${CMAKE_INSTALL_PREFIX}/etc/${CLAW_CONFIG_FILE}")

# Define ANTLR jar archive install location.
set(ANTLR "${CMAKE_INSTALL_PREFIX}/share/cx2x/${ANTLR_NAME}.jar")

# Define preprocessor information used in the driver and tests
# The driver uses preprocessed files only but the tests compile the code.
if("${CMAKE_Fortran_COMPILER}" MATCHES "ftn")
  # For Cray Compiler
  set(FPPFLAGS "-e P")            # for preprocessing only
  set(FPP_REDIRECT false)         # cannot use redirection > to save file
  set(CLAW_TEST_FFP_FLAGS "-e Z") # for preprocessing and compilation
else()
  # Std option for gfortran/pgi
  set(FPPFLAGS "-E")          # for preprocessing only
  set(FPP_REDIRECT true)      # use redirection > to save file
  set(CLAW_TEST_FFP_FLAGS "") # don't need it with gfortran as it is automatic
endif()

# Build OMNI compiler
if(BUILD_OMNI)
  set(OMNIC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/omni-compiler)
  ExternalProject_Add(
    omni-compiler
    SOURCE_DIR ${OMNIC_DIR}
    CONFIGURE_COMMAND touch ${OMNIC_DIR}/configure.ac ${OMNIC_DIR}/aclocal.m4
    ${OMNIC_DIR}/configure ${OMNIC_DIR}/Makefile.am ${OMNIC_DIR}/Makefile.in
    && ${OMNIC_DIR}/configure --prefix=${CMAKE_INSTALL_PREFIX} ${OMNI_CONF_OPTION}
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

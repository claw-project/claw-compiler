cmake_minimum_required(VERSION 2.8)

project(clawfc)
set(CLAWFC_VERSION "0.1")

find_package(Java REQUIRED)

enable_language(Fortran)

include(UseJava)
include(ExternalProject)

enable_testing()

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "default install path" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)


option(BUILD_OMNI "Build OMNI Compiler" ON)

# Define the OMNI installation directory
set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")
set(OMNI_CLASSPATH "${OMNI_HOME}/share/xcalablemp")
set(BUILD_DIR "${CMAKE_SOURCE_DIR}/build")
set(OMNI_CX2X_CLAW_NAME om-cx2x-claw)
set(OMNI_CX2X_XCODEML_NAME om-cx2x-xcodeml)

# Define OMNI Compiler executables and jar archives
set(OMNI_F_FRONT "${OMNI_HOME}/bin/F_Front")
set(OMNI_C_FRONT "${OMNI_HOME}/bin/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-exc-tools.jar")
set(OMNI_JAR_COMMON "${OMNI_CLASSPATH}/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")

# Define location of jar archives where they are built. For test and javadoc.
set(LOCAL_OMNI_JAR_TOOLS "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
set(LOCAL_OMNI_JAR_COMMON "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar")
set(LOCAL_OMNI_JAR_F_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar")
set(LOCAL_OMNI_JAR_C_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar")

set(OMNI_BUILD_JAR_CX2X_CLAW "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_BUILD_JAR_CX2X_XCODEML "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_CLAW "${CMAKE_INSTALL_PREFIX}/share/cx2x/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_XCODEML "${CMAKE_INSTALL_PREFIX}/share/cx2x/${OMNI_CX2X_XCODEML_NAME}.jar")

if(BUILD_OMNI)
  set(OMNIC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/omni-compiler)
  # Build OMNI compiler
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


# Build instruction for the xcodeml translator tool
add_subdirectory(omni-cx2x)

# compiler driver
add_subdirectory(driver)

# Build functional test
add_subdirectory(test)

# Build documentation
add_subdirectory(documentation)

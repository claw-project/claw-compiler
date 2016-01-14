cmake_minimum_required(VERSION 2.8)

project(CLAWC)

find_package(Java REQUIRED)
include(UseJava)
enable_testing()

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "/usr/local/" CACHE PATH "default install path" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)


# Define the OMNI installation directory
set(OMNI_HOME "/usr/local/" CACHE STRING "OMNI Compiler install dir")

set(BUILD_DIR "${CMAKE_SOURCE_DIR}/build/")
set(OMNI_CX2X_NAME om-cx2x)

set(OMNI_F_FRONT "${OMNI_HOME}/bin/F_Front")
set(OMNI_C_FRONT "${OMNI_HOME}/bin/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_HOME}/share/xcalablemp/om-exc-tools.jar")
set(OMNI_JAR_COMMON "${OMNI_HOME}/share/xcalablemp/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_HOME}/share/xcalablemp/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_HOME}/share/xcalablemp/om-c-back.jar")
set(OMNI_BUILD_JAR_CX2X "${BUILD_DIR}${OMNI_CX2X_NAME}.jar")
set(OMNI_JAR_CX2X "${CMAKE_INSTALL_PREFIX}/share/cx2x/${OMNI_CX2X_NAME}.jar")


# Build instruction for the xcodeml translator tool
add_subdirectory(omni-cx2x)

# compiler driver
add_subdirectory(driver)

# Build functional test
add_subdirectory(test)

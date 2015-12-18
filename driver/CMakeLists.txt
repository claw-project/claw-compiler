# This CMake file define the configuration and the installation steps of the
# claw compiler driver

# TODO remove once it is integrated with the main CMakeLists.txt
cmake_minimum_required(VERSION 3.1)
project(CLAWC VERSION 0.1.0)

find_package(Java)
#if(NOT Java_Runtime_FOUND)
#  message(FATAL_ERROR "Java not found")
#endif()

# Define variables needed to configure the files
set(OMNI_HOME "/usr/local")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(JAVA_OPT "-Xmx200m -Xms200m")
set(OMNI_F2X_FLAGS "-I${OMNI_HOME}/include -M.")
set(FPPFLAGS "-E")  # TODO must be define on the fly
set(FPP "gfortran") # TODO must be define on the fly CMAKE_Fotran_COMPILER
set(CPP_OPT "-cpp") # TODO must be define on the fly
set(OMNI_JAR1 "${OMNI_HOME}/share/xcalablemp/om-exc-tools.jar")
set(OMNI_JAR2 "${OMNI_HOME}/share/xcalablemp/om-c-back.jar")
set(OMNI_JAR3 "${OMNI_HOME}/share/xcalablemp/om-f-back.jar")
set(OMNI_JAR4 "${OMNI_HOME}/share/xcalablemp/om-common.jar")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawc")
set(CLAW_LIB_SH "claw_f_lib.sh")

# Check existence of OMNI Compiler Java librairies
if(NOT EXISTS ${OMNI_JAR1} OR NOT EXISTS ${OMNI_JAR2} OR NOT EXISTS ${OMNI_JAR3} OR NOT EXISTS ${OMNI_JAR4})
  message(FATAL_ERROR "OMNI Compiler library not found. Specify OMNI Compiler install path with -DOMNI_HOME=<path>")
endif()

# Check the existence of the OMNI Compiler front-end
# TODO


# Configure files with varibales information
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_CONF_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_CONF_FILE}
  @ONLY
)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}
  @ONLY
)
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/libexec/${CLAW_LIB_SH}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/libexec/${CLAW_LIB_SH}
  @ONLY
)

# Installatin steps
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_CONF_FILE}
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
  DESTINATION ${CMAKE_INSTALL_PREFIX}/etc
)
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
  DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
)
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/libexec/${CLAW_LIB_SH}
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
  DESTINATION ${CMAKE_INSTALL_PREFIX}/libexec
)

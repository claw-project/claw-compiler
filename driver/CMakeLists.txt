cmake_minimum_required(VERSION 3.1)

# Set varibous variables needed for the configuration

find_package(Java)

#if(NOT Java_Runtime_FOUND)
#  message(FATAL_ERROR "Java not found")
#endif()
set(OMNI_HOME "/usr/local")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(JAVA_OPT "-Xmx200m -Xms200m")
set(OMNI_F2X_FLAGS "-I${OMNI_HOME}/include -M.")
set(FPPFLAGS "-E")
set(FPP "gfortran")
set(CPP_OPT "-cpp")
set(OMNI_JAR1 "${OMNI_HOME}/share/xcalablemp/om-exc-tools.jar")
set(OMNI_JAR2 "${OMNI_HOME}/share/xcalablemp/om-c-back.jar")
set(OMNI_JAR3 "${OMNI_HOME}/share/xcalablemp/om-f-back.jar")
set(OMNI_JAR4 "${OMNI_HOME}/share/xcalablemp/om-common.jar")

set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")

set(CLAW_CONF_FILE "clawc.conf")
set(CLAW_COMPILER_FILE "clawc")

# Check existence of OMNI Compiler Java librairies
if(NOT EXISTS ${OMNI_JAR1} OR NOT EXISTS ${OMNI_JAR2} OR NOT EXISTS ${OMNI_JAR3} OR NOT EXISTS ${OMNI_JAR4})
  message(FATAL_ERROR "OMNI Compiler library not found. Specify OMNI Compiler install path with -DOMNI_HOME=<path>")
endif()



configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_CONF_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_CONF_FILE}
  @ONLY
)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_COMPILER_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_COMPILER_FILE}
  @ONLY
)


install(
  FILES ${CLAW_CONF_FILE}
  DESTINATION ${CMAKE_INSTALL_PREFIX}/etc
)

install(
  FILES ${CLAW_COMPILER_FILE}
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
  DESTINATION ${CMAKE_INSTALL_PREFIX}/bin
)

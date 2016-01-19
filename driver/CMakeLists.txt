# This CMake file define the configuration and the installation steps of the
# claw compiler driver

find_package(Java)
#if(NOT Java_Runtime_FOUND)
#  message(FATAL_ERROR "Java not found")
#endif()

# Define variables needed to configure the files
set(OMNI_HOME "/usr/local")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(JAVA_OPT "-Xmx200m -Xms200m")
set(OMNI_F2X_FLAGS "")
set(FPPFLAGS "-E")  # TODO must be define on the fly
set(FPP "gfortran") # TODO must be define on the fly CMAKE_Fotran_COMPILER
set(CPP_OPT "-cpp") # TODO must be define on the fly
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAW_LIB_SH "claw_f_lib.sh")

# Check existence of OMNI Compiler Java librairies
if(NOT EXISTS ${OMNI_JAR_TOOLS} OR NOT EXISTS ${OMNI_JAR_COMMON} OR NOT EXISTS ${OMNI_JAR_F_BACKEND} OR NOT EXISTS ${OMNI_JAR_C_BACKEND})
  message(FATAL_ERROR "OMNI Compiler library not found. Specify OMNI Compiler install path with -DOMNI_HOME=<path>")
endif()

# Check the existence of the OMNI Compiler front-end
if(NOT EXISTS ${OMNI_F_FRONT})
  message(FATAL_ERROR "OMNI Compiler Fortran front-end not found.") # TODO specifiy how to give OMNI_HOME
endif()

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

# Only to run test, we use the local build
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_SOURCE_DIR}/driver/etc/")
set(OMNI_JAR_TOOLS="${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
set(OMNI_JAR_COMMON "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar")
set(OMNI_JAR_F_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar")
set(OMNI_JAR_CX2X_CLAW "${CMAKE_SOURCE_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_XCODEML "${CMAKE_SOURCE_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar")
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_CONF_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/etc/${CLAW_CONF_FILE}_test
  @ONLY
)
set(CLAW_CONF_FILE "${CLAW_CONF_FILE}_test")
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}.in
  ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}_test
  @ONLY
)

# Copy the executable to be used by the test directly before installation
file(REMOVE ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_COMPILER_FILE})
file(REMOVE ${CMAKE_CURRENT_SOURCE_DIR}/${CLAW_COMPILER_FILE}_test)
file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}
  DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/
  FILE_PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
)
file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/bin/${CLAW_COMPILER_FILE}_test
  DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/
  FILE_PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
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

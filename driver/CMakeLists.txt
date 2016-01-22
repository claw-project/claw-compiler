# This CMake file define the configuration and the installation steps of the
# claw compiler driver

find_package(Java)
#if(NOT Java_Runtime_FOUND)
#  message(FATAL_ERROR "Java not found")
#endif()

# Define variables needed to configure the files
set(OMNI_HOME ${CMAKE_INSTALL_PREFIX})
set(OMNI_DRIVER_DIR "${OMNI_HOME}/libexec")
set(OMNI_INCLUDE_DIR "${OMNI_HOME}/include")
set(OMNI_BIN_DIR "${OMNI_HOME}/bin")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(JAVA_OPT "-Xmx200m -Xms200m")
set(OMNI_F2X_FLAGS "")
set(FPPFLAGS "-E")  # TODO must be define on the fly
set(FPP "gfortran") # TODO must be define on the fly CMAKE_Fotran_COMPILER
set(CPP_OPT "-cpp") # TODO must be define on the fly
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAW_LIB_SH "claw_f_lib.sh")

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


# Only to run test, we use the local build
set(OMNI_HOME "${CMAKE_SOURCE_DIR}/omni-compiler")
set(OMNI_DRIVER_DIR "${OMNI_HOME}/Driver/libexec")
set(OMNI_BIN_DIR "${OMNI_HOME}/F-Frontend/src")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_SOURCE_DIR}/driver/etc/")
set(OMNI_JAR_TOOLS "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
message("OMNI_JAR_TOOLS ${OMNI_JAR_TOOLS}")
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

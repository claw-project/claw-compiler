# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file centrailize variables used in the project


# Define build variables
set(BUILD_DIR "${CMAKE_SOURCE_DIR}/build")

# Define CX2X libraries names
set(OMNI_CX2X_CLAW_NAME "om-cx2x-claw")
set(OMNI_CX2X_XCODEML_NAME "om-cx2x-xcodeml")

# Define default configuration path.
set(CLAW_CONFIG_FILE "claw-default.xml")
set(OMNI_CX2X_CONFIG_DEFAULT "${CMAKE_INSTALL_PREFIX}/etc/${CLAW_CONFIG_FILE}")

# Driver variables
set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")
set(OMNI_CLASSPATH "${OMNI_HOME}/share/xcalablemp")
set(OMNI_DRIVER_DIR "${OMNI_HOME}/libexec")
set(OMNI_INCLUDE_DIR "${OMNI_HOME}/include")
set(OMNI_BIN_DIR "${OMNI_HOME}/bin")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(OM_CX2X_DRIVER_LIB_DIR "${CMAKE_INSTALL_PREFIX}/libexec/")
set(OMNI_F_FRONT "${OMNI_BIN_DIR}/F_Front")
set(OMNI_C_FRONT "${OMNI_BIN_DIR}/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-exc-tools.jar")
set(OMNI_JAR_COMMON "${OMNI_CLASSPATH}/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")
set(OMNI_F2X_FLAGS "")
set(JAVA_OPT "-Xmx200m -Xms200m")
set(FPP "${CMAKE_Fortran_COMPILER}")
set(CPP_OPT "${FPPFLAGS}")
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAW_LIB_SH "claw_f_lib.sh")

# Define CLAW jar archives build location.
set(OMNI_BUILD_JAR_CX2X_CLAW "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_BUILD_JAR_CX2X_XCODEML "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")

# Define CLAW jar archives install location.
set(CX2X_JAR_INSTALL_LOCATION "${CMAKE_INSTALL_PREFIX}/share/cx2x")
set(OMNI_JAR_CX2X_CLAW "${CX2X_JAR_INSTALL_LOCATION}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_XCODEML "${CX2X_JAR_INSTALL_LOCATION}/${OMNI_CX2X_XCODEML_NAME}.jar")

# Define OMNI Compiler jar archives build location.
set(LOCAL_OMNI_JAR_TOOLS "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
set(LOCAL_OMNI_JAR_COMMON "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar")
set(LOCAL_OMNI_JAR_F_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar")
set(LOCAL_OMNI_JAR_C_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar")



# Define third-party dependency names
set(ANTLR_NAME "antlr4")
set(COMMON_CLI_NAME "commons-cli")
set(ANTLR "${CX2X_JAR_INSTALL_LOCATION}/${ANTLR_NAME}.jar")
set(COMMON_CLI "${CX2X_JAR_INSTALL_LOCATION}/${COMMON_CLI_NAME}.jar")

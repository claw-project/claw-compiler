# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file centrailize variables used in the project


# Define build variables
set(BUILD_DIR "${CMAKE_SOURCE_DIR}/build")
set(ANT_FLAGS "-quiet")

# Define CX2X libraries names
set(OMNI_CX2X_CLAW_NAME "om-cx2x-claw")
set(OMNI_CX2X_XCODEML_NAME "om-cx2x-xcodeml")

# Define default configuration path.
set(CLAW_CONFIG_FILE "claw-default.xml")
set(CLAW_CONFIG_XSD "claw_config.xsd")
set(CLAW_CONFIG_SET_XSD "claw_transformation_set.xsd")
set(CLAW_TRANS_SET_INTERNAL "claw-internal-set.xml")
set(CLAW_TRANS_SET_LOW "claw-low-level-set.xml")
set(CLAW_TRANS_SET_HIGH "claw-high-level-set.xml")
set(OMNI_CX2X_CONFIG_PATH "${CMAKE_INSTALL_PREFIX}/etc/")

# Driver variables
set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")
set(OMNI_CLASSPATH "${OMNI_HOME}/share/xcalablemp")
set(OMNI_DRIVER_DIR "${OMNI_HOME}/libexec")
set(OMNI_INCLUDE_DIR "${OMNI_HOME}/include")
set(CLAW_XMOD_GENERIC "${OMNI_HOME}/fincludes")
set(OMNI_XMOD_GENERIC "${OMNI_HOME}/fincludes")
set(OMNI_BIN_DIR "${OMNI_HOME}/bin")
set(OM_CX2X_DRIVER_CONF_DIR "${CMAKE_INSTALL_PREFIX}/etc/")
set(OM_CX2X_DRIVER_LIB_DIR "${CMAKE_INSTALL_PREFIX}/libexec/")
set(OMNI_F_FRONT "${OMNI_BIN_DIR}/F_Front")
set(OMNI_C_FRONT "${OMNI_BIN_DIR}/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-exc-tools.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")
set(OMNI_F2X_FLAGS "")
if(${Java_VERSION} VERSION_EQUAL "1.7")
  set(JAVA_OPT "-Xmx200m -Xms200m")
endif()
set(FPP "${CMAKE_Fortran_COMPILER}")
set(CPP_OPT "${FPPFLAGS}")
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAW_LIB_SH "claw_f_lib.sh")

# Define CLAW jar archives build location.
set(OMNI_BUILD_JAR_CX2X_CLAW "${BUILD_DIR}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_BUILD_JAR_CX2X_XCODEML "${BUILD_DIR}/${OMNI_CX2X_XCODEML_NAME}.jar")

# Define CLAW jar archives install location.
set(CX2X_JAR_INSTALL_LOCATION "${CMAKE_INSTALL_PREFIX}/share/claw")
set(OMNI_JAR_CX2X_CLAW "${CX2X_JAR_INSTALL_LOCATION}/${OMNI_CX2X_CLAW_NAME}.jar")
set(OMNI_JAR_CX2X_XCODEML "${CX2X_JAR_INSTALL_LOCATION}/${OMNI_CX2X_XCODEML_NAME}.jar")

# Define OMNI Compiler jar archives build location.
set(LOCAL_OMNI_JAR_TOOLS "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar")
set(LOCAL_OMNI_JAR_F_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar")
set(LOCAL_OMNI_JAR_C_BACKEND "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar")

# Define third-party dependency names
set(ANTLR4_NAME "antlr4")
set(ANTLR4_RUNTIME_NAME "antlr4-runtime")
set(ANTLR_RUNTIME_NAME "antlr-runtime")

set(ANTLR4 "${CX2X_JAR_INSTALL_LOCATION}/${ANTLR4_NAME}.jar")
set(ANTLR4_RUNTIME "${CX2X_JAR_INSTALL_LOCATION}/${ANTLR4_RUNTIME_NAME}.jar")

set(COMMON_CLI_NAME "commons-cli")
set(COMMON_CLI "${CX2X_JAR_INSTALL_LOCATION}/${COMMON_CLI_NAME}.jar")

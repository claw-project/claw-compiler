# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file centralize variables used in the different build files of
# the project.

#
# CLAW X2T variables
#

# Libraries names and paths
set(CLAW_X2T_TATSU "claw-x2t-tatsu")
set(CLAW_X2T_SHENRON "claw-x2t-shenron")
set(CLAW_X2T_WANI "claw-x2t-wani")
set(CLAW_X2T_JAR_INSTALL_PATH "${CMAKE_INSTALL_PREFIX}/share/claw")
set(CLAW_X2T_TATSU_JAR "${CLAW_X2T_JAR_INSTALL_PATH}/${CLAW_X2T_TATSU}.jar")
set(CLAW_X2T_SHENRON_JAR "${CLAW_X2T_JAR_INSTALL_PATH}/${CLAW_X2T_SHENRON}.jar")
set(CLAW_X2T_WANI_JAR "${CLAW_X2T_JAR_INSTALL_PATH}/${CLAW_X2T_WANI}.jar")

# Configurations files and paths
set(CLAW_CONFIG_FILE "claw-default.xml")
set(CLAW_CONFIG_XSD "claw_config.xsd")
set(CLAW_CONFIG_SET_XSD "claw_transformation_set.xsd")
set(CLAW_TRANS_SET_INTERNAL "claw-internal-set.xml")
set(CLAW_TRANS_SET_LOW "claw-low-level-set.xml")
set(CLAW_TRANS_SET_HIGH "claw-high-level-set.xml")
set(CLAW_X2T_CONFIG_PATH "${CMAKE_INSTALL_PREFIX}/etc/")

# Driver files
set(CLAW_CONF_FILE "claw_f.conf")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAW_LIB_SH "claw_f_lib.sh")
set(CLAW_X2T_DRIVER_LIB_DIR "${CMAKE_INSTALL_PREFIX}/libexec/")

#
# OMNI Compiler variables
#
set(OMNI_HOME "${CMAKE_INSTALL_PREFIX}")
set(OMNI_CLASSPATH "${OMNI_HOME}/share/xcalablemp")
set(OMNI_DRIVER_DIR "${OMNI_HOME}/libexec")
set(OMNI_INCLUDE_DIR "${OMNI_HOME}/include")
set(OMNI_XMOD_GENERIC "${OMNI_HOME}/fincludes")
set(OMNI_BIN_DIR "${OMNI_HOME}/bin")
set(OMNI_F_FRONT "${OMNI_BIN_DIR}/F_Front")
set(OMNI_C_FRONT "${OMNI_BIN_DIR}/C_Front")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")
set(OMNI_F2X_FLAGS "")

# Common module files
set(CLAW_XMOD_GENERIC "${OMNI_HOME}/fincludes")

# Define OMNI Compiler jar archives build location.
set(
  BUILD_OMNI_JAR_TOOLS
  "${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar"
)
set(
  BUILD_OMNI_JAR_F_BACKEND
  "${CMAKE_SOURCE_DIR}/omni-compiler/F-BackEnd/build/om-f-back.jar"
)
set(
  BUILD_OMNI_JAR_C_BACKEND
  "${CMAKE_SOURCE_DIR}/omni-compiler/C-BackEnd/build/om-c-back.jar"
)


#
# Third party libraries
#
set(ANTLR4_NAME "antlr4")
set(ANTLR4_RUNTIME_NAME "antlr4-runtime")
set(ANTLR_RUNTIME_NAME "antlr-runtime")

set(ANTLR4 "${CLAW_X2T_JAR_INSTALL_PATH}/${ANTLR4_NAME}.jar")
set(ANTLR4_RUNTIME "${CLAW_X2T_JAR_INSTALL_PATH}/${ANTLR4_RUNTIME_NAME}.jar")

set(COMMON_CLI_NAME "commons-cli")
set(COMMON_CLI "${CLAW_X2T_JAR_INSTALL_PATH}/${COMMON_CLI_NAME}.jar")

set(BUILD_ANTLR4 "${CMAKE_SOURCE_DIR}/cx2t/lib/${ANTLR4_NAME}.jar")
set(
  BUILD_ANTLR4_RUNTIME
  "${CMAKE_SOURCE_DIR}/cx2t/lib/${ANTLR4_RUNTIME_NAME}.jar"
)
set(BUILD_COMMON_CLI "${CMAKE_SOURCE_DIR}/cx2t/lib/${COMMON_CLI_NAME}.jar")

#
# External program options
#
if(${Java_VERSION} VERSION_EQUAL "1.7")
  set(JAVA_OPT "-Xmx200m -Xms200m")
endif()

set(FPP "${CMAKE_Fortran_COMPILER}")
set(CPP_OPT "${FPPFLAGS}")

set(ANT_FLAGS "-quiet")

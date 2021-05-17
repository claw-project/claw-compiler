# This file is released under terms of BSD license
# See LICENSE file for more information

# This CMake file centralize variables used in the different build files of
# the project.

set(CLAW_JAVA_TARGET "1.8")

#
# CLAW X2T variables
#

# Intermediate target directory used during build
set(INT_CLAW_HOME "${CMAKE_BINARY_DIR}/home")
set(TEST_REPORTS_DIR "${CMAKE_BINARY_DIR}/test-reports")
file(MAKE_DIRECTORY ${TEST_REPORTS_DIR})

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
set(CLAW_CFG_DIR_NAME "etc")
set(CLAW_X2T_CONFIG_PATH "${CMAKE_INSTALL_PREFIX}/${CLAW_CFG_DIR_NAME}")

# Driver files
set(CLAWFC_JAR "clawfc.jar")
set(CLAW_COMPILER_FILE "clawfc")
set(CLAWFC_SRC_DIR "${CMAKE_SOURCE_DIR}/driver/src")
set(CLAWFC_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/driver/build/generated-src")
set(CLAWFC_CLASSES_DIR "${CMAKE_BINARY_DIR}/driver/classes")
set(CLAW_X2T_CONFIG_DIR "${CMAKE_BINARY_DIR}/driver/etc")
# Config at build-time
set(CLAWFC_CONF_FILE "clawfc.properties")
# Config at runtime (after install)
set(CLAWFC_RUNTIME_CONF_FILE "clawfc.runtime.properties")
set(CLAWFC_DIST_DIR "${INT_CLAW_HOME}/bin")

# CLAW driver test utils
set(CLAWFC_TEST_UTIL_SRC_DIR "${CMAKE_SOURCE_DIR}/driver/test_utils/src")

# Driver unit-tests
set(CLAWFC_UT_JAR "clawfc-ut.jar")
set(CLAWFC_UT_SRC_DIR "${CMAKE_SOURCE_DIR}/driver/unittests/src")
set(CLAWFC_UT_RES_DIR "${CMAKE_SOURCE_DIR}/driver/unittests/res")
set(CLAWFC_UT_BIN_DIR "${CMAKE_BINARY_DIR}/driver/ut/bin")
set(CLAWFC_UT_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/driver/ut/build/generated-src")
set(CLAWFC_UT_CLASSES_DIR "${CMAKE_BINARY_DIR}/driver/ut/build/classes")
set(CLAWFC_UT_REPORT "${TEST_REPORTS_DIR}/clawfc-ut.txt")

# Driver tests
set(CLAWFC_TESTS_JAR "clawfc-tests.jar")
set(CLAWFC_TESTS_SRC_DIR "${CMAKE_SOURCE_DIR}/driver/tests/src")
set(CLAWFC_TESTS_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/driver/tests/build/generated-src")
set(CLAWFC_TESTS_RES_DIR "${CMAKE_SOURCE_DIR}/driver/tests/res")
set(CLAWFC_TESTS_CLASSES_DIR "${CMAKE_BINARY_DIR}/driver/tests/classes")
set(CLAWFC_TESTS_BIN_DIR "${CMAKE_BINARY_DIR}/driver/tests/bin")
set(CLAWFC_TESTS_REPORT "${TEST_REPORTS_DIR}/clawfc.txt")

# CLAW tests
set(CLAW_TESTS_JAR "claw-tests.jar")
set(CLAW_TESTS_SRC_DIR "${CMAKE_SOURCE_DIR}/tests_runner/src")
set(CLAW_TESTS_GEN_SRC_DIR "${CMAKE_BINARY_DIR}/claw/tests/build/generated-src")
set(CLAW_TESTS_RES_DIR "${CMAKE_SOURCE_DIR}/test")
set(CLAW_TESTS_CLASSES_DIR "${CMAKE_BINARY_DIR}/claw/tests/classes")
set(CLAW_TESTS_BIN_DIR "${CMAKE_BINARY_DIR}/claw/tests/bin")
set(CLAW_TESTS_DEFAULT_WORKING_DIR "${CMAKE_BINARY_DIR}/claw/tests/run_data")
set(CLAW_TESTS_REPORT "${TEST_REPORTS_DIR}/claw.txt")

#
# OMNI Compiler variables
#
set(OMNI_GIT_COMMIT_HASH "6a317fa038ad4eb9e34a23a8682a170cf011fd5c" CACHE STRING
    "OMNI compiler tools GIT commit hash")
set(OMNI_GIT_REPOSITORY "https://github.com/claw-project/xcodeml-tools.git" CACHE STRING
    "OMNI compiler tools GIT repository URL")
set(OMNI_GIT_BRANCH "master" CACHE STRING "OMNI compiler tools GIT repository branch")
set(OMNI_VERSION_TAG ${OMNI_GIT_COMMIT_HASH})
if(BUILD_OMNI_XCODEML_TOOLS)
    if(ADD_OMNI_XCODEML_TOOLS_TO_INSTALL)
        set(OMNI_HOME "${INT_CLAW_HOME}")
    else()
        set(OMNI_HOME "${CMAKE_BINARY_DIR}/omni-compiler-install")
    endif(ADD_OMNI_XCODEML_TOOLS_TO_INSTALL)
else()
    if(NOT DEFINED OMNI_HOME)
        message(FATAL_ERROR "When BUILD_OMNI_XCODEML_TOOLS is off, OMNI_HOME variable must be set to path to the xcodeml-tools install")
    endif()
endif(BUILD_OMNI_XCODEML_TOOLS)
set(OMNI_CLASSPATH "${OMNI_HOME}/share")
if(ADD_OMNI_XCODEML_TOOLS_TO_INSTALL)
    set(OMNI_XCODEML_TOOLS_INSTALL_DIR ${CMAKE_INSTALL_PREFIX})
    set(OMNI_INSTALL_CLASSPATH "../share")
else()
    set(OMNI_XCODEML_TOOLS_INSTALL_DIR ${OMNI_HOME})
    set(OMNI_INSTALL_CLASSPATH ${OMNI_CLASSPATH})
endif(ADD_OMNI_XCODEML_TOOLS_TO_INSTALL)
set(OMNI_DRIVER_DIR "${OMNI_HOME}/libexec")
set(OMNI_XMOD_GENERIC "${OMNI_HOME}/fincludes")
set(OMNI_BIN_DIR_NAME "bin")
set(OMNI_BIN_DIR "${OMNI_HOME}/${OMNI_BIN_DIR_NAME}")
set(OMNI_F_FRONT_NAME "ffront-cpp")
set(OMNI_F_FRONT "${OMNI_BIN_DIR}/${OMNI_F_FRONT_NAME}")
set(OMNI_JAR_TOOLS "${OMNI_CLASSPATH}/om-common.jar")
set(OMNI_JAR_F_BACKEND "${OMNI_CLASSPATH}/om-f-back.jar")
set(OMNI_JAR_C_BACKEND "${OMNI_CLASSPATH}/om-c-back.jar")
set(OMNI_JAR_JNI "${OMNI_CLASSPATH}/ffront.jar")
set(OMNI_F2X_FLAGS "")

execute_process(
    COMMAND ${OMNI_F_FRONT} --version
    WORKING_DIRECTORY ${OMNI_BIN_DIR}
    OUTPUT_VARIABLE OMNI_VERSION_STRING
    ERROR_VARIABLE OMNI_VERSION_STRING
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )

# Common module files
set(CLAW_XMOD_GENERIC "${OMNI_HOME}/fincludes")


# CX2T
set(CX2T_DIR "${CMAKE_SOURCE_DIR}/cx2t")

set(CX2T_BUILD_DIR "${CMAKE_BINARY_DIR}/cx2t")

set(CX2T_COMMON_DIR "${CMAKE_SOURCE_DIR}/cx2t")
set(CX2T_3RDPARTY_DIST_DIR "${CX2T_BUILD_DIR}/3rdparty/dist")

set(CX2T_SRC_DIR "${CMAKE_SOURCE_DIR}/cx2t/src")
set(CX2T_GEN_SRC_DIR "${CX2T_BUILD_DIR}/build/generated-src")
set(CX2T_CLASSES_DIR "${CX2T_BUILD_DIR}/build/classes")
set(CX2T_DIST_DIR "${INT_CLAW_HOME}/share/claw")

set(CX2T_UT_SRC_DIR "${CMAKE_SOURCE_DIR}/cx2t/unittest")
set(CX2T_UT_GEN_SRC_DIR "${CX2T_BUILD_DIR}/unit-tests/generated-src")
set(CX2T_UT_CLASSES_DIR "${CX2T_BUILD_DIR}/unit-tests/classes")
set(CX2T_UT_DIST_DIR "${CX2T_BUILD_DIR}/unit-tests/dist")
set(CX2T_UT_REPORTS_DIR "${CX2T_BUILD_DIR}/unit-tests/reports")
set(CX2T_PROPERTIES_DIR "${CX2T_BUILD_DIR}/common")

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
set(BUILD_COMMON_CLI "${CX2T_3RDPARTY_DIST_DIR}/${COMMON_CLI_NAME}.jar")

set(BUILD_ANTLR4 "${CX2T_3RDPARTY_DIST_DIR}/${ANTLR4_NAME}.jar")
set( BUILD_ANTLR4_RUNTIME "${CX2T_3RDPARTY_DIST_DIR}/${ANTLR4_RUNTIME_NAME}.jar")

set(TOML_NAME "cava-toml")
set(TOML "${CLAW_X2T_JAR_INSTALL_PATH}/${TOML_NAME}.jar")
set(BUILD_TOML "${CX2T_3RDPARTY_DIST_DIR}/${TOML_NAME}.jar")

set(ARGPARSE_NAME "argparse4j")

set(FPP "${CMAKE_Fortran_COMPILER}")
set(CPP_OPT "${FPPFLAGS}")

set(ANT_FLAGS "-quiet")

# Ant projects
set(ANT_SOLUTION "${CMAKE_SOURCE_DIR}/ant-solution.xml")

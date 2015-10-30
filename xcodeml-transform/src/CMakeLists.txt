set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )

add_jar(omni-x2x
  x2x/ClawX2X.java
  x2x/translator/CLAWtranslator.java
  x2x/translator/CLAWblockVisitor.java
  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

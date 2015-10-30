set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )

add_jar(omni-x2x
  x2x/ClawX2X.java
  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

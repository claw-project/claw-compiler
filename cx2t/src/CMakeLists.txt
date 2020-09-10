# This file is released under terms of BSD license
# See LICENSE file for more information

# CLAW X2T libraries

# Generate file with version number for Configuration check
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/claw/ClawVersion.java.in
  ${CX2T_GEN_SRC_DIR}/claw/ClawVersion.java
  @ONLY
)

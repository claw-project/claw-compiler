# This file is released under terms of BSD license
# See LICENSE file for more information

# Compilation and execution of JUnit test cases.

# Configure files depending on a path
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java
  @ONLY
)

# Add target for the compilation of JUnit test cases with ANT
add_custom_target(omni-cx2x-unittest ALL)
add_custom_command(TARGET omni-cx2x-unittest COMMAND ant)

# Add test cases
add_test(NAME junit COMMAND ant junit)

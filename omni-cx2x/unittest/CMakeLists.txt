# This file is released under terms of BSD license
# See LICENSE file for more information

# Compilation and execution of JUnit test cases.

# Configure files depending on a path
configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java.in
  ${CMAKE_CURRENT_SOURCE_DIR}/helper/TestConstant.java
  @ONLY
)

# Bootstrap the compilation process
add_custom_target(omni-cx2x-ut-init ALL)
add_custom_command(TARGET omni-cx2x-ut-init COMMAND ${Ant_EXECUTABLE} init)

# Add target for the compilation of JUnit test cases with ANT
add_custom_target(omni-cx2x-ut ALL)
add_custom_command(TARGET omni-cx2x-ut COMMAND ant)
add_dependencies(omni-cx2x-ut omni-cx2x-ut-init)

# Add test cases
add_test(NAME junit COMMAND ${Ant_EXECUTABLE} junit)

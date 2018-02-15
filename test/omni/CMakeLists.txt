set(CLAW_FLAGS_constant_kind --force)
set(CLAW_FLAGS_TARGET_CPU_constant_kind --directive=none)
set(CLAW_FLAGS_TARGET_GPU_constant_kind --directive=none)

claw_add_advanced_test_set(
  NAME omni
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

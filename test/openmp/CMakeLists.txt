# OpenMP related test set

# Set some specific CLAW Compiler flag for some tests
set(CLAW_FLAGS_primitive1 "--target=cpu --directive=openmp")

add_basic_test_set(NAME openmp DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

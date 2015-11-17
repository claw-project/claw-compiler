cmake_minimum_required(VERSION 2.8)

enable_language (Fortran)
enable_testing ()

# Set up some common env variable for the transformation flow
set (OMNIJAR_INCLUDE /usr/local/lib/om-common.jar:/usr/local/lib/om-f-back.jar:/usr/local/lib/om-exc-tools.jar:../../../build/omni-x2x.jar)
set (OMNI_F_FRONTEND F_Front)
set (OMNI_X2X x2x.ClawX2X)
set (OMNI_F_BACKEND xcodeml.f.util.omx2f)

# List of test directories
add_subdirectory(loops)

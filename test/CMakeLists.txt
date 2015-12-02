enable_language (Fortran)

# Set up some common env variable for the transformation flow
set (OMNIJAR_INCLUDE /usr/local/lib/om-common.jar:/usr/local/lib/om-f-back.jar:/usr/local/lib/om-exc-tools.jar:../../../build/omni-cx2x.jar)
set (OMNI_F_FRONTEND F_Front)
set (OMNI_F_FRONTEND_OPTION -fleave-comment)
set (OMNI_X2X x2x.ClawX2X)
set (OMNI_F_BACKEND xcodeml.f.util.omx2f)

set(XCODEML_EXT .xcodeml)
set(CLAW_EXT ${XCODEML_EXT}.claw)

# List of test directories
add_subdirectory(loops)
add_subdirectory(fortran)

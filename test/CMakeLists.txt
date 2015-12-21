enable_language (Fortran)

# Set up some common env variable for the transformation flow
#set (OMNIJAR_INCLUDE ${OMNI_JAR_COMMON}:${OMNI_JAR_F_BACKEND}:${OMNI_JAR_TOOLS}:${OMNI_BUILD_JAR_CX2X})
#set (OMNI_F_FRONTEND_OPTION -fleave-comment)
#set (OMNI_X2X x2x.ClawX2X)
#set (OMNI_F_BACKEND xcodeml.f.util.omx2f)
set (CLAWF90 clawf90)

#set(XCODEML_EXT .xcodeml)
#set(CLAW_EXT ${XCODEML_EXT}.claw)

# List of test directories
add_subdirectory(loops)
add_subdirectory(fortran)

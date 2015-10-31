cmake_minimum_required(VERSION 2.8)

set (ORIGINAL_FILE original_code.f90)
set (XCODEML_FILE original_code.f90.xcodeml)
set (XCODEML_CLAW_FILE original_code.f90.xcodeml.claw)
set (OUTPUT_FILE transformed_code.f90)

set (OMNIJAR_INCLUDE /usr/local/lib/om-common.jar:/usr/local/lib/om-f-back.jar:/usr/local/lib/om-exc-tools.jar:../../../build/omni-x2x.jar)

set (OMNI_F_FRONTEND F_Front)
set (OMNI_X2X x2x.ClawX2X)
set (OMNI_F_BACKEND xcodeml.f.util.omx2f)


# Create intermediate representation in XcodeML Fortran format
add_custom_command(
  OUTPUT  ${XCODEML_FILE}
  COMMAND ${OMNI_F_FRONTEND} -o ${XCODEML_FILE} ${ORIGINAL_FILE}
  DEPENDS ${ORIGINAL_FILE}
  COMMENT "Generating XcodeML file")

# Manipulate the intermediate representation and output XcodeML Fortran
add_custom_command(
  OUTPUT  ${XCODEML_CLAW_FILE}
  COMMAND java -cp ${OMNIJAR_INCLUDE} ${OMNI_X2X} -xcodeml -o ${XCODEML_CLAW_FILE} ${XCODEML_FILE}
  DEPENDS ${XCODEML_FILE}
  COMMENT "Apply XcodeML transformation")

# Decompile XcodeML Fortran to standard Fortran
add_custom_command(
  OUTPUT  ${OUTPUT_FILE}
  COMMAND java -cp ${OMNIJAR_INCLUDE} ${OMNI_F_BACKEND} -l -w 80 -o ${OUTPUT_FILE} ${XCODEML_CLAW_FILE}
  DEPENDS ${XCODEML_CLAW_FILE}
  COMMENT "Decompile XcodeML to Fortran")

add_custom_target(transform ALL
  DEPENDS ${OUTPUT_FILE})

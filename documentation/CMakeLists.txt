# Generate documentation
find_package(LATEX)
if(NOT ${PDFLATEX_COMPILER} STREQUAL "")
  set(CLAW_DOC_FILE "claw_fortran_compiler")
  add_custom_target(
    claw-latex
    COMMAND ${PDFLATEX_COMPILER} ${CLAW_DOC_FILE}.tex &>/dev/null
    COMMENT "Generating documentation"
    SOURCES ${CLAW_DOC_FILE}.tex
  )
endif()

# Build the javadoc
set (JAVADOC_TITLE "OMNI CLAW XcodeML to XcodeML translator (omni-cx2x)")
add_custom_target(
  claw-javadoc
  COMMAND ${Java_JAVADOC_EXECUTABLE}
    -d ${CMAKE_SOURCE_DIR}/documentation/javadoc
    -sourcepath ${CMAKE_SOURCE_DIR}/omni-cx2x/src
    -classpath ${LOCAL_OMNI_JAR_TOOLS}:${LOCAL_OMNI_JAR_COMMON}:${LOCAL_OMNI_JAR_F_BACKEND}:${LOCAL_OMNI_JAR_C_BACKEND}
    -doctitle '<h1>${JAVADOC_TITLE}</h1>'
    -windowtitle '${JAVADOC_TITLE}'
    -author
    -use
    -version
    cx2x
    cx2x.translator
    cx2x.translator.common
    cx2x.translator.misc
    cx2x.translator.pragma
    cx2x.translator.transformation.loop
    cx2x.translator.transformation.openacc
    cx2x.translator.transformation.utility
    cx2x.translator.transformer
    cx2x.xcodeml.error
    cx2x.xcodeml.exception
    cx2x.xcodeml.helper
    cx2x.xcodeml.transformation
    cx2x.xcodeml.xelement
  COMMENT "Generating javadoc"
)

if(NOT ${PDFLATEX_COMPILER} STREQUAL "")
  add_custom_target(doc DEPENDS claw-javadoc claw-latex)
else()
  add_custom_target(doc DEPENDS claw-javadoc)
endif()

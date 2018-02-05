# This file is released under terms of BSD license
# See LICENSE file for more information

# Build the javadoc
set (JAVADOC_TITLE "OMNI CLAW XcodeML to XcodeML translator (cx2t)")
add_custom_target(
  claw-javadoc
  COMMAND ${Java_JAVADOC_EXECUTABLE}
    -d ${CMAKE_SOURCE_DIR}/documentation/javadoc
    -sourcepath ${CMAKE_SOURCE_DIR}/cx2t/src
    -classpath ${LOCAL_OMNI_JAR_TOOLS}:${LOCAL_OMNI_JAR_COMMON}:${LOCAL_OMNI_JAR_F_BACKEND}:${LOCAL_OMNI_JAR_C_BACKEND}:${ANTLR}
    -doctitle '<h1>${JAVADOC_TITLE}</h1>'
    -windowtitle '${JAVADOC_TITLE}'
    -author
    -use
    -version
    cx2x
    cx2x.translator
    cx2x.translator.common
    cx2x.translator.language
    cx2x.translator.misc
    cx2x.translator.transformation.loop
    cx2x.translator.transformation.openacc
    cx2x.translator.transformation.utility
    cx2x.xcodeml.error
    cx2x.xcodeml.exception
    cx2x.xcodeml.helper
    cx2x.xcodeml.transformation
    cx2x.xcodeml.xelement
  COMMENT "Generating javadoc"
)

if(NOT ${PDFLATEX_COMPILER} STREQUAL "")
  add_custom_target(doc DEPENDS claw-javadoc claw-compiler-manual claw-developer-guide)
else()
  add_custom_target(doc DEPENDS claw-javadoc)
endif()

add_subdirectory(developer)
add_subdirectory(manual)

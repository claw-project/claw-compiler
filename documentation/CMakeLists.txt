# Build the javadoc
set (JAVADOC_TITLE "omni-cx2x")
create_javadoc(
  claw-javadoc
  PACKAGES
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
  INSTALLPATH ${CMAKE_SOURCE_DIR}/documentation/javadoc
  SOURCEPATH ${CMAKE_SOURCE_DIR}/omni-cx2x/src
  CLASSPATH
    ${LOCAL_OMNI_JAR_TOOLS}
    ${LOCAL_OMNI_JAR_COMMON}
    ${LOCAL_OMNI_JAR_F_BACKEND}
    ${LOCAL_OMNI_JAR_C_BACKEND}
  WINDOWTITLE ${JAVADOC_TITLE}
  DOCTITLE "<h1>${JAVADOC_TITLE}</h1>"
  AUTHOR TRUE
  USE TRUE
  VERSION TRUE
)
set_target_properties(
  claw-javadoc_javadoc
  PROPERTIES
  EXCLUDE_FROM_ALL 1
)
add_custom_target(doc DEPENDS claw-javadoc_javadoc)

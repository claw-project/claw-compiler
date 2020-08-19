# This file is released under terms of BSD license
# See LICENSE file for more information

# Build the javadoc
set(JAVADOC_TITLE "CLAW X2T (XcodeML to XcodeML Translator)")
add_custom_target(
  claw-javadoc
  COMMAND ${Java_JAVADOC_EXECUTABLE}
    -d ${CMAKE_SOURCE_DIR}/documentation/javadoc
    -sourcepath ${CMAKE_SOURCE_DIR}/cx2t/src
    -classpath ${OMNI_JAR_TOOLS}:${BUILD_OMNI_JAR_F_BACKEND}:${BUILD_OMNI_JAR_C_BACKEND}:${BUILD_ANTLR4}:${BUILD_ANTLR4_RUNTIME}:${BUILD_COMMON_CLI}
    -doctitle '<h1>${JAVADOC_TITLE}</h1>'
    -windowtitle '${JAVADOC_TITLE}'
    -author
    -use
    -version
    claw
    claw.tatsu
    claw.tatsu.analysis.dependency
    claw.tatsu.common
    claw.tatsu.directive.common
    claw.tatsu.directive.generator
    claw.tatsu.directive.generator.openacc
    claw.tatsu.primitive
    claw.tatsu.xcodeml.abstraction
    claw.tatsu.xcodeml.backend
    claw.tatsu.xcodeml.error
    claw.tatsu.xcodeml.exception
    claw.tatsu.xcodeml.module
    claw.tatsu.xcodeml.xnode
    claw.tatsu.xcodeml.xnode.common
    claw.tatsu.xcodeml.xnode.fortran
    claw.shenron.transformation
    claw.shenron.translator
    claw.wani
    claw.wani.language
    claw.wani.language.parser
    claw.wani.report
    claw.wani.transformation
    claw.wani.transformation.internal
    claw.wani.transformation.ll.caching
    claw.wani.transformation.ll.directive
    claw.wani.transformation.ll.loop
    claw.wani.transformation.ll.utility
    claw.wani.transformation.sca
    claw.wani.x2t.configuration
    claw.wani.x2t.configuration.openacc
    claw.wani.x2t.translator
  COMMENT "Generating javadoc"
)

if(NOT ${PDFLATEX_COMPILER} STREQUAL "")
  add_custom_target(doc DEPENDS claw-javadoc claw-compiler-manual claw-developer-guide)
else()
  add_custom_target(doc DEPENDS claw-javadoc)
endif()

add_subdirectory(developer)
add_subdirectory(manual)

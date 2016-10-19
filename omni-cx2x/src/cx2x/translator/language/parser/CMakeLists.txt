# Generate the ANTLR Parser
add_custom_target(claw-antlr ALL
  COMMAND ${Java_JAVA_EXECUTABLE} -classpath ${ANTLR_JAR} org.antlr.v4.Tool -o . -package cx2x.translator.language ${CMAKE_CURRENT_SOURCE_DIR}/Claw.g4
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/Claw.g4
)

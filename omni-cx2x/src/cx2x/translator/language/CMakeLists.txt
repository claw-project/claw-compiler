# Generate the ANTLR Parser
add_custom_target(antlr ALL
  COMMAND ${Java_JAVA_EXECUTABLE} -classpath ${ANTLR_JAR} org.antlr.v4.Tool -o . -package cx2x.translator.language Claw.g4
  DEPENDS Claw.g4
)

include(junit.cmake)

set(JUNIT_VERSION "4.12")
set(HAMCREST_VERSION "1.3")

set(OMNICX2X_JAR1 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar)
set(OMNICX2X_JAR2 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar)
# TODO detect jar and do not build if not present (use find_jar)
set(JUNIT_JAR /usr/local/lib/junit-${JUNIT_VERSION}.jar)
set(HAMCREST_JAR /usr/local/lib/hamcrest-core-${HAMCREST_VERSION}.jar)

if(NOT EXISTS ${JUNIT_JAR})
  message( FATAL_ERROR "JUnit jar is not present (${JUNIT_JAR}). CMake will exit." )
endif()

if(NOT EXISTS ${HAMCREST_JAR})
  message( FATAL_ERROR "Hamcrest jar is not present (${HAMCREST_JAR}). CMake will exit." )
endif()

set(CMAKE_JAVA_INCLUDE_PATH
  ${OMNICX2X_JAR1}
  ${OMNICX2X_JAR2}
  ${JUNIT_JAR}
  ${HAMCREST_JAR}
)

add_jar(omni-cx2x-unittest

  helper/XmlHelper.java
  pragma/ClawPragmaTest.java
  xelement/XcodeProgTest.java
  xelement/XidTest.java
  xelement/XexprModelTest.java
  xelement/XvarRefTest.java
  xelement/XvarTest.java

  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

get_target_property(UNITTEST_JAR omni-cx2x-unittest JAR_FILE)

# Add unit test classes

add_junit_test(xelement
   CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
   TESTS xelement.XidTest xelement.XvarTest xelement.XcodeProgTest xelement.XexprModelTest xelement.XvarRefTest
)

add_junit_test(pragma
   CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
   TESTS pragma.ClawPragmaTest
)

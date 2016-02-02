include(junit.cmake)

set(JUNIT_VERSION "4.12")
set(HAMCREST_VERSION "1.3")



set(OMNICX2X_JAR1 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar)
set(OMNICX2X_JAR2 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar)
set(JUNIT_JAR ${PROJECT_BINARY_DIR}/build/junit-${JUNIT_VERSION}.jar)
set(HAMCREST_JAR ${PROJECT_BINARY_DIR}/build/hamcrest-core-${HAMCREST_VERSION}.jar)
# Try to download the jar dependencies

if(NOT EXISTS ${JUNIT_JAR})
  message("Try to download junit dependency")
  file(
    DOWNLOAD
    http://search.maven.org/remotecontent?filepath=junit/junit/4.12/junit-4.12.jar
    ${JUNIT_JAR}
    STATUS status
  )

  list( GET status 0 error_code )
  if( error_code )
    file( REMOVE ${JUNIT_JAR} )
  endif()

endif()

if(NOT EXISTS ${HAMCREST_JAR})
  message("Try to download hamcrest dependency")
  file(
    DOWNLOAD
    http://search.maven.org/remotecontent?filepath=org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar
    ${HAMCREST_JAR}
  )

  list( GET status 0 error_code )
  if( error_code )
    file( REMOVE ${JUNIT_JAR} )
  endif()

endif()

if(NOT EXISTS ${JUNIT_JAR} OR NOT EXISTS ${HAMCREST_JAR})
  message( WARNING "JUnit jar or Hamcrest could not be downloaded. Unit tests will not be built and executed." )
else()

  set(CMAKE_JAVA_INCLUDE_PATH
    ${OMNICX2X_JAR1}
    ${OMNICX2X_JAR2}
    ${JUNIT_JAR}
    ${HAMCREST_JAR}
  )

  add_jar(omni-cx2x-unittest
    helper/XmlHelper.java
    cx2x/translator/misc/UtilityTest.java
    cx2x/translator/pragma/ClawPragmaTest.java
    cx2x/translator/pragma/ClawRangeTest.java
    cx2x/xcodeml/xelement/XbasicTypeTest.java
    cx2x/xcodeml/xelement/XcodeProgTest.java
    cx2x/xcodeml/xelement/XexprModelTest.java
    cx2x/xcodeml/xelement/XfctTypeTest.java
    cx2x/xcodeml/xelement/XidTest.java
    cx2x/xcodeml/xelement/XloopIterationRangeTest.java
    cx2x/xcodeml/xelement/XsymbolsTest.java
    cx2x/xcodeml/xelement/XtypeTableTest.java
    cx2x/xcodeml/xelement/XvarRefTest.java
    cx2x/xcodeml/xelement/XvarTest.java
    OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
  )

  get_target_property(UNITTEST_JAR omni-cx2x-unittest JAR_FILE)

  # Add unit test classes
  add_junit_test(junit-xcodeml.xelement
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
    TESTS
    cx2x.xcodeml.xelement.XbasicTypeTest
    cx2x.xcodeml.xelement.XcodeProgTest
    cx2x.xcodeml.xelement.XexprModelTest
    cx2x.xcodeml.xelement.XfctTypeTest
    cx2x.xcodeml.xelement.XidTest
    cx2x.xcodeml.xelement.XloopIterationRangeTest
    cx2x.xcodeml.xelement.XsymbolsTest
    cx2x.xcodeml.xelement.XtypeTableTest
    cx2x.xcodeml.xelement.XvarRefTest
    cx2x.xcodeml.xelement.XvarTest
  )

  add_junit_test(junit-translator.pragma
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
    TESTS
    cx2x.translator.pragma.ClawPragmaTest
    cx2x.translator.pragma.ClawRangeTest
  )

  add_junit_test(junit-misc.utility
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
    TESTS
    cx2x.translator.misc.UtilityTest
  )

endif()

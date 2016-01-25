include(junit.cmake)

set(JUNIT_VERSION "4.12")
set(HAMCREST_VERSION "1.3")

set(OMNICX2X_JAR1 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar)
set(OMNICX2X_JAR2 ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar)
# TODO detect jar and do not build if not present (use find_jar)
set(JUNIT_JAR /usr/local/lib/junit-${JUNIT_VERSION}.jar)
set(HAMCREST_JAR /usr/local/lib/hamcrest-core-${HAMCREST_VERSION}.jar)



if(NOT EXISTS ${JUNIT_JAR} OR NOT EXISTS ${HAMCREST_JAR})
  message( WARNING "JUnit jar or Hamcrest is not present. Unit test will not be build." )
else()

  set(CMAKE_JAVA_INCLUDE_PATH
    ${OMNICX2X_JAR1}
    ${OMNICX2X_JAR2}
    ${JUNIT_JAR}
    ${HAMCREST_JAR}
  )

  add_jar(omni-cx2x-unittest
    helper/XmlHelper.java
    cx2x/translator/pragma/ClawPragmaTest.java
    cx2x/xcodeml/xelement/XbasicTypeTest.java
    cx2x/xcodeml/xelement/XcodeProgTest.java
    cx2x/xcodeml/xelement/XexprModelTest.java
    cx2x/xcodeml/xelement/XfctTypeTest.java
    cx2x/xcodeml/xelement/XidTest.java
    cx2x/xcodeml/xelement/XsymbolsTest.java
    cx2x/xcodeml/xelement/XvarRefTest.java
    cx2x/xcodeml/xelement/XvarTest.java
    OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
  )

  get_target_property(UNITTEST_JAR omni-cx2x-unittest JAR_FILE)

  # Add unit test classes
  add_junit_test(xcodeml.xelement
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
    TESTS
    cx2x.xcodeml.xelement.XbasicTypeTest
    cx2x.xcodeml.xelement.XcodeProgTest
    cx2x.xcodeml.xelement.XexprModelTest
    cx2x.xcodeml.xelement.XfctTypeTest
    cx2x.xcodeml.xelement.XidTest
    cx2x.xcodeml.xelement.XsymbolsTest
    cx2x.xcodeml.xelement.XvarRefTest
    cx2x.xcodeml.xelement.XvarTest
  )

  add_junit_test(translator.pragma
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH} ${UNITTEST_JAR}
    TESTS
    cx2x.translator.pragma.ClawPragmaTest
  )

endif()

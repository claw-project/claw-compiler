set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )

add_jar(omni-x2x
  x2x/ClawX2X.java
  x2x/translator/xobject/CLAWblockVisitor.java
  x2x/translator/xobject/CLAWglobalDecl.java
  x2x/translator/xobject/CLAWtranslator.java
  x2x/translator/pragma/CLAWanalyzeLocalPragma.java
  x2x/translator/pragma/CLAWanalyzePragma.java
  x2x/translator/pragma/CLAWpragma.java
  x2x/translator/xcodeml/CLAWxcodemlTranslator.java
  x2x/translator/xcodeml/CLAWloop.java
  x2x/translator/xcodeml/CLAWloopFusion.java
  x2x/translator/xcodeml/CLAWloopInterchange.java
  x2x/translator/xcodeml/CLAWextract.java
  x2x/translator/xcodeml/CLAWindexRange.java
  x2x/translator/xcodeml/CLAWbound.java
  x2x/translator/xcodeml/CLAWvar.java
  x2x/translator/xcodeml/CLAWloopIterationRange.java
  x2x/translator/xcodeml/CLAWname.java
  x2x/translator/xcodeml/CLAWfct.java
  x2x/translator/xcodeml/CLAWfctDef.java
  x2x/translator/xcodeml/CLAWfctCall.java
  x2x/translator/xcodeml/CLAWelementHelper.java
  x2x/translator/xcodeml/XelementName.java
  x2x/translator/xcodeml/XcodemlDocument.java
  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

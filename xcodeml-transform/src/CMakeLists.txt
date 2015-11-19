set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )

add_jar(omni-x2x
  x2x/ClawX2X.java
  x2x/translator/xobject/CLAWblockVisitor.java
  x2x/translator/xobject/CLAWglobalDecl.java
  x2x/translator/xobject/CLAWtranslator.java
  x2x/translator/pragma/CLAWanalyzeLocalPragma.java
  x2x/translator/pragma/CLAWanalyzePragma.java
  x2x/translator/pragma/CLAWpragma.java
  x2x/translator/pragma/CLAWmapping.java
  x2x/translator/xcodeml/CLAWxcodemlTranslator.java
  x2x/translator/xcodeml/Xloop.java
  x2x/translator/xcodeml/CLAWloopFusion.java
  x2x/translator/xcodeml/CLAWloopInterchange.java
  x2x/translator/xcodeml/CLAWextract.java
  x2x/translator/xcodeml/XindexRange.java
  x2x/translator/xcodeml/Xbound.java
  x2x/translator/xcodeml/Xvar.java
  x2x/translator/xcodeml/XloopIterationRange.java
  x2x/translator/xcodeml/Xname.java
  x2x/translator/xcodeml/Xfct.java
  x2x/translator/xcodeml/XfctDef.java
  x2x/translator/xcodeml/XfctCall.java
  x2x/translator/xcodeml/XelementHelper.java
  x2x/translator/xcodeml/Xid.java
  x2x/translator/xcodeml/XvarDecl.java
  x2x/translator/xcodeml/XbasicType.java
  x2x/translator/xcodeml/XelementName.java
  x2x/translator/xcodeml/XcodemlDocument.java
  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

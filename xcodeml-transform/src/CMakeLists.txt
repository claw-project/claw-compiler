set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )

add_jar(omni-x2x
  x2x/ClawX2X.java
  x2x/translator/xobject/CLAWblockVisitor.java
  x2x/translator/xobject/CLAWglobalDecl.java
  x2x/translator/xobject/CLAWtranslator.java

# Pragma package
  x2x/translator/pragma/CLAWanalyzeLocalPragma.java
  x2x/translator/pragma/CLAWanalyzePragma.java
  x2x/translator/pragma/CLAWpragma.java
  x2x/translator/pragma/CLAWmapping.java

# XcodeML package
  x2x/translator/xcodeml/CLAWxcodemlTranslator.java

# XcodeML.translation package
  x2x/translator/xcodeml/transformation/DependentTransformationGroup.java
  x2x/translator/xcodeml/transformation/IndependentTransformationGroup.java
  x2x/translator/xcodeml/transformation/LoopFusion.java
  x2x/translator/xcodeml/transformation/LoopInterchange.java
  x2x/translator/xcodeml/transformation/LoopExtraction.java
  x2x/translator/xcodeml/transformation/Transformation.java
  x2x/translator/xcodeml/transformation/TransformationGroup.java

# XcodeML.Xelement package
  x2x/translator/xcodeml/xelement/XargumentsTable.java
  x2x/translator/xcodeml/xelement/XbasicType.java
  x2x/translator/xcodeml/xelement/Xbound.java
  x2x/translator/xcodeml/xelement/XcodeProg.java
  x2x/translator/xcodeml/xelement/XdeclTable.java
  x2x/translator/xcodeml/xelement/XelementHelper.java
  x2x/translator/xcodeml/xelement/XelementName.java
  x2x/translator/xcodeml/xelement/Xfct.java
  x2x/translator/xcodeml/xelement/XfctDef.java
  x2x/translator/xcodeml/xelement/XfctCall.java
  x2x/translator/xcodeml/xelement/XfctType.java
  x2x/translator/xcodeml/xelement/Xid.java
  x2x/translator/xcodeml/xelement/XindexRange.java
  x2x/translator/xcodeml/xelement/Xkind.java
  x2x/translator/xcodeml/xelement/Xloop.java
  x2x/translator/xcodeml/xelement/XloopIterationRange.java
  x2x/translator/xcodeml/xelement/Xname.java
  x2x/translator/xcodeml/xelement/Xstep.java
  x2x/translator/xcodeml/xelement/XsymbolTable.java
  x2x/translator/xcodeml/xelement/Xtype.java
  x2x/translator/xcodeml/xelement/XtypeTable.java
  x2x/translator/xcodeml/xelement/Xvar.java
  x2x/translator/xcodeml/xelement/XvarDecl.java

  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

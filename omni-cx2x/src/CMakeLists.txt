# TODO move dependencies ...
set(CMAKE_JAVA_INCLUDE_PATH ../../../omni-compiler/XcodeML-Common/build/om-common.jar ../../../omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )
#set(CMAKE_JAVA_INCLUDE_PATH ${OMNI_JAR_COMMON} ${OMNI_JAR_TOOLS})
set(CMAKE_JAVA_COMPILE_FLAGS -Xlint:unchecked)


add_jar(${OMNI_CX2X_NAME}
# Base class
  x2x/Cx2x.java

# xobject package
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

# XcodeML.transformation package
  x2x/translator/xcodeml/transformation/DependentTransformationGroup.java
  x2x/translator/xcodeml/transformation/IndependentTransformationGroup.java
  x2x/translator/xcodeml/transformation/LoopFusion.java
  x2x/translator/xcodeml/transformation/LoopInterchange.java
  x2x/translator/xcodeml/transformation/LoopExtraction.java
  x2x/translator/xcodeml/transformation/Transformation.java
  x2x/translator/xcodeml/transformation/TransformationGroup.java
  x2x/translator/xcodeml/transformation/UtilityRemove.java

# XcodeML.transformer
  x2x/translator/xcodeml/transformer/ClawTransformer.java
  x2x/translator/xcodeml/transformer/Transformer.java

# XcodeML.Xelement package
  x2x/xcodeml/xelement/XanalysisError.java
  x2x/xcodeml/xelement/XargumentsTable.java
  x2x/xcodeml/xelement/XarrayConstructor.java
  x2x/xcodeml/xelement/XarrayIndex.java
  x2x/xcodeml/xelement/XarrayRef.java
  x2x/xcodeml/xelement/XbaseElement.java
  x2x/xcodeml/xelement/XbaseExpr.java
  x2x/xcodeml/xelement/XbasicType.java
  x2x/xcodeml/xelement/Xbody.java
  x2x/xcodeml/xelement/Xbound.java
  x2x/xcodeml/xelement/XcharacterConstant.java
  x2x/xcodeml/xelement/Xclonable.java
  x2x/xcodeml/xelement/XcodeProg.java
  x2x/xcodeml/xelement/XcomplexConstant.java
  x2x/xcodeml/xelement/Xcondition.java
  x2x/xcodeml/xelement/Xconstant.java
  x2x/xcodeml/xelement/XconstType.java
  x2x/xcodeml/xelement/XdeclTable.java
  x2x/xcodeml/xelement/XdoStatement.java
  x2x/xcodeml/xelement/Xelse.java
  x2x/xcodeml/xelement/XexprModel.java
  x2x/xcodeml/xelement/XelementHelper.java
  x2x/xcodeml/xelement/XelementName.java
  x2x/xcodeml/xelement/Xfct.java
  x2x/xcodeml/xelement/XfctDef.java
  x2x/xcodeml/xelement/XfctCall.java
  x2x/xcodeml/xelement/XfctType.java
  x2x/xcodeml/xelement/XglobalSymbolTable.java
  x2x/xcodeml/xelement/Xid.java
  x2x/xcodeml/xelement/XifStatement.java
  x2x/xcodeml/xelement/XindexRange.java
  x2x/xcodeml/xelement/XintConstant.java
  x2x/xcodeml/xelement/Xkind.java
  x2x/xcodeml/xelement/Xlength.java
  x2x/xcodeml/xelement/XlogicalConstant.java
  x2x/xcodeml/xelement/XloopIterationRange.java
  x2x/xcodeml/xelement/XlowerBound.java
  x2x/xcodeml/xelement/Xname.java
  x2x/xcodeml/xelement/XrealConstant.java
  x2x/xcodeml/xelement/Xscope.java
  x2x/xcodeml/xelement/Xstep.java
  x2x/xcodeml/xelement/XstructConstructor.java
  x2x/xcodeml/xelement/XsymbolTable.java
  x2x/xcodeml/xelement/Xthen.java
  x2x/xcodeml/xelement/Xtype.java
  x2x/xcodeml/xelement/XtypeTable.java
  x2x/xcodeml/xelement/XupperBound.java
  x2x/xcodeml/xelement/Xvalue.java
  x2x/xcodeml/xelement/Xvar.java
  x2x/xcodeml/xelement/XvarDecl.java
  x2x/xcodeml/xelement/XvarRef.java

# XcodeML.Xelement.Exception package
  x2x/translator/exception/IllegalTransformationException.java

  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)

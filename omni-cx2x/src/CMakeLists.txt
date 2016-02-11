# Build Claw XcodeML to XcodeML translator

set(CMAKE_JAVA_INCLUDE_PATH ${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Common/build/om-common.jar ${CMAKE_SOURCE_DIR}/omni-compiler/XcodeML-Exc-Tools/build/om-exc-tools.jar )
#set(CMAKE_JAVA_INCLUDE_PATH ${OMNI_JAR_COMMON} ${OMNI_JAR_TOOLS})
set(CMAKE_JAVA_COMPILE_FLAGS -Xlint:unchecked)


add_jar(${OMNI_CX2X_XCODEML_NAME}

# XcodeML.transformer
  cx2x/xcodeml/transformation/Transformer.java

# XcodeML.error package
  cx2x/xcodeml/error/XanalysisError.java

# XcodeML.Xelement.Exception package
  cx2x/xcodeml/exception/IllegalDirectiveException.java
  cx2x/xcodeml/exception/IllegalTransformationException.java

# XcodeML.helper package
  cx2x/xcodeml/helper/XelementHelper.java

# XcodeML.Xelement package
  cx2x/xcodeml/xelement/XargumentsTable.java
  cx2x/xcodeml/xelement/XarrayConstructor.java
  cx2x/xcodeml/xelement/XarrayIndex.java
  cx2x/xcodeml/xelement/XarrayRef.java
  cx2x/xcodeml/xelement/XbaseElement.java
  cx2x/xcodeml/xelement/XbaseExpr.java
  cx2x/xcodeml/xelement/XbasicType.java
  cx2x/xcodeml/xelement/XbinaryExpr.java
  cx2x/xcodeml/xelement/Xbody.java
  cx2x/xcodeml/xelement/Xbound.java
  cx2x/xcodeml/xelement/XcharacterConstant.java
  cx2x/xcodeml/xelement/Xclonable.java
  cx2x/xcodeml/xelement/XcodeProgram.java
  cx2x/xcodeml/xelement/XcomplexConstant.java
  cx2x/xcodeml/xelement/Xcondition.java
  cx2x/xcodeml/xelement/Xconstant.java
  cx2x/xcodeml/xelement/XconstType.java
  cx2x/xcodeml/xelement/XdeclTable.java
  cx2x/xcodeml/xelement/XdoStatement.java
  cx2x/xcodeml/xelement/Xelse.java
  cx2x/xcodeml/xelement/XenhancedElement.java
  cx2x/xcodeml/xelement/XexprModel.java
  cx2x/xcodeml/xelement/XelementName.java
  cx2x/xcodeml/xelement/XfunctionCall.java
  cx2x/xcodeml/xelement/XfunctionDefinition.java
  cx2x/xcodeml/xelement/XfunctionType.java
  cx2x/xcodeml/xelement/XglobalDeclTable.java
  cx2x/xcodeml/xelement/XglobalSymbolTable.java
  cx2x/xcodeml/xelement/Xid.java
  cx2x/xcodeml/xelement/XifStatement.java
  cx2x/xcodeml/xelement/Xindex.java
  cx2x/xcodeml/xelement/XindexRange.java
  cx2x/xcodeml/xelement/XintConstant.java
  cx2x/xcodeml/xelement/Xintent.java
  cx2x/xcodeml/xelement/Xkind.java
  cx2x/xcodeml/xelement/Xlength.java
  cx2x/xcodeml/xelement/XlogicalConstant.java
  cx2x/xcodeml/xelement/XloopIterationRange.java
  cx2x/xcodeml/xelement/XlowerBound.java
  cx2x/xcodeml/xelement/XmoduleDefinition.java
  cx2x/xcodeml/xelement/Xname.java
  cx2x/xcodeml/xelement/Xparams.java
  cx2x/xcodeml/xelement/Xpragma.java
  cx2x/xcodeml/xelement/XrealConstant.java
  cx2x/xcodeml/xelement/Xscope.java
  cx2x/xcodeml/xelement/Xstep.java
  cx2x/xcodeml/xelement/XstructConstructor.java
  cx2x/xcodeml/xelement/XsymbolTable.java
  cx2x/xcodeml/xelement/Xthen.java
  cx2x/xcodeml/xelement/Xtype.java
  cx2x/xcodeml/xelement/XtypeTable.java
  cx2x/xcodeml/xelement/XupperBound.java
  cx2x/xcodeml/xelement/Xvalue.java
  cx2x/xcodeml/xelement/Xvar.java
  cx2x/xcodeml/xelement/XvarDecl.java
  cx2x/xcodeml/xelement/XvarRef.java

  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

add_jar(${OMNI_CX2X_CLAW_NAME}
# Base class
  cx2x/Cx2x.java

# translator.common package
  cx2x/translator/common/Constant.java

# translator.misc package
  cx2x/translator/misc/Utility.java

# translator.pragma package
  cx2x/translator/pragma/ClawMapping.java
  cx2x/translator/pragma/ClawMappingVar.java
  cx2x/translator/pragma/ClawPragma.java
  cx2x/translator/pragma/ClawRange.java

# translator package
  cx2x/translator/ClawXcodeMlTranslator.java

# translator.transformer package
  cx2x/translator/transformer/ClawTransformer.java

# translator.transformation package
  cx2x/translator/transformation/loop/LoopFusion.java
  cx2x/translator/transformation/loop/LoopInterchange.java
  cx2x/translator/transformation/loop/LoopExtraction.java
  cx2x/translator/transformation/openacc/OpenAccContinuation.java
  cx2x/translator/transformation/utility/UtilityRemove.java

  OUTPUT_DIR ${PROJECT_BINARY_DIR}/build
)

install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_CLAW_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)
install(FILES ${PROJECT_BINARY_DIR}/build/${OMNI_CX2X_XCODEML_NAME}.jar DESTINATION ${CMAKE_INSTALL_PREFIX}/share/cx2x/)

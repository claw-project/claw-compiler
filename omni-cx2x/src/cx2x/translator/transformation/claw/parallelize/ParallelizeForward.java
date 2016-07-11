/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;

import java.util.List;

/**
 * The parallelize forward transformation applies the changes in the subroutine
 * signatures to function call and function in which the call is nested if
 * needed.
 *
 * @author clementval
 */
public class ParallelizeForward extends Transformation {

  private final ClawLanguage _claw;
  private Xnode _fctCall;
  private XfunctionType _fctType;
  private boolean _localFct = false;
  private boolean _flatten = false;
  private Xmod _mod = null;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public ParallelizeForward(ClawLanguage directive) {
    super(directive);
    _claw = directive; // Keep information about the claw directive here
  }


  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    Xnode next = XnodeUtil.getNextSibling(_claw.getPragma());
    if(next == null){
      xcodeml.addError("Directive is not followed a valid statement.",
          _claw.getPragma().getLineNo());
      return false;
    }
    if(next.Opcode() == Xcode.EXPRSTATEMENT){
      return analyzeForward(xcodeml);
    } else if (next.Opcode() == Xcode.FDOSTATEMENT) {
      return analyzeForwardWithDo(xcodeml);
    }
    xcodeml.addError("Directive is not followed a valid statement.",
        _claw.getPragma().getLineNo());
    return false;
  }

  /**
   * Analyze the directive when it is used just before a do statement.
   * @param xcodeml Current XcodeML file unit.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForwardWithDo(XcodeProgram xcodeml){
    _flatten = true;
    return true;
  }


  /**
   * Analyze the directive when it is used just before a function call.
   * @param xcodeml Current XcodeML file unit.
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeForward(XcodeProgram xcodeml){
    Xnode ex = XnodeUtil.findDirectNext(Xcode.EXPRSTATEMENT, _claw.getPragma());
    if(ex == null){
      xcodeml.addError("Directive is not followed by a fct call.",
          _claw.getPragma().getLineNo());
      return false;
    }

    _fctCall = ex.find(Xcode.FUNCTIONCALL);
    if(_fctCall == null){
      xcodeml.addError("Directive is not followed by a fct call.",
          _claw.getPragma().getLineNo());
      return false;
    }
    String fctCallName = _fctCall.find(Xcode.NAME).getValue();
    String fctType = _fctCall.find(Xcode.NAME).getAttribute(Xattr.TYPE);

    _fctType = (XfunctionType)xcodeml.getTypeTable().get(fctType);
    XfunctionDefinition fctDef = XnodeUtil.findFunctionDefinition(
        xcodeml.getGlobalDeclarationsTable(), fctCallName);
    if(_fctType != null && fctDef != null){
      _localFct = true;
    } else {

      XfunctionDefinition parentFctDef =
          XnodeUtil.findParentFunction(_claw.getPragma());
      List<Xdecl> uses = parentFctDef.getDeclarationTable().getAll(Xcode.FUSEDECL);

      for(Xdecl d : uses){

        // Check whether a CLAW file is available.
        _mod = TransformationHelper.
            locateClawModuleFile(d.getAttribute(Xattr.NAME));

        if(_mod != null){
          if(_mod.getIdentifiers().contains(fctCallName)){
            String type = _mod.getIdentifiers().get(fctCallName).getAttribute(Xattr.TYPE);
            _fctType = (XfunctionType) _mod.getTypeTable().get(type);
            if(_fctType != null){
              return true;
            }
          }
        }
      } // TODO look up in module use statements



      xcodeml.addError("Function signature not found in the current module.",
          _claw.getPragma().getLineNo());
      return false;
    }
    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
    if(_flatten){
      transformFlatten(xcodeml, transformer);
    } else {
      transformStd(xcodeml, transformer);
    }

    // Delete pragma
    _claw.getPragma().delete();
  }

  /**
   * Do the flatten transformation for the forward directive. This
   * transformation adapt the function call nested in the do statements and
   * removes those do statements. The containing subroutine is not adapted.
   * @param xcodeml     Current XcodeML file unit.
   * @param transformer Current transformer.
   * @throws Exception If something goes wrong.
   */
  private void transformFlatten(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {

  }

  /**
   * Do the standard transformation for the forward directive. This
   * transformation adapt the function call and replicates any necessary changes
   * to the containing subroutine.
   * @param xcodeml     Current XcodeML file unit.
   * @param transformer Current transformer.
   * @throws Exception If something goes wrong.
   */
  private void transformStd(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {
    XfunctionDefinition fDef = XnodeUtil.findParentFunction(_claw.getPragma());
    XfunctionType parentFctType = (XfunctionType)xcodeml.getTypeTable().
        get(fDef.getName().getAttribute(Xattr.TYPE));

    List<Xnode> params = _fctType.getParams().getAll();
    List<Xnode> args = _fctCall.find(Xcode.ARGUMENTS).getChildren();

    // 1. Adapt function call with potential new arguments
    for(int i = args.size(); i < params.size(); i++){
      Xnode p = params.get(i);
      String var = p.getValue();
      String type;
      if(!fDef.getSymbolTable().contains(var)){
        // Size variable have to be declared
        XbasicType intTypeIntentIn = XnodeUtil.createBasicType(xcodeml,
            xcodeml.getTypeTable().generateIntegerTypeHash(),
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(intTypeIntentIn);
        XnodeUtil.createIdAndDecl(var,
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, fDef, xcodeml);
        type = intTypeIntentIn.getType();
        XnodeUtil.createAndAddParam(xcodeml, var, type, parentFctType);
      } else {
        // Var exists already. Add to to the parameters if not here.
        type = fDef.getSymbolTable().get(var).getType();
        XnodeUtil.
            createAndAddParamIfNotExists(xcodeml, var, type, parentFctType);
      }

      Xnode arg = XnodeUtil.createVar(type, var, Xscope.LOCAL, xcodeml);
      _fctCall.find(Xcode.ARGUMENTS).appendToChildren(arg, false);
    }

    // 2. Adapt function/subroutine in which the function call is nested
    for(Xnode pBase : _fctType.getParams().getAll()){
      for(Xnode pUpdate : parentFctType.getParams().getAll()){
        if(pBase.getValue().equals(pUpdate.getValue())){
          XbasicType typeBase = (_localFct) ? (XbasicType)
              xcodeml.getTypeTable().get(pBase.getAttribute(Xattr.TYPE)) :
              (XbasicType) _mod.getTypeTable().get(pBase.getAttribute(Xattr.TYPE));
          XbasicType typeToUpdate = (XbasicType)xcodeml.getTypeTable().get(pUpdate.getAttribute(Xattr.TYPE));

          // Types have different dimensions
          if(typeBase.getDimensions() > typeToUpdate.getDimensions()){
            String type = XnodeUtil.duplicateWithDimension(typeBase,
                typeToUpdate, xcodeml);
            pUpdate.setAttribute(Xattr.TYPE, type);

            Xid id = fDef.getSymbolTable().get(pBase.getValue());
            if(id != null){
              id.setAttribute(Xattr.TYPE, type);
            }
            Xdecl varDecl = fDef.getDeclarationTable().get(pBase.getValue());
            if(varDecl != null){
              varDecl.find(Xcode.NAME).setAttribute(Xattr.TYPE, type);
            }
          }
        }
      }
    }

    // 3. Replicate the change in a potential module file
    XmoduleDefinition modDef = XnodeUtil.findParentModule(fDef);
    XnodeUtil.updateModuleSignature(xcodeml, fDef, parentFctType, modDef, _claw,
        transformer);
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // independent transformation
  }
}

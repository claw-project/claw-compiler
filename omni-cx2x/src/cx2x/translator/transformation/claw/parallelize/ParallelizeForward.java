/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.language.ClawLanguage;
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
  private String _fctType;
  private boolean _localFct = false;

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

    _fctType = _fctCall.find(Xcode.NAME).getAttribute(Xattr.TYPE);
    XfunctionType fctType = (XfunctionType)xcodeml.getTypeTable().get(_fctType);
    if(fctType != null){
      _localFct = true;
    } else {
      // TODO check whether the function is defined in another module
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
    XfunctionDefinition fDef = XnodeUtil.findParentFunction(_claw.getPragma());
    XfunctionType parentFctType = (XfunctionType)xcodeml.getTypeTable().
        get(fDef.getName().getAttribute(Xattr.TYPE));

    XfunctionType fctType = (XfunctionType)xcodeml.getTypeTable().get(_fctType);

    List<Xnode> params = fctType.getParams().getAll();
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
        type = fDef.getSymbolTable().get(var).getType();
      }

      Xnode arg = XnodeUtil.createVar(type, var, Xscope.LOCAL, xcodeml);
      _fctCall.find(Xcode.ARGUMENTS).appendToChildren(arg, false);
    }

    // 2. Adapt function/subroutine in which the function call is nested



    // 3. Replicate the change in a potential module file


  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // independent transformation
  }
}

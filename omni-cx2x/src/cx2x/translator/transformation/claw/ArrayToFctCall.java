/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

import java.util.List;

/**
 * An array access to function call transformation replace the access to an
 * array value by a function call.
 *
 * @author clementval
 */
public class ArrayToFctCall extends Transformation {
  private final ClawLanguage _claw;
  private XfunctionDefinition _replaceFct;


  /**
   * ArrayToFctCall ctor.
   *
   * @param directive The directive that triggered the transformation.
   */
  public ArrayToFctCall(AnalyzedPragma directive) {
    super(directive);
    _claw = (ClawLanguage)directive;
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    XfunctionDefinition _fctDef = XelementHelper.findParentFctDef(_claw.getPragma());
    if(_fctDef == null){
      xcodeml.addError("Cannot locate function definition.",
          _claw.getPragma().getLineNo());
      return false;
    }

    if(!_fctDef.getDeclarationTable().contains(_claw.getArrayName())){
      xcodeml.addError(_claw.getArrayName() +
          " is not delcared in current function/subroutine.",
          _claw.getPragma().getLineNo());
      return false;
    }

    _replaceFct = xcodeml.getGlobalDeclarationsTable().
        getFctDefinition(_claw.getFctName());
    if(_replaceFct == null){
      xcodeml.addError("Function " + _claw.getFctName() +
          " not found in current file.",
          _claw.getPragma().getLineNo());
      return false;
    }

    // TODO does it make sense ?
   /*if(_replaceFct.getParams().count() != _claw.getFctParams().size()){
      xcodeml.addError("Function " + _claw.getFctName() +
              " parameters mismatch.",
          _claw.getPragma().getLineNo());
      return false;
    }*/

    return true; // skeleton
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {

    XfunctionType fctType =
        (XfunctionType)xcodeml.getTypeTable().get(_replaceFct.getName().getType());



    // Prepare the function call
    XfunctionCall fctCall = XfunctionCall.create(xcodeml,
        fctType.getReturnType(), _claw.getFctName(),
        _replaceFct.getName().getType());
    XargumentsTable args = fctCall.getArgumentsTable();
    for(String arg : _claw.getFctParams()){
      Xvar var = Xvar.create(XelementName.TYPE_F_INT, arg, Xscope.LOCAL, xcodeml);
      args.add(var);
    }

    List<XarrayRef> refs =
        XelementHelper.getAllArrayReferencesInSiblings(_claw.getPragma(),
            _claw.getArrayName());

    for(XarrayRef ref : refs){
      XelementHelper.insertAfter(ref, fctCall.cloneObject());
      ref.delete();
    }

    fctCall.delete();
    _claw.getPragma().delete();
    this.transformed();
  }
}

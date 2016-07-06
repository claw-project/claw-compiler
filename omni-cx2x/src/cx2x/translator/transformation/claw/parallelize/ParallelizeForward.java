/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

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
    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception {

  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // independent transformation
  }
}

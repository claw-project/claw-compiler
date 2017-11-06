/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.loop;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

/**
 * If extraction transformation
 *
 * @author clementval
 */
public class IfExtract extends ClawTransformation {

  private Xnode _doStmt = null;
  private Xnode _ifStmt = null;

  public IfExtract(ClawLanguage directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    _doStmt = _claw.getPragma().matchSibling(Xcode.FDOSTATEMENT);
    if(_doStmt == null) {
      xcodeml.addError("Do statement missing after directive.",
          _claw.getPragma().lineNo());
      return false;
    }
    _ifStmt = _doStmt.body().matchDirectDescendant(Xcode.FIFSTATEMENT);
    if(_ifStmt == null) {
      xcodeml.addError("If statement not found in the do statement.",
          _claw.getPragma().lineNo());
      return false;
    }
    int counterIfStmt = 0;
    for(Xnode n : _doStmt.body().children()) {
      if(n.opcode() != Xcode.FIFSTATEMENT
          && n.opcode() != Xcode.FPRAGMASTATEMENT)
      {
        xcodeml.addError("If statement is not purely nested in the do statement",
            _claw.getPragma().lineNo());
        return false;
      } else if(n.opcode() == Xcode.FIFSTATEMENT) {
        ++counterIfStmt;
      }
    }
    if(counterIfStmt > 1) {
      xcodeml.addError("Only one if statement can be present for extraction.",
          _claw.getPragma().lineNo());
      return false;
    }
    return true;
  }

  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other) throws Exception
  {
    // Copy the body of the if statement inside the body of the do statement
    Xnode thenBlock = _ifStmt.matchDirectDescendant(Xcode.THEN);
    Xnode thenDoStmt = _doStmt.cloneNode();
    XnodeUtil.appendBody(thenDoStmt.body(), thenBlock.body());

    // Copy the if statement and clean its body
    Xnode newIfStmt = _ifStmt.cloneNode();
    Xnode newThen = newIfStmt.matchDirectDescendant(Xcode.THEN);
    for(Xnode n : newThen.body().children()) {
      n.delete();
    }

    // Add the new if statement after the do statement
    _doStmt.insertAfter(newIfStmt);

    // Insert the do statement in the new if-then statement
    newThen.body().insert(thenDoStmt, false);

    Xnode elseBlock = _ifStmt.matchDirectDescendant(Xcode.ELSE);
    if(elseBlock != null) {
      Xnode elseDoStmt = _doStmt.cloneNode();
      XnodeUtil.appendBody(elseDoStmt.body(), elseBlock.body());
      Xnode newElse = newIfStmt.matchDirectDescendant(Xcode.ELSE);
      for(Xnode n : newElse.body().children()) {
        n.delete();
      }
      newElse.body().insert(elseDoStmt, false);
      Xnode duplicateIf = elseDoStmt.body().matchDirectDescendant(Xcode.FIFSTATEMENT);
      XnodeUtil.safeDelete(duplicateIf);
    }

    // Delete the old statements and pragma
    Xnode duplicateIf =
        thenDoStmt.body().matchDirectDescendant(Xcode.FIFSTATEMENT);
    duplicateIf.delete();
    XnodeUtil.safeDelete(_ifStmt);
    XnodeUtil.safeDelete(_doStmt);
    removePragma();
  }
}

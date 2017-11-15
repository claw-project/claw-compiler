/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.loop;

import cx2x.translator.directive.Directive;
import cx2x.translator.language.ClawPragma;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformation.primitive.Loop;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.NestedDoStatement;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

import java.util.List;

/**
 * A LoopInterchange transformation is a an independent transformation. It allow
 * to reorder nested loops up to three levels.
 *
 * @author clementval
 */

public class LoopInterchange extends ClawTransformation {

  private NestedDoStatement _doStmts = null;

  /**
   * Constructs a new LoopInterchange triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop interchange
   *                  transformation.
   */
  public LoopInterchange(ClawPragma directive) {
    super(directive);
  }

  /**
   * Apply the transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are applied.
   * @param translator     The translator used to applied the transformations.
   * @param transformation Only for dependent transformation. The other
   *                       transformation part of the transformation.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation transformation)
      throws IllegalTransformationException
  {

    analyze(xcodeml, translator);

    Loop.reorder(_doStmts, _claw.getIndexes());

    // Generate directive pragmas if needed
    Directive.generateAdditionalDirectives(_claw, xcodeml,
        _doStmts.getOuterStatement(), _doStmts.getOuterStatement());

    removePragma();
    transformed();
  }

  /**
   * Loop fusion analysis:
   * - Find the different do statement that will be reordered.
   * - Check the validity of the new ordering option.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @return True if the transformation can be performed. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    // Find next loop after pragma
    //_loopLevel0 = _claw.getPragma().matchSibling(Xcode.FDOSTATEMENT);
    Xnode outerDoStatement =
        _claw.getPragma().matchSibling(Xcode.F_DO_STATEMENT);
    if(outerDoStatement == null) {
      xcodeml.addError("top level loop not found",
          _claw.getPragma().lineNo());
      return false;
    }

    int nestedLevel = _claw.getIndexes() != null ?
        _claw.getIndexes().size() : 2;
    _doStmts = new NestedDoStatement(outerDoStatement, nestedLevel);

    if(_claw.getIndexes() != null) {
      if(_claw.getIndexes().size() != 3) {
        xcodeml.addError("new-order option has not enough parameters",
            _claw.getPragma().lineNo());
      }

      List<String> inductions = _doStmts.getInductionVariables();
      for(String idx : _claw.getIndexes()) {
        if(!inductions.contains(idx.toLowerCase())) {
          xcodeml.addError("invalid induction variable in new-order option. "
              + idx, _claw.getPragma().lineNo());
          return false;
        }
      }
    } else {
      if(_doStmts.size() < 2) {
        xcodeml.addError("Not enough nested do statements to reorder",
            _claw.getPragma().lineNo());
      }
    }
    return true;
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    return false; // independent transformation
  }
}

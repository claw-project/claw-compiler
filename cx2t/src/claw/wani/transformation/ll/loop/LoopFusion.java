/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Loop;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.ClawConstant;
import claw.wani.language.ClawConstraint;
import claw.wani.language.ClawPragma;
import claw.wani.language.ClawClause;
import claw.wani.transformation.ClawTransformation;

import java.util.Arrays;
import java.util.Collections;

/**
 * A LoopFusion transformation is a dependent transformation. If two LoopFusion
 * transformation units shared the same fusion group and the same loop iteration
 * information, they can be merge together.
 * The transformation consists of merging the body of the second do statement to
 * the end of the first do statement.
 *
 * @author clementval
 */

public class LoopFusion extends ClawTransformation {

  // Contains the value of the group option
  private String _groupClauseLabel = ClawConstant.EMPTY_STRING;
  // The loop statement involved in the Transformation
  private NestedDoStatement _doStmt;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public LoopFusion(ClawPragma directive) {
    super(directive);
    if(_claw.hasClause(ClawClause.GROUP)) {
      _groupClauseLabel = directive.value(ClawClause.GROUP);
    }
  }

  /**
   * LoopFusion ctor without pragma. Create a LoopFusion dynamically at runtime.
   *
   * @param loop           The do statement to be merge by the loop fusion.
   * @param ghostDirective The generated directive that will be used for the
   *                       loop-fusion transformation.
   */
  public LoopFusion(Xnode loop, ClawPragma ghostDirective) {
    super(ghostDirective);
    _doStmt = new NestedDoStatement(loop, 1);
    if(_claw.hasClause(ClawClause.GROUP)) {
      _groupClauseLabel = ghostDirective.value(ClawClause.GROUP);
    }
    if(_claw.getPragma() != null) {
      setStartLine(ghostDirective.getPragma().lineNo());
    }
  }

  /**
   * Loop fusion analysis:
   * - Without collapse clause: check whether the pragma statement is followed
   * by a do statement.
   * - With collapse clause: Find the n do statements following the pragma.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    Xnode outerLoop = _claw.getPragma().matchSibling(Xcode.F_DO_STATEMENT);
    if(outerLoop == null) {
      xcodeml.addError("Do statement missing after directive.",
          _claw.getPragma().lineNo());
      return false;
    }
    _doStmt = new NestedDoStatement(outerLoop, _claw.getCollapseValue());

    // With collapse clause
    if(_claw.hasClause(ClawClause.COLLAPSE) && _claw.getCollapseValue() > 0
        && _claw.getCollapseValue() > _doStmt.size())
    {
      xcodeml.addError("not enough do statements for collapse value",
          _claw.getPragma().lineNo());
      return false;
    }
    return true;
  }

  /**
   * Apply the loop fusion transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are
   *                       applied.
   * @param translator     The translator used to applied the transformations.
   * @param transformation The other loop fusion unit to be merge with this
   *                       one.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    if(!(transformation instanceof LoopFusion)) {
      throw new IllegalTransformationException("Incompatible transformation",
          _claw.getPragma().lineNo());
    }
    LoopFusion other = (LoopFusion) transformation;

    // Apply different transformation if the collapse clause is used
    if(_claw != null && _claw.hasClause(ClawClause.COLLAPSE)
        && _claw.getCollapseValue() > 0 &&
        _claw.getCollapseValue() > _doStmt.size())
    {
      throw new IllegalTransformationException(
          "Cannot apply transformation, one or both do stmt are invalid.",
          _claw.getPragma().lineNo()
      );
    }

    // Actual merge happens here
    Loop.merge(_doStmt, other.getNestedDoStmt());

    // Clean up
    other.removePragma();
    this.removePragma();

    other.transformed();
  }

  /**
   * Check whether the loop fusion unit can be merged with the given loop fusion
   * unit. To be able to be transformed together, the loop fusion units must
   * share the same parent block, the same iteration range, the same group
   * option and both units must be not transformed.
   *
   * @param transformation The other loop fusion unit to be merge with this one.
   * @return True if the two loop fusion unit can be merge together.
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    if(!(transformation instanceof LoopFusion)) {
      return false;
    }

    LoopFusion other = (LoopFusion) transformation;

    // No loop is transformed already
    if(this.isTransformed() || other.isTransformed()) {
      return false;
    }

    // Loop must share the same group option
    if(!hasSameGroupClause(other)) {
      return false;
    }

    // Loops can only be merged if they are at the same level
    if(!_doStmt.getOuterStatement().hasSameParentBlock(
        other.getNestedDoStmt().getOuterStatement()))
    {
      return false;
    }

    if(!hasCompatibleConstraints(xcodeml, other)) {
      return false;
    }

    if(_claw.hasClause(ClawClause.COLLAPSE) && _claw.getCollapseValue() > 0) {
      for(int i = 0; i < _claw.getCollapseValue(); ++i) {
        if(!Loop.hasSameIndexRange(_doStmt.get(i),
            other.getNestedDoStmt().get(i)))
        {
          return false;
        }
      }
      return true;
    } else {
      // Loop must share the same iteration range
      return Loop.hasSameIndexRange(_doStmt.getOuterStatement(),
          other.getNestedDoStmt().getOuterStatement());
    }
  }

  /**
   * Check compatibility of constraint clause on loop-fusion transformation.
   *
   * @param xcodeml Current translation unit.
   * @param other   The other loop fusion unit to be merge with this one.
   * @return True if the constraints are compatible. False otherwise.
   */
  private boolean hasCompatibleConstraints(XcodeProgram xcodeml,
                                           LoopFusion other)
  {
    ClawConstraint currentConstraint = ClawConstraint.DIRECT;
    if(_claw.hasClause(ClawClause.CONSTRAINT)
        && other.getLanguageInfo().hasClause(ClawClause.CONSTRAINT))
    {
      // Check the constraint clause. Must be identical if set.
      if(_claw.getConstraintClauseValue() !=
          other.getLanguageInfo().getConstraintClauseValue())
      {
        return false;
      }
      currentConstraint = _claw.getConstraintClauseValue();
    } else {
      if(_claw.hasClause(ClawClause.CONSTRAINT)
          || other.getLanguageInfo().hasClause(ClawClause.CONSTRAINT))
      {
        // Constraint are not consistent
        return false;
      }
    }

    // Following constraint are used only in default mode. If constraint clause
    // is set to none, there are note checked.
    if(currentConstraint == ClawConstraint.DIRECT
        && !_doStmt.getOuterStatement().isDirectSibling(
        other.getNestedDoStmt().getOuterStatement(),
        Collections.singletonList(Xcode.F_PRAGMA_STATEMENT)))
    // Only pragma statement can be between the two loops.
    {
      return false;
    } else {
      xcodeml.addWarning("Unconstrained loop-fusion generated",
          Arrays.asList(_claw.getPragma().lineNo(),
              other.getLanguageInfo().getPragma().lineNo()));
    }
    return true;
  }

  /**
   * Return the do statement associated with this loop fusion unit at given
   * depth.
   *
   * @return A do statement.
   */
  private NestedDoStatement getNestedDoStmt() {
    return _doStmt;
  }

  /**
   * Get the group option associated with this loop fusion unit.
   *
   * @return Group option value.
   */
  private String getGroupClauseLabel() {
    return _groupClauseLabel;
  }

  /**
   * Check whether the given loop fusion unit share the same group fusion option
   * with this loop fusion unit.
   *
   * @param otherLoopUnit The other loop fusion unit to be check with this one.
   * @return True if the two loop fusion units share the same group fusion
   * option.
   */
  private boolean hasSameGroupClause(LoopFusion otherLoopUnit) {
    return (otherLoopUnit.getGroupClauseLabel() == null
        ? getGroupClauseLabel() == null
        : otherLoopUnit.getGroupClauseLabel().equals(getGroupClauseLabel()));
  }
}

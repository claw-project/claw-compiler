/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawConstraint;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

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
  private Xnode[] _doStmts;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public LoopFusion(ClawLanguage directive) {
    super(directive);
    if(_claw.hasGroupClause()) {
      _groupClauseLabel = directive.getGroupValue();
    }
    _doStmts = new Xnode[0];
  }

  /**
   * LoopFusion ctor without pragma. Create a LoopFusion dynamically at runtime.
   *
   * @param loop           The do statement to be merge by the loop fusion.
   * @param ghostDirective The generated directive that will be used for the
   *                       loop-fusion transformation.
   */
  public LoopFusion(Xnode loop, ClawLanguage ghostDirective) {
    super(ghostDirective);
    if(_claw.hasCollapseClause()) {
      _doStmts = new Xnode[_claw.getCollapseValue()];
      _doStmts[0] = loop;
      for(int i = 1; i < _claw.getCollapseValue(); ++i) {
        _doStmts[i] = _doStmts[i - 1].body().
            matchDirectDescendant(Xcode.FDOSTATEMENT);
      }
    } else {
      _doStmts = new Xnode[]{loop};
    }

    if(_claw.hasGroupClause()) {
      _groupClauseLabel = ghostDirective.getGroupValue();
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
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // With collapse clause
    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0) {
      _doStmts = new Xnode[_claw.getCollapseValue()];
      for(int i = 0; i < _claw.getCollapseValue(); ++i) {
        if(i == 0) { // Find the outer do statement from pragma
          _doStmts[0] = _claw.getPragma().matchSibling(Xcode.FDOSTATEMENT);
        } else { // Find the next i loops
          _doStmts[i] = _doStmts[i - 1].body().
              matchDirectDescendant(Xcode.FDOSTATEMENT);
        }
        if(_doStmts[i] == null) {
          xcodeml.addError("Do statement missing at depth " + i +
              " after directive.", _claw.getPragma().lineNo());
          return false;
        }
      }
      return true;
    } else {
      // Without collapse clause, locate the do statement after the pragma
      Xnode doStmt = _claw.getPragma().matchSibling(Xcode.FDOSTATEMENT);
      if(doStmt == null) {
        xcodeml.addError("Do statement missing after directive.",
            _claw.getPragma().lineNo());
        return false;
      }
      _doStmts = new Xnode[]{doStmt};
      return true;
    }
  }

  /**
   * Apply the loop fusion transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are
   *                       applied.
   * @param transformer    The transformer used to applied the transformations.
   * @param transformation The other loop fusion unit to be merge with this
   *                       one.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    if(!(transformation instanceof LoopFusion)) {
      throw new IllegalTransformationException("Incompatible transformation",
          _claw.getPragma().lineNo());
    }
    LoopFusion other = (LoopFusion) transformation;

    // Apply different transformation if the collapse clause is used
    if(_claw != null && _claw.hasCollapseClause()
        && _claw.getCollapseValue() > 0)
    {
      // Merge the most inner loop with the most inner loop of the other fusion
      // unit
      int innerDoStmtIdx = _claw.getCollapseValue() - 1;
      if(_doStmts[innerDoStmtIdx] == null
          || other.getDoStmtAtIndex(innerDoStmtIdx) == null)
      {
        throw new IllegalTransformationException(
            "Cannot apply transformation, one or both do stmt are invalid.",
            _claw.getPragma().lineNo()
        );
      }
      XnodeUtil.appendBody(_doStmts[innerDoStmtIdx].body(),
          other.getDoStmtAtIndex(innerDoStmtIdx).body());
    } else {
      // Without collapse clause, only first do statements are considered.
      XnodeUtil.appendBody(_doStmts[0].body(),
          other.getDoStmtAtIndex(0).body());
    }
    other.finalizeTransformation();
    XnodeUtil.safeDelete(_claw.getPragma()); // Delete the pragma statement
  }

  /**
   * Call by the transform method of the master loop fusion unit on the slave
   * loop fusion unit to finalize the transformation. Pragma and loop of the
   * slave loop fusion unit are deleted.
   */
  private void finalizeTransformation() {
    // Delete the pragma of the transformed loop
    XnodeUtil.safeDelete(_claw.getPragma());

    // Delete the do statement that was merged with the main one
    XnodeUtil.safeDelete(_doStmts[0]);

    // Set transformation as done.
    this.transformed();
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

    ClawConstraint currentConstraint = ClawConstraint.DIRECT;
    if(_claw.hasConstraintClause()
        && other.getLanguageInfo().hasConstraintClause())
    {
      // Check the constraint clause. Must be identical if set.
      if(_claw.getConstraintClauseValue() !=
          other.getLanguageInfo().getConstraintClauseValue())
      {
        return false;
      }
      currentConstraint = _claw.getConstraintClauseValue();
    } else {
      if(_claw.hasConstraintClause()
          || other.getLanguageInfo().hasConstraintClause())
      {
        // Constraint are not consistent
        return false;
      }
    }

    // Following constraint are used only in default mode. If constraint clause
    // is set to none, there are note checked.
    if(currentConstraint == ClawConstraint.DIRECT) {
      // Only pragma statement can be between the two loops.
      if(!XnodeUtil.isDirectSibling(_doStmts[0], other.getDoStmtAtIndex(0),
          Collections.singletonList(Xcode.FPRAGMASTATEMENT)))
      {
        return false;
      }
    } else {
      xcodeml.addWarning("Unconstrained loop-fusion generated",
          Arrays.asList(_claw.getPragma().lineNo(),
              other.getLanguageInfo().getPragma().lineNo()));
    }

    // Loops can only be merged if they are at the same level
    if(!XnodeUtil.hasSameParentBlock(_doStmts[0], other.getDoStmtAtIndex(0))) {
      return false;
    }

    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0) {
      for(int i = 0; i < _claw.getCollapseValue(); ++i) {
        if(!XnodeUtil.hasSameIndexRange(_doStmts[i],
            other.getDoStmtAtIndex(i)))
        {
          return false;
        }
      }
      return true;
    } else {
      // Loop must share the same iteration range
      return XnodeUtil.hasSameIndexRange(_doStmts[0],
          other.getDoStmtAtIndex(0));
    }
  }

  /**
   * Return the do statement associated with this loop fusion unit at given
   * depth.
   *
   * @param depth Integer value representing the depth of the loop from the
   *              pragma statement.
   * @return A do statement.
   */
  private Xnode getDoStmtAtIndex(int depth) {
    if(_claw != null && _claw.hasCollapseClause() &&
        depth < _claw.getCollapseValue())
    {
      return _doStmts[depth];
    }
    return _doStmts[0];
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

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

/**
 * A LoopFusion transformation is a dependent transformation. If two LoopFusion
 * transformation units shared the same fusion group and the same loop iteration
 * information, they can be merge together.
 * The transformation consists of merging the body of the second do statement to
 * the end of the first do statement.
 *
 * @author clementval
 */

public class LoopFusion extends Transformation {

  // Contains the value of the group option
  private String _groupLabel = ClawConstant.EMPTY_STRING;
  // The loop statement involved in the Transformation
  private Xnode[] _loops;
  private final ClawLanguage _claw;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public LoopFusion(ClawLanguage directive) {
    super(directive);
    _claw = directive;
    if(_claw.hasGroupClause()) {
      _groupLabel = directive.getGroupValue();
    }
    _loops = new Xnode[0];
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
    _claw = ghostDirective;
    if(_claw.hasCollapseClause()) {
      _loops = new Xnode[_claw.getCollapseValue()];
      _loops[0] = loop;
      for(int i = 1; i < _claw.getCollapseValue(); ++i) {
        _loops[i] = XnodeUtil.find(Xcode.FDOSTATEMENT,
            _loops[i - 1].getBody(), false);
      }
    } else {
      _loops = new Xnode[]{loop};
    }

    if(_claw.hasGroupClause()) {
      _groupLabel = ghostDirective.getGroupValue();
    }
    if(_claw.getPragma() != null) {
      setStartLine(ghostDirective.getPragma().getLineNo());
    }
  }

  /**
   * Loop fusion analysis:
   * - Without collapse clause: find whether the pragma statement is followed
   * by a do statement.
   * - With collapse clause: Find the i loops following the pragma.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // With collapse clause
    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0) {
      _loops = new Xnode[_claw.getCollapseValue()];
      for(int i = 0; i < _claw.getCollapseValue(); ++i) {
        if(i == 0) { // Find the outer loop from pragma
          _loops[0] =
              XnodeUtil.findNext(Xcode.FDOSTATEMENT, _claw.getPragma());
        } else { // Find the next i loops
          _loops[i] = XnodeUtil.find(Xcode.FDOSTATEMENT,
              _loops[i - 1].getBody(), false);
        }
        if(_loops[i] == null) {
          xcodeml.addError("Cannot find loop at depth " + i +
              " after directive", _claw.getPragma().getLineNo());
          return false;
        }
      }
      return true;
    } else {
      // Without collapse clause, just fin the first do statement from the
      // pragma
      Xnode loop =
          XnodeUtil.findNext(Xcode.FDOSTATEMENT, _claw.getPragma());
      if(loop == null) {
        xcodeml.addError("Cannot find loop after directive",
            _claw.getPragma().getLineNo());
        return false;
      }
      _loops = new Xnode[]{loop};
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
          _claw.getPragma().getLineNo());
    }
    LoopFusion loopFusionUnit = (LoopFusion) transformation;

    // Apply different transformation if the collapse clause is used
    if(_claw != null && _claw.hasCollapseClause()
        && _claw.getCollapseValue() > 0) {
      // Merge the most inner loop with the most inner loop of the other fusion
      // unit
      int innerLoopIdx = _claw.getCollapseValue() - 1;
      if(_loops[innerLoopIdx] == null
          || loopFusionUnit.getLoop(innerLoopIdx) == null) {
        throw new IllegalTransformationException(
            "Cannot apply transformation, one or both do stmt are invalid.",
            _claw.getPragma().getLineNo()
        );
      }
      XnodeUtil.appendBody(_loops[innerLoopIdx].getBody(),
          loopFusionUnit.getLoop(innerLoopIdx).getBody());
    } else {
      XnodeUtil.appendBody(_loops[0].getBody(),
          loopFusionUnit.getLoop(0).getBody());
    }
    loopFusionUnit.finalizeTransformation();
  }

  /**
   * Call by the transform method of the master loop fusion unit on the slave
   * loop fusion unit to finalize the transformation. Pragma and loop of the
   * slave loop fusion unit are deleted.
   */
  private void finalizeTransformation() {
    // Delete the pragma of the transformed loop
    if(_claw.getPragma() != null) {
      _claw.getPragma().delete();
    }

    // Delete the loop that was merged with the main one
    if(_loops[0] != null) {
      _loops[0].delete();
    }
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
  public boolean canBeTransformedWith(Transformation transformation) {
    if(!(transformation instanceof LoopFusion)) {
      return false;
    }
    LoopFusion other = (LoopFusion) transformation;

    // No loop is transformed already
    if(this.isTransformed() || other.isTransformed()) {
      return false;
    }
    // Loop must share the same group option
    if(!hasSameGroupOption(other)) {
      return false;
    }

    // Loops can only be merged if they are at the same level
    if(!XnodeUtil.hasSameParentBlock(_loops[0], other.getLoop(0))) {
      return false;
    }

    if(_claw != null && _claw.hasCollapseClause()
        && _claw.getCollapseValue() > 0) {
      for(int i = 0; i < _claw.getCollapseValue(); ++i) {
        if(!XnodeUtil.hasSameIndexRange(_loops[i], other.getLoop(i))) {
          return false;
        }
      }
      return true;
    } else {
      // Loop must share the same iteration range
      return XnodeUtil.hasSameIndexRange(_loops[0], other.getLoop(0));
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
  private Xnode getLoop(int depth) {
    if(_claw != null && _claw.hasCollapseClause() &&
        depth < _claw.getCollapseValue()) {
      return _loops[depth];
    }
    return _loops[0];
  }

  /**
   * Get the group option associated with this loop fusion unit.
   *
   * @return Group option value.
   */
  private String getGroupOptionLabel() {
    return _groupLabel;
  }

  /**
   * Check whether the given loop fusion unit share the same group fusion option
   * with this loop fusion unit.
   *
   * @param otherLoopUnit The other loop fusion unit to be check with this one.
   * @return True if the two loop fusion units share the same group fusion
   * option.
   */
  private boolean hasSameGroupOption(LoopFusion otherLoopUnit) {
    return (otherLoopUnit.getGroupOptionLabel() == null
        ? getGroupOptionLabel() == null
        : otherLoopUnit.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

}

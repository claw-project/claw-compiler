/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.exception.*;

/**
 * A LoopFusion transformation is a dependent transformation. If two LoopFusion
 * transformation units shared the same fusion group and the same loop iteration
 * information, they can be merge together.
 * The transformation consists of merging the body of the second do statement to
 * the end of the first do statement.
 *
 * @author clementval
 */

public class LoopFusion extends Transformation<LoopFusion> {
  // Contains the value of the group option
  private String _groupLabel = null;
  // The loop statement involved in the Transformation
  private XdoStatement[] _loops = null;

  private ClawLanguage _claw;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public LoopFusion(ClawLanguage directive){
    super(directive);
    _claw = directive;
    _groupLabel = directive.getGroupName();
  }

  /**
   * LoopFusion ctor without pragma. Create a LoopFusion dynamically at runtime.
   * @param loop        The do statement to be merge by the loop fusion.
   * @param group       The group fusion option to apply during the loop fusion.
   * @param lineNumber  The line number that triggered the transformation.
   */
  public LoopFusion(XdoStatement loop, String group, int lineNumber){
    super(null);
    _loops = new XdoStatement[] { loop };
    _groupLabel = group;
    setStartLine(lineNumber);
  }

  /**
   * Loop fusion analysis:
   * - Without collapse clause: find whether the pragma statement is followed
   * by a do statement.
   * - With collapse clause: Finf the i loops follwing the pragma.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // With collapse clause
    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0){
      _loops = new XdoStatement[_claw.getCollapseValue()];
      for(int i = 0; i < _claw.getCollapseValue(); ++i){
        if(i == 0){ // Find the outter loop from pragma
          _loops[0] = XelementHelper.
              findNextDoStatement(_claw.getPragma());
        } else { // Find the next i loops
          _loops[i] = XelementHelper.
              findDoStatement(_loops[i-1].getBody(), false);
        }
        if(_loops[i] == null){
          xcodeml.addError("Cannot find loop at depth " + i +
              " after directive", _claw.getPragma().getLineNo());
          return false;
        }
      }
      return true;
    } else {
      // Without collapse clause, just fin the first do statement from the
      // pragma
      XdoStatement loop = XelementHelper.
          findNextDoStatement(_claw.getPragma());
      if(loop == null){
        xcodeml.addError("Cannot find loop after directive",
            _claw.getPragma().getLineNo());
        return false;
      }
      _loops = new XdoStatement[] { loop };
      return true;
    }
  }

  /**
   * Apply the loop fusion transformation.
   * @param xcodeml         The XcodeML on which the transformations are
   *                        applied.
   * @param transformer     The transformer used to applied the transformations.
   * @param loopFusionUnit  The other loop fusion unit to be merge with this
   *                        one.
   * @throws IllegalTransformationException if the transformation cannot be
   * applied.
   */
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        LoopFusion loopFusionUnit)
      throws IllegalTransformationException
  {
    // Apply different transformation if the collapse clause is used
    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0){
      // Merge the most inner loop with the most inner loop of the other fusion
      // unit
      int innerLoopIdx = _claw.getCollapseValue() - 1;
      if(_loops[innerLoopIdx] == null
          || loopFusionUnit.getLoop(innerLoopIdx) == null)
      {
        throw new IllegalTransformationException(
            "Cannot apply transformation, one or both do stmt are invalid.",
            _claw.getPragma().getLineNo()
        );
      }
      _loops[innerLoopIdx].appendToBody(loopFusionUnit.getLoop(innerLoopIdx));
    } else {
      _loops[0].appendToBody(loopFusionUnit.getLoop(0));
    }
    loopFusionUnit.finalizeTransformation();
  }

  /**
   * Call by the transform method of the master loop fusion unit on the slave
   * loop fusion unit to finalize the transformation. Pragma and loop of the
   * slave loop fusion unit are deleted.
   */
  private void finalizeTransformation(){
    // Delete the pragma of the transformed loop
    if(_claw.getPragma() != null){
      _claw.getPragma().delete();
    }

    // Delete the loop that was merged with the main one
    if(_loops[0] != null){
      _loops[0].delete();
    }
    this.transformed();
  }

  /**
   * Check whether the loop fusion unit can be merged with the given loop fusion
   * unit. To be able to be transformed together, the loop fusion units must
   * share the same parent block, the same iteration range, the same group
   * option and both units must be not transformed.
   * @param otherLoopUnit The other loop fusion unit to be merge with this one.
   * @return True if the two loop fusion unit can be merge together.
   */
  public boolean canBeTransformedWith(LoopFusion otherLoopUnit){

    // No loop is transformed already
    if(this.isTransformed() || otherLoopUnit.isTransformed()){
      return false;
    }
    // Loop must share the same group option
    if(!hasSameGroupOption(otherLoopUnit)){
      return false;
    }

    // Loops can only be merged if they are at the same level
    if(!XelementHelper.hasSameParentBlock(_loops[0],
        otherLoopUnit.getLoop(0)))
    {
      return false;
    }

    if(_claw.hasCollapseClause() && _claw.getCollapseValue() > 0){
      for(int i = 0; i < _claw.getCollapseValue(); ++i){
        if(!_loops[i].hasSameRangeWith(otherLoopUnit.getLoop(i))){
          return false;
        }
      }
      return true;
    } else {
      // Loop must share the same iteration range
      return _loops[0].hasSameRangeWith(otherLoopUnit.getLoop(0));
    }

  }

  /**
   * Return the do statement associated with this loop fusion unit at given
   * depth.
   * @param depth Integer avlue representing the depth of the loop from the
   *              pragma statement.
   * @return A do statement.
   */
  private XdoStatement getLoop(int depth){
    if(_claw.hasCollapseClause() &&
        depth < _claw.getCollapseValue())
    {
      return _loops[depth];
    }
    return _loops[0];
  }

  /**
   * Get the group option associated with this loop fusion unit.
   * @return Group option value.
   */
  private String getGroupOptionLabel(){
    return _groupLabel;
  }

  /**
   * Check whether the given loop fusion unit share the same group fusion option
   * with this loop fusion unit.
   * @param otherLoopUnit The other loop fusion unit to be check with this one.
   * @return True if the two loop fusion units share the same group fusion
   * option.
   */
  private boolean hasSameGroupOption(LoopFusion otherLoopUnit){
    return (otherLoopUnit.getGroupOptionLabel() == null
      ? getGroupOptionLabel() == null
      : otherLoopUnit.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

}

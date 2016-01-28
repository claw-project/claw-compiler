/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.pragma.ClawPragma;
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
  private XdoStatement _loop = null;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   * @param pragma The pragma that triggered the loop fusion transformation.
   */
  public LoopFusion(Xpragma pragma){
    super(pragma);
    _groupLabel = ClawPragma.getGroupOptionValue(_pragma.getData());
  }

  /**
   * LoopFusion ctor without pragma. Create a LoopFusion dynamically at runtime.
   * @param loop        The do statement to be merge by the loop fusion.
   * @param group       The group fusion option to apply during the loop fusion.
   * @param lineNumber  The line number that triggered the transformation.
   */
  public LoopFusion(XdoStatement loop, String group, int lineNumber){
    super(null);
    _loop = loop;
    _groupLabel = group;
    setStartLine(lineNumber);
  }

  /**
   * Loop fusion analysis: find whether the pragma statement is followed by a
   * do statement.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    _loop = XelementHelper.findNextDoStatement(_pragma);
    if(_loop == null){
      xcodeml.addError("Cannot find loop after directive",
        _pragma.getLine());
      return false;
    }
    return true;
  }

  /**
   * Apply the loop fusion transformation.
   * @param xcodeml         The XcodeML on which the transformations are
   *                        applied.
   * @param transformer     The transformer used to applied the transformations.
   * @param loopFusionUnit  The other loop fusion unit to be merge with this
   *                        one.
   * @throws IllegalTransformationException
   */
  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopFusion loopFusionUnit) throws IllegalTransformationException
  {
    _loop.appendToBody(loopFusionUnit.getLoop());
    loopFusionUnit.finalizeTransformation();
  }

  /**
   * Call by the transform method of the master loop fusion unit on the slave
   * loop fusion unit to finalize the transformation. Pragma and loop of the
   * slave loop fusion unit are deleted.
   */
  public void finalizeTransformation(){
    // Delete the pragma of the transformed loop
    if(_pragma != null){
      _pragma.delete();
    }
    // Delete the loop that was merged with the main one
    if(_loop != null){
      _loop.delete();
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
    // Loops can only be merged if they are at the same level
    if(!XelementHelper.hasSameParentBlock(_loop, otherLoopUnit.getLoop())){
      return false;
    }
    // Loop must share the same group option
    if(!hasSameGroupOption(otherLoopUnit)){
      return false;
    }
    // Loop must share the same iteration range
    return _loop.hasSameRangeWith(otherLoopUnit.getLoop());
  }

  /**
   * Return the do statement associated with this loop fusion unit.
   * @return A do statement element.
   */
  public XdoStatement getLoop(){
    return _loop;
  }

  /**
   * Get the group option associated with this loop fusion unit.
   * @return Group option value.
   */
  public String getGroupOptionLabel(){
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

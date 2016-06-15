/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XelementName;
import cx2x.xcodeml.xelement.Xscope;
import cx2x.xcodeml.xnode.Xattr;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

/**
 * Class holding information about defined dimension.
 *
 * @author clementval
 */
public class ClawDimension {
  private final int _lowerBound;
  private final int _upperBound;
  private final String _lowerBoundId;
  private final String _upperBoundId;
  private final String _identifier;
  private String _lowerBoundType;
  private String _upperBoundType;

  public static final String BASE_DIM = ":";

  /**
   * Constructs a new dimension object from the extracted information.
   * @param id          Identifier of the defined dimension.
   * @param lowerBound  Lower bound of the dimension.
   * @param upperBound  Upper bound of the dimension.
   * TODO maybe add step information (in the grammar as well)
   */
  public ClawDimension(String id, String lowerBound, String upperBound){
    _identifier = id;

    int tempLb, tempUb;
    String tempLbStr, tempUbStr;
    try {
      tempLb = Integer.parseInt(lowerBound);
      tempLbStr = null;
    } catch (NumberFormatException ex){
      tempLb = -1;
      tempLbStr = lowerBound;
    }

    try {
      tempUb = Integer.parseInt(upperBound);
      tempUbStr = null;
    } catch (NumberFormatException ex){
      tempUb = -1;
      tempUbStr = upperBound;
    }

    _lowerBound = tempLb;
    _upperBound = tempUb;
    _lowerBoundId = tempLbStr;
    _upperBoundId = tempUbStr;
  }


  /**
   * Check whether lower bound is an identifier variable.
   * @return True if the lower bound is an identifier variable.
   */
  public boolean lowerBoundIsVar(){
    return _lowerBoundId != null;
  }

  /**
   * Check whether upper bound is an identifier variable.
   * @return True if the upper bound is an identifier variable.
   */
  public boolean upperBoundIsVar(){
    return _upperBoundId != null;
  }

  /**
   * @return Lower bound value. -1 if lower bound is an identifier variable.
   */
  public int getLowerBoundInt(){
    return _lowerBound;
  }

  /**
   * @return Upper bound value. -1 if upper bound is an identifier variable.
   */
  public int getUpperBoundInt(){
    return _upperBound;
  }

  /**
   * @return Lower bound value. Null if lower bound is an integer constant.
   */
  public String getLowerBoundId(){
    return _lowerBoundId;
  }

  /**
   * @return Upper bound value. Null if upper bound is an integer constant.
   */
  public String getUpperBoundId(){
    return _upperBoundId;
  }

  /**
   * Get the identifier for the current dimension.
   * @return The identifier of the current dimension.
   */
  public String getIdentifier(){
    return _identifier;
  }

  /**
   * Set the value of upper bound var.
   * @param value Type value.
   */
  public void setUpperBoundType(String value){
    _upperBoundType = value;
  }

  /**
   * Set the value of lower bound var.
   * @param value Type value.
   */
  public void setLowerBoundType(String value){
    _lowerBoundType = value;
  }


  /**
   * Generate the correct indexRange element with lowerBound, upperBound and
   * step from the current dimension.
   * @param xcodeml Current XcodeML progra unit in which elements will be
   *                created.
   * @return A new indexRange elements.
   */
  public Xnode generateIndexRange(XcodeProgram xcodeml, boolean withStep) {
    Xnode range = new Xnode(Xcode.INDEXRANGE, xcodeml);
    Xnode lower = new Xnode(Xcode.LOWERBOUND, xcodeml);
    Xnode upper = new Xnode(Xcode.UPPERBOUND, xcodeml);
    range.appendToChildren(lower, false);
    range.appendToChildren(upper, false);

    if (withStep){
      Xnode step = new Xnode(Xcode.STEP, xcodeml);
      Xnode stepValue = new Xnode(Xcode.FINTCONSTANT, xcodeml);
      stepValue.setAttribute(Xattr.TYPE, XelementName.TYPE_F_INT);
      step.appendToChildren(stepValue, false);
      stepValue.setValue(XelementName.DEFAULT_STEP_VALUE);
      range.appendToChildren(step, false);
    }

    // lower bound
    if(lowerBoundIsVar()){
      Xnode lowerBoundValue = XelementHelper.createVar(_lowerBoundType,
          _lowerBoundId, Xscope.LOCAL, xcodeml);
      lower.appendToChildren(lowerBoundValue, false);
    } else {
      Xnode lowerBoundValue = new Xnode(Xcode.FINTCONSTANT, xcodeml);
      lowerBoundValue.setAttribute(Xattr.TYPE, XelementName.TYPE_F_INT);
      lowerBoundValue.setValue(String.valueOf(_lowerBound));
      lower.appendToChildren(lowerBoundValue, false);
    }

    // upper bound
    if(upperBoundIsVar()){
      Xnode upperBoundValue = XelementHelper.createVar(_upperBoundType,
          _upperBoundId, Xscope.LOCAL, xcodeml);
      upper.appendToChildren(upperBoundValue, false);
    } else {
      Xnode upperBoundValue = new Xnode(Xcode.FINTCONSTANT, xcodeml);
      upperBoundValue.setAttribute(Xattr.TYPE, XelementName.TYPE_F_INT);
      upperBoundValue.setValue(String.valueOf(_upperBound));
      lower.appendToChildren(upperBoundValue, false);
    }
    return range;
  }

  /**
   * Generate the array index that will be placed in the array reference for
   * this additional dimension.
   * @param xcodeml Current XcodeML progra unit in which elements will be
   *                created.
   * @return A new arrayIndex element including a var element with the dimension
   * identifier.
   */
  public Xnode generateArrayIndex(XcodeProgram xcodeml) {
    Xnode aIdx = new Xnode(Xcode.ARRAYINDEX, xcodeml);
    Xnode var = XelementHelper.createVar(XelementName.TYPE_F_INT, _identifier,
        Xscope.LOCAL, xcodeml);
    aIdx.appendToChildren(var, false);
    return aIdx;
  }

}

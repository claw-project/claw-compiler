/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.*;

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
   * @returnThe identifier of the current dimension.
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
   * @throws IllegalTransformationException if elements cannot be created.
   */
  public XindexRange generateIndexRange(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    XindexRange range = XelementHelper.createEmpty(XindexRange.class, xcodeml);
    XlowerBound lower = XelementHelper.createEmpty(XlowerBound.class, xcodeml);
    XupperBound upper = XelementHelper.createEmpty(XupperBound.class, xcodeml);
    Xstep step = XelementHelper.createEmpty(Xstep.class, xcodeml);
    XintConstant stepValue =
        XelementHelper.createEmpty(XintConstant.class, xcodeml);
    stepValue.setType(XelementName.TYPE_F_INT);
    stepValue.setValue(XelementName.DEFAULT_STEP_VALUE);
    step.appendToChildren(stepValue, false);

    range.appendToChildren(lower, false);
    range.appendToChildren(upper, false);
    range.appendToChildren(step, false);

    // lower bound
    if(lowerBoundIsVar()){
      Xvar lowerBoundValue =
          Xvar.create(_lowerBoundType, _lowerBoundId, Xscope.LOCAL, xcodeml);
      lower.appendToChildren(lowerBoundValue, false);
    } else {
      XintConstant lowerBoundValue =
          XelementHelper.createEmpty(XintConstant.class, xcodeml);
      lowerBoundValue.setType(XelementName.TYPE_F_INT);
      lowerBoundValue.setValue(String.valueOf(_lowerBound));
      lower.appendToChildren(lowerBoundValue, false);
    }

    // upper bound
    if(upperBoundIsVar()){
      Xvar upperBoundValue =
          Xvar.create(_upperBoundType, _upperBoundId, Xscope.LOCAL, xcodeml);
      upper.appendToChildren(upperBoundValue, false);
    } else {
      XintConstant upperBoundValue =
          XelementHelper.createEmpty(XintConstant.class, xcodeml);
      upperBoundValue.setType(XelementName.TYPE_F_INT);
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
   * @throws IllegalTransformationException if elements cannot be created.
   */
  public XarrayIndex generateArrayIndex(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    XarrayIndex aIdx = XelementHelper.createEmpty(XarrayIndex.class, xcodeml);
    Xvar v = Xvar.create(XelementName.TYPE_F_INT, _identifier, Xscope.LOCAL,
        xcodeml);
    aIdx.appendToChildren(v, false);
    return aIdx;
  }

}

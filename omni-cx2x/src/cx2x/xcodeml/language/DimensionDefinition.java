/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.*;

/**
 * Class holding information about defined dimension.
 *
 * @author clementval
 */
public class DimensionDefinition {

  public static final String BASE_DIM = ":";
  private final int _lowerBound;
  private final int _upperBound;
  private final String _lowerBoundId;
  private final String _upperBoundId;
  private final String _identifier;
  private String _lowerBoundType;
  private String _upperBoundType;

  /**
   * Constructs a new dimension object from the extracted information.
   *
   * @param id         Identifier of the defined dimension.
   * @param lowerBound Lower bound of the dimension.
   * @param upperBound Upper bound of the dimension.
   *                   TODO maybe add step information (in the grammar as well)
   */
  public DimensionDefinition(String id, String lowerBound, String upperBound) {
    _identifier = id;

    int tempLb, tempUb;
    String tempLbStr, tempUbStr;
    try {
      tempLb = Integer.parseInt(lowerBound);
      tempLbStr = null;
    } catch(NumberFormatException ex) {
      tempLb = -1;
      tempLbStr = lowerBound;
    }

    try {
      tempUb = Integer.parseInt(upperBound);
      tempUbStr = null;
    } catch(NumberFormatException ex) {
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
   *
   * @return True if the lower bound is an identifier variable.
   */
  public boolean lowerBoundIsVar() {
    return _lowerBoundId != null;
  }

  /**
   * Check whether upper bound is an identifier variable.
   *
   * @return True if the upper bound is an identifier variable.
   */
  public boolean upperBoundIsVar() {
    return _upperBoundId != null;
  }

  /**
   * @return Lower bound value. -1 if lower bound is an identifier variable.
   */
  public int getLowerBoundInt() {
    return _lowerBound;
  }

  /**
   * @return Upper bound value. -1 if upper bound is an identifier variable.
   */
  public int getUpperBoundInt() {
    return _upperBound;
  }

  /**
   * @return Lower bound value. Null if lower bound is an integer constant.
   */
  public String getLowerBoundId() {
    return _lowerBoundId;
  }

  /**
   * @return Upper bound value. Null if upper bound is an integer constant.
   */
  public String getUpperBoundId() {
    return _upperBoundId;
  }

  /**
   * Get the identifier for the current dimension.
   *
   * @return The identifier of the current dimension.
   */
  public String getIdentifier() {
    return _identifier;
  }

  /**
   * Set the value of upper bound var.
   *
   * @param value Type value.
   */
  public void setUpperBoundType(String value) {
    _upperBoundType = value;
  }

  /**
   * Set the value of lower bound var.
   *
   * @param value Type value.
   */
  public void setLowerBoundType(String value) {
    _lowerBoundType = value;
  }


  /**
   * Generate the correct indexRange element with lowerBound, upperBound and
   * step from the current dimension.
   *
   * @param xcodeml  Current XcodeML program unit in which elements will be
   *                 created.
   * @param withStep IF true, step element is created.
   * @return A new indexRange elements.
   */
  public Xnode generateIndexRange(XcodeML xcodeml, boolean withStep) {
    Xnode range = xcodeml.createNode(Xcode.INDEXRANGE);
    Xnode lower = xcodeml.createNode(Xcode.LOWERBOUND);
    Xnode upper = xcodeml.createNode(Xcode.UPPERBOUND);
    range.append(lower);
    range.append(upper);

    if(withStep) {
      Xnode step = xcodeml.createNode(Xcode.STEP);
      Xnode stepValue = xcodeml.createNode(Xcode.FINTCONSTANT);
      stepValue.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      step.append(stepValue);
      stepValue.setValue(Xname.DEFAULT_STEP_VALUE);
      range.append(step);
    }

    // lower bound
    if(lowerBoundIsVar()) {
      if(_lowerBoundType == null) {
        _lowerBoundType = xcodeml.getTypeTable().generateIntegerTypeHash();
        XbasicType bType = xcodeml.createBasicType(_lowerBoundType,
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(bType);
      }
      Xnode lowerBoundValue = xcodeml.createVar(_lowerBoundType, _lowerBoundId,
          Xscope.LOCAL);
      lower.append(lowerBoundValue);
    } else {
      Xnode lowerBoundValue = xcodeml.createNode(Xcode.FINTCONSTANT);
      lowerBoundValue.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      lowerBoundValue.setValue(String.valueOf(_lowerBound));
      lower.append(lowerBoundValue);
    }

    // upper bound
    if(upperBoundIsVar()) {
      if(_lowerBoundType == null) {
        _upperBoundType = xcodeml.getTypeTable().generateIntegerTypeHash();
        XbasicType bType = xcodeml.createBasicType(_upperBoundType,
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(bType);
      }
      Xnode upperBoundValue = xcodeml.createVar(_upperBoundType, _upperBoundId,
          Xscope.LOCAL);
      upper.append(upperBoundValue);
    } else {
      Xnode upperBoundValue = xcodeml.createNode(Xcode.FINTCONSTANT);
      upperBoundValue.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      upperBoundValue.setValue(String.valueOf(_upperBound));
      lower.append(upperBoundValue);
    }
    return range;
  }

  /**
   * Generate the array index that will be placed in the array reference for
   * this additional dimension.
   *
   * @param xcodeml Current XcodeML program unit in which elements will be
   *                created.
   * @return A new arrayIndex element including a var element with the dimension
   * identifier.
   */
  public Xnode generateArrayIndex(XcodeProgram xcodeml) {
    Xnode aIdx = xcodeml.createNode(Xcode.ARRAYINDEX);
    Xnode var = xcodeml.createVar(Xname.TYPE_F_INT, _identifier, Xscope.LOCAL);
    aIdx.append(var);
    return aIdx;
  }

}

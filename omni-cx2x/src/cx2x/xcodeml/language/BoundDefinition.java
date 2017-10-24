/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.*;

/**
 * Class holding information about bound (upper/lower).
 *
 * @author clementval
 */
public class BoundDefinition {

  private String _boundTypeHash = null;
  private String _strBoundValue = null;
  private int _intBoundValue;
  private BoundType _boundType = BoundType.LOWER;

  /**
   * Constructs a BoundDefinition from String value. Detects if bound is an
   * integer constant or a var.
   *
   * @param boundValue String representation of the bound value.
   */
  BoundDefinition(String boundValue, BoundType type) {
    _boundType = type;
    try {
      _intBoundValue = Integer.parseInt(boundValue);
      _strBoundValue = null;
    } catch(NumberFormatException ex) {
      _intBoundValue = -1;
      _strBoundValue = boundValue;
    }
  }

  /**
   * Check whether the bound is a var.
   *
   * @return True if the bound is a var.
   */
  public boolean isVar() {
    return _strBoundValue != null;
  }

  /**
   * Set type associated with the bound
   *
   * @param value Type hash value.
   */
  public void setType(String value) {
    _boundTypeHash = value;
  }

  /**
   * Get integer value of the bound object.
   *
   * @return Integer value. -1 if not set.
   */
  public int getIntValue() {
    return _intBoundValue;
  }

  /**
   * Get string value of the bound object.
   *
   * @return String value. null if not set.
   */
  public String getValue() {
    return _strBoundValue;
  }

  /**
   * Generate the corresponding node to represent the bound.
   *
   * @param xcodeml Current XcodeML translation unit.
   * @return Newly created node (lowerBound or upperBound).
   */
  public Xnode generate(XcodeML xcodeml) {
    Xcode opcode = _boundType == BoundType.LOWER ?
        Xcode.LOWERBOUND : Xcode.UPPERBOUND;
    Xnode bound = xcodeml.createNode(opcode);
    bound.append(generateValueNode(xcodeml));
    return bound;
  }

  /**
   * Generate the corresponding node to represent the bound value.
   *
   * @param xcodeml Current XcodeML translation unit.
   * @return Newly created value node (Var or FintConstant).
   */
  Xnode generateValueNode(XcodeML xcodeml) {
    if(isVar()) {
      if(_boundType == null) {
        _boundTypeHash = xcodeml.getTypeTable().generateIntegerTypeHash();
        XbasicType bType = xcodeml.createBasicType(_boundTypeHash,
            Xname.TYPE_F_INT, Xintent.IN);
        xcodeml.getTypeTable().add(bType);
      }
      return xcodeml.createVar(_boundTypeHash, _strBoundValue, Xscope.LOCAL);
    } else {
      Xnode boundValue = xcodeml.createNode(Xcode.FINTCONSTANT);
      boundValue.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      boundValue.setValue(String.valueOf(_intBoundValue));
      return boundValue;
    }
  }

  // Enum representing the type of bound
  public enum BoundType {
    LOWER, UPPER
  }
}

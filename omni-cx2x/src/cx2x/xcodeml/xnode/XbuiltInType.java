/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * Enumeration representing the different built-in type from XcodeML/F IR (9.1)
 *
 * @author clementval
 */
public enum XbuiltInType {
  INT(Xname.TYPE_F_INT),
  REAL(Xname.TYPE_F_REAL),
  COMPLEX(Xname.TYPE_F_COMPLEX),
  LOGICAL(Xname.TYPE_F_LOGICAL),
  CHAR(Xname.TYPE_F_CHAR),
  VOID(Xname.TYPE_F_VOID),
  NONE("");

  // Member holding XcodeML/F IR value.
  private final String _irValue;

  /**
   * Default constructor of enum member with associated IR value.
   *
   * @param irValue Value from XcodeML/F IR.
   */
  XbuiltInType(String irValue) {
    _irValue = irValue;
  }

  /**
   * Get type from XcodeML/F IR value.
   *
   * @param value Type value from XcodeML/F IR.
   * @return Corresponding enum value. NONE if value corresponds to nothing.
   */
  public static XbuiltInType fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case Xname.TYPE_F_INT:
        return INT;
      case Xname.TYPE_F_REAL:
        return REAL;
      case Xname.TYPE_F_COMPLEX:
        return COMPLEX;
      case Xname.TYPE_F_LOGICAL:
        return LOGICAL;
      case Xname.TYPE_F_CHAR:
        return CHAR;
      case Xname.TYPE_F_VOID:
        return VOID;
      default:
        return NONE;
    }
  }

  @Override
  public String toString() {
    return _irValue;
  }
}

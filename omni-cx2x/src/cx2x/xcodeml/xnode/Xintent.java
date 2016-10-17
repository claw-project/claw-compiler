/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * The Xintent represents the possible value for the intent attribute in XcodeML
 * intermediate representation.
 * <p>
 * Possible value are: in, out, inout
 *
 * @author clementval
 */
public enum Xintent {
  IN,
  OUT,
  INOUT,
  NONE;

  /**
   * Convert current enum to String value.
   *
   * @return Corresponding String value.
   */
  public String toString() {
    switch(this) {
      case IN:
        return Xname.INTENT_IN;
      case OUT:
        return Xname.INTENT_OUT;
      case INOUT:
        return Xname.INTENT_INOUT;
      default:
        return "";
    }
  }

  /**
   * Convert string value to enum.
   *
   * @param value String value.
   * @return Corresponding enum value.
   */
  public static Xintent fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case Xname.INTENT_IN:
        return IN;
      case Xname.INTENT_OUT:
        return OUT;
      case Xname.INTENT_INOUT:
        return INOUT;
    }
    return NONE;
  }
}

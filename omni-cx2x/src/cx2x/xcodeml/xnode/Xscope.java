/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * The Xscope represents the possible value for the scope attribute in XcodeML
 * intermediate representation.
 * <p>
 * Possible value are: local, global, param
 *
 * @author clementval
 */

public enum Xscope {
  LOCAL(Xname.SCOPE_LOCAL),
  GLOBAL(Xname.SCOPE_GLOBAL),
  PARAM(Xname.SCOPE_PARAM);

  private final String _value;
  Xscope(String value) {
    _value = value;
  }

  /**
   * Convert string value to enum.
   *
   * @param value String value.
   * @return Corresponding enum value.
   */
  public static Xscope fromString(String value) {
    if(value == null) {
      return null;
    }
    switch(value.toLowerCase()) {
      case Xname.SCOPE_LOCAL:
        return LOCAL;
      case Xname.SCOPE_GLOBAL:
        return GLOBAL;
      case Xname.SCOPE_PARAM:
        return PARAM;
    }
    return null;
  }

  /**
   * Convert current enum to String value.
   *
   * @return Corresponding String value.
   */
  @Override
  public String toString() {
    return _value;
  }

}

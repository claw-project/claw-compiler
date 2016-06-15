/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * The Xscope represents the possible value for the scope attribute in XcodeML
 * intermediate representation.
 *
 * Possible value are: local, global, param
 *
 * @author clementval
 */

public enum Xscope {
  LOCAL,
  GLOBAL,
  PARAM
  ;

  /**
   * Convert current enum to String value.
   * @return Corresponding String value.
   */
  public String toString(){
    switch(this){
      case LOCAL:
        return Xname.SCOPE_LOCAL;
      case GLOBAL:
        return Xname.SCOPE_GLOBAL;
      case PARAM:
        return Xname.SCOPE_PARAM;
      default:
        return "";
    }
  }

  /**
   * Convert string value to enum.
   * @param value String value.
   * @return Corresponding enum value.
   */
  public static Xscope fromString(String value){
    if(value == null){
      return null;
    }
    switch (value) {
      case Xname.SCOPE_LOCAL:
        return LOCAL;
      case Xname.SCOPE_GLOBAL:
        return GLOBAL;
      case Xname.SCOPE_PARAM:
        return PARAM;
    }
    return null;
  }

}

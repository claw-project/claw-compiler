/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

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

  public String toString(){
    switch(this){
      case LOCAL:
        return XelementName.SCOPE_LOCAL;
      case GLOBAL:
        return XelementName.SCOPE_GLOBAL;
      case PARAM:
        return XelementName.SCOPE_PARAM;
      default:
        return "";
    }
  }

  public static Xscope fromString(String value){
    switch (value) {
      case XelementName.SCOPE_LOCAL:
        return LOCAL;
      case XelementName.SCOPE_GLOBAL:
        return GLOBAL;
      case XelementName.SCOPE_PARAM:
        return PARAM;
    }
    return null;
  }



}

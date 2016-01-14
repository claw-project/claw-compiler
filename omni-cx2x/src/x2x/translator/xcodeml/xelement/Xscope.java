package x2x.translator.xcodeml.xelement;

/**
 * The Xscope represents the possible value for the scope attribute in XcodeML
 * intermediate representation.
 *
 * Possible value are: local, global, param
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
    if(value.equals(XelementName.SCOPE_LOCAL)){
      return LOCAL;
    } else if (value.equals(XelementName.SCOPE_GLOBAL)){
      return GLOBAL;
    } else if (value.equals(XelementName.SCOPE_PARAM)){
      return PARAM;
    }
    return null;
  }



}

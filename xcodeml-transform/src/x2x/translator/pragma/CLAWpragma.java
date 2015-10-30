package x2x.translator.pragma;

import exc.object.Xobject;



public enum CLAWpragma {
  //directive
  LOOP_FUSION,
  LOOP_INTERCHANGE,

  // loop-fusion
  FUSION_GROUP,

  // loop-interchange
  NEW_ORDER,
  ;

  private static final String CLAW_DIRECTIVE = "claw";
  private static final String LOOP_FUSION_DIRECTIVE = "loop-fusion";
  private static final String LOOP_INTERCHANGE_DIRECTIVE = "loop-interchange";

  private String name = null;

  public String getName() {
    if (name == null) name = toString().toLowerCase();
    return name;
  }

  public static CLAWpragma getDirective(String pragma){
    // TODO error handling
    String[] parts = pragma.split(" ");
    String directive = parts[1];
    switch(directive){
      case LOOP_FUSION_DIRECTIVE:
        return CLAWpragma.LOOP_FUSION;
      case LOOP_INTERCHANGE_DIRECTIVE:
        return CLAWpragma.LOOP_INTERCHANGE;
      default:
        return null;
    }
  }


  // Check the correctness of a claw directive
  // TODO correct error message
  public static boolean isValid(String pragma){
    String[] parts = pragma.split(" ");

    if(parts.length < 2){
      return false;
    }

    String claw = parts[0];
    String directive = parts[1];

    if(!claw.equals(CLAW_DIRECTIVE)){
      return false;
    }

    switch(directive){
      case LOOP_FUSION_DIRECTIVE:
        return isValidOption(CLAWpragma.LOOP_FUSION, null);
      case LOOP_INTERCHANGE_DIRECTIVE:
        return isValidOption(CLAWpragma.LOOP_INTERCHANGE, null);
      default:
        return false;
    }
  }

  // TODO check option according to the directive
  private static boolean isValidOption(CLAWpragma directive, String[] options){
    return true;
  }



  public static CLAWpragma valueOf(Xobject x) {
    return valueOf(x.getString());
  }

  public boolean isDirective(){
    switch(this){
    case LOOP_FUSION:
    case LOOP_INTERCHANGE:
      return true;
    default:
      return false;
    }
  }

  public boolean isLoop(){
    switch(this){
    case LOOP_FUSION:
    case LOOP_INTERCHANGE:
      return true;
    default:
      return false;
    }
  }
}

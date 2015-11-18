package x2x.translator.pragma;

import exc.object.Xobject;
import java.util.regex.*;


public enum CLAWpragma {
  //directive
  LOOP_FUSION,
  LOOP_INTERCHANGE,
  LOOP_VECTOR,
  LOOP_EXTRACT,

  // loop-fusion
  FUSION_GROUP,

  ;

  private static final String CLAW_DIRECTIVE = "claw";
  private static final String LOOP_FUSION_DIRECTIVE = "loop-fusion";
  private static final String LOOP_INTERCHANGE_DIRECTIVE = "loop-interchange";
  private static final String LOOP_VECTOR_DIRECTIVE = "loop-vector";
  private static final String LOOP_EXTRACT_DIRECTIVE = "loop-extract";
  private static final String OPTION_FUSION_GROUP = "group";
  private static final String MULTIPLE_SPACES = " *";
  private static final String INNER_OPTION = "\\(([^)]+)\\)";

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
      case LOOP_VECTOR_DIRECTIVE:
        return CLAWpragma.LOOP_VECTOR;
      case LOOP_EXTRACT_DIRECTIVE:
        return CLAWpragma.LOOP_EXTRACT;
      default:
        return null;
    }
  }


  public static String getGroupOptionValue(String pragma){
    if(getDirective(pragma) != CLAWpragma.LOOP_FUSION){
      return null;
    }

    Matcher matchFullDirective = Pattern.compile(CLAW_DIRECTIVE +
      MULTIPLE_SPACES + LOOP_FUSION_DIRECTIVE + MULTIPLE_SPACES +
      OPTION_FUSION_GROUP + INNER_OPTION, Pattern.CASE_INSENSITIVE
      ).matcher(pragma);
    if(!matchFullDirective.matches()){
      return null;
    }

    Matcher matchOption = Pattern.compile(INNER_OPTION,
      Pattern.CASE_INSENSITIVE).matcher(pragma);
    while(matchOption.find()) {
      return matchOption.group(1);
    }
    return null;
  }

  public static String getNewOrderOptionValue(String pragma){
    if(getDirective(pragma) != CLAWpragma.LOOP_INTERCHANGE){
      return null;
    }

    Matcher matchFullDirective = Pattern.compile(CLAW_DIRECTIVE +
      MULTIPLE_SPACES + LOOP_INTERCHANGE_DIRECTIVE + MULTIPLE_SPACES
      + INNER_OPTION, Pattern.CASE_INSENSITIVE).matcher(pragma);
    if(!matchFullDirective.matches()){
      return null;
    }

    Matcher matchOption = Pattern.compile(INNER_OPTION,
      Pattern.CASE_INSENSITIVE).matcher(pragma);
    while(matchOption.find()) {
      return matchOption.group(1);
    }
    return null;
  }

  public static boolean startsWithClaw(String pragma){
    if(pragma.startsWith(CLAW_DIRECTIVE)){
      return true;
    }
    return false;
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
      case LOOP_VECTOR_DIRECTIVE:
        return isValidOption(CLAWpragma.LOOP_VECTOR, null);
      case LOOP_EXTRACT_DIRECTIVE:
        return isValidOption(CLAWpragma.LOOP_EXTRACT, null);
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
    case LOOP_VECTOR:
    case LOOP_EXTRACT:
      return true;
    default:
      return false;
    }
  }

  public boolean isLoop(){
    switch(this){
    case LOOP_FUSION:
    case LOOP_INTERCHANGE:
    case LOOP_VECTOR:
    case LOOP_EXTRACT:
      return true;
    default:
      return false;
    }
  }
}

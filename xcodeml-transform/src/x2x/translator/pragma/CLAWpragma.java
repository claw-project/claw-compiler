package x2x.translator.pragma;

import exc.object.Xobject;
import java.util.regex.*;
import java.util.Arrays;


public enum CLAWpragma {
  //directive
  LOOP_FUSION,
  LOOP_INTERCHANGE,
  LOOP_VECTOR,
  LOOP_EXTRACT,

  // loop-fusion
  FUSION_GROUP,

  ;

  private static final String PREFIX_CLAW = "claw";
  private static final String DIRECTIVE_LOOP_FUSION = "loop-fusion";
  private static final String DIRECTIVE_LOOP_INTERCHANGE = "loop-interchange";
  private static final String DIRECTIVE_LOOP_VECTOR = "loop-vector";
  private static final String DIRECTIVE_LOOP_EXTRACT = "loop-extract";
  private static final String OPTION_FUSION_GROUP = "group";
  private static final String MULTIPLE_SPACES = " *";
  private static final String INNER_OPTION = "\\(([^)]+)\\)";
  private static final String ANY_SPACES = "\\s*";



  private static final String REGEX_LOOP_FUSION = PREFIX_CLAW + ANY_SPACES +
    DIRECTIVE_LOOP_FUSION;

  private static final String REGEX_LOOP_INTERCHANGE = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_INTERCHANGE;

  private static final String REGEX_OPTION_SIMPLE = ANY_SPACES + INNER_OPTION;
  private static final String REGEX_OPTION_GROUP = ANY_SPACES +
    OPTION_FUSION_GROUP + REGEX_OPTION_SIMPLE;




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
      case DIRECTIVE_LOOP_FUSION:
        return CLAWpragma.LOOP_FUSION;
      case DIRECTIVE_LOOP_INTERCHANGE:
        return CLAWpragma.LOOP_INTERCHANGE;
      case DIRECTIVE_LOOP_VECTOR:
        return CLAWpragma.LOOP_VECTOR;
      case DIRECTIVE_LOOP_EXTRACT:
        return CLAWpragma.LOOP_EXTRACT;
      default:
        return null;
    }
  }


  public static String getGroupOptionValue(String pragma){
    Matcher matchFullDirective = Pattern.compile(REGEX_LOOP_FUSION +
      REGEX_OPTION_GROUP, Pattern.CASE_INSENSITIVE).matcher(pragma);

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

  public static String getSimpleOptionValue(String pragma){
    if(getDirective(pragma) != CLAWpragma.LOOP_INTERCHANGE){
      return null;
    }

    Matcher matchFullDirective = Pattern.compile(REGEX_LOOP_INTERCHANGE
      + REGEX_OPTION_SIMPLE,
      Pattern.CASE_INSENSITIVE).matcher(pragma);

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
    if(pragma.startsWith(PREFIX_CLAW)){
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

    if(!claw.equals(PREFIX_CLAW)){
      return false;
    }

    String options = null;
    if (parts.length > 2){
      // Take only the options
      String[] temp = Arrays.copyOfRange(parts, 2, parts.length);
      options = String.join(" ", temp);
    }

    switch(directive){
      case DIRECTIVE_LOOP_FUSION:
        return isValidOptions(CLAWpragma.LOOP_FUSION, options);
      case DIRECTIVE_LOOP_INTERCHANGE:
        return isValidOptions(CLAWpragma.LOOP_INTERCHANGE, options);
      case DIRECTIVE_LOOP_VECTOR:
        return isValidOptions(CLAWpragma.LOOP_VECTOR, options);
      case DIRECTIVE_LOOP_EXTRACT:
        return isValidOptions(CLAWpragma.LOOP_EXTRACT, options);
      default:
        return false;
    }
  }

  private static boolean isValidOptions(CLAWpragma directive, String option){
    switch(directive){
      case LOOP_FUSION: // loop-fusion has only group option
        return CLAWpragma.isGroupOptionValid(option);
      case LOOP_INTERCHANGE:
        return CLAWpragma.isOrderOptionValid(option);
      case LOOP_VECTOR:
        return true; // TODO
      case LOOP_EXTRACT:
        return true; // TODO
      default:
        return false;
    }
  }

  private static boolean isGroupOptionValid(String option){
    return CLAWpragma.checkOptionalOption(option, REGEX_OPTION_GROUP, false);
  }

  private static boolean isOrderOptionValid(String option){
    return CLAWpragma.checkOptionalOption(option, REGEX_OPTION_SIMPLE, false);
  }

  private static boolean checkOptionalOption(String option, String regex,
    boolean allowOnlySpaces)
  {
    if(option == null){ // option is not mandatory
      return true;
    }
    Matcher matchOption = Pattern.compile(regex, Pattern.CASE_INSENSITIVE)
      .matcher(option);

    while(matchOption.find()) {
      String match =  matchOption.group(1);
      if(match.trim().length() > 0){
        return true;
      }
      return false;
    }

    return false;
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

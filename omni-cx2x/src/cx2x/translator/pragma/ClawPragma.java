/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import exc.object.Xobject;
import java.util.regex.*;
import java.util.Arrays;

/**
 * Enumeration of the different directive available in the CLAW language.
 *
 * @author clementval
 */
 
public enum ClawPragma {
  //directive
  LOOP_FUSION,
  LOOP_INTERCHANGE,
  LOOP_VECTOR,
  LOOP_EXTRACT,
  UTILITIES_REMOVE,
  BASE_END,

  // loop-fusion
  FUSION_GROUP,
  ;

  private static final String PREFIX_CLAW = "claw";
  private static final String DIRECTIVE_LOOP_FUSION = "loop-fusion";
  private static final String DIRECTIVE_LOOP_INTERCHANGE = "loop-interchange";
  private static final String DIRECTIVE_LOOP_VECTOR = "loop-vector";
  private static final String DIRECTIVE_LOOP_EXTRACT = "loop-extract";
  private static final String DIRECTIVE_UTILITIES_REMOVE = "remove";
  private static final String DIRECTIVE_BASE_END = "end";
  private static final String OPTION_FUSION_GROUP = "group";
  private static final String OPTION_EXTRACT_RANGE = "range";
  private static final String OPTION_EXTRACT_MAP = "map";
  private static final String OPTION_FUSION = "fusion";
  private static final String MULTIPLE_SPACES = " *";
  private static final String INNER_OPTION = "\\(([^)]+)\\)";
  private static final String ANY_SPACES = "\\s*";
  private static final String RANGE = "\\(([^=]+)=([^,]+),([^,]+),?(.+)?\\)";
  private static final String SIMPLE_MAPPING = "\\(([^:]+):(.*)\\)";


  private static final String REGEX_LOOP_FUSION = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_FUSION;
  private static final String REGEX_LOOP_INTERCHANGE = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_INTERCHANGE;
  private static final String REGEX_LOOP_EXTRACT = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_EXTRACT;

  private static final String REGEX_OPTION_SIMPLE = ANY_SPACES + INNER_OPTION;
  private static final String REGEX_OPTION_GROUP = ANY_SPACES
    + OPTION_FUSION_GROUP + REGEX_OPTION_SIMPLE;
  private static final String REGEX_OPTION_RANGE = ANY_SPACES
    + OPTION_EXTRACT_RANGE + ANY_SPACES + RANGE;

  private static final String REGEX_OPTION_MAP = ANY_SPACES
    + OPTION_EXTRACT_MAP + ANY_SPACES + SIMPLE_MAPPING;

  private static final String REGEX_REMOVE = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_UTILITIES_REMOVE;
  private static final String REGEX_END_REMOVE = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_BASE_END + ANY_SPACES + DIRECTIVE_UTILITIES_REMOVE;



  private String name = null;

  public String getName() {
    if (name == null) name = toString().toLowerCase();
    return name;
  }

  public static ClawPragma getDirective(String pragma){
    // TODO error handling
    String[] parts = pragma.split(" ");
    String directive = parts[1];
    switch(directive){
      case DIRECTIVE_LOOP_FUSION:
        return ClawPragma.LOOP_FUSION;
      case DIRECTIVE_LOOP_INTERCHANGE:
        return ClawPragma.LOOP_INTERCHANGE;
      case DIRECTIVE_LOOP_VECTOR:
        return ClawPragma.LOOP_VECTOR;
      case DIRECTIVE_LOOP_EXTRACT:
        return ClawPragma.LOOP_EXTRACT;
      case DIRECTIVE_UTILITIES_REMOVE:
        return ClawPragma.UTILITIES_REMOVE;
      case DIRECTIVE_BASE_END:
        return ClawPragma.BASE_END;
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
    if(matchOption.find()) {
      return matchOption.group(1);
    }
    return null;
  }

  public static String getSimpleOptionValue(String pragma){
    if(getDirective(pragma) != ClawPragma.LOOP_INTERCHANGE){
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
    if(matchOption.find()) {
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
        return isValidOptions(ClawPragma.LOOP_FUSION, options);
      case DIRECTIVE_LOOP_INTERCHANGE:
        return isValidOptions(ClawPragma.LOOP_INTERCHANGE, options);
      case DIRECTIVE_LOOP_VECTOR:
        return isValidOptions(ClawPragma.LOOP_VECTOR, options);
      case DIRECTIVE_LOOP_EXTRACT:
        return isValidOptions(ClawPragma.LOOP_EXTRACT, options);
      case DIRECTIVE_UTILITIES_REMOVE:
        return true;
      case DIRECTIVE_BASE_END:
        return isValidOptions(ClawPragma.BASE_END, options);
      default:
        return false;
    }
  }

  private static boolean isValidOptions(ClawPragma directive, String option){
    switch(directive){
      case LOOP_FUSION: // loop-fusion has only group option
        return ClawPragma.isGroupOptionValid(option);
      case LOOP_INTERCHANGE:
        return ClawPragma.isOrderOptionValid(option);
      case LOOP_VECTOR:
        return true; // TODO
      case LOOP_EXTRACT:
        return ClawPragma.isRangeOptionValid(option)
          && ClawPragma.isMapOptionValid(option);
      case BASE_END:
        // Only remove is associated with end directive now
        if(option.contains(DIRECTIVE_UTILITIES_REMOVE)){
          return true;
        }
        return false;
      default:
        return false;
    }
  }

  private static boolean isGroupOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_GROUP, false);
  }

  private static boolean isOrderOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_SIMPLE, false);
  }

  private static boolean isRangeOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_RANGE, false);
  }

  private static boolean isMapOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_MAP, false);
  }

  private static boolean checkOptionalOption(String option, String regex,
    boolean allowOnlySpaces)
  {
    // TODO handle allowOnlySpaces parameter
    if(option == null){ // option is not mandatory
      return true;
    }
    Matcher matchOption = Pattern.compile(regex, Pattern.CASE_INSENSITIVE)
      .matcher(option);

    if(matchOption.find()) {
      String match =  matchOption.group(1);
      if(match.trim().length() > 0){
        return true;
      }
      return false;
    }

    return false;
  }

  public static ClawPragma valueOf(Xobject x) {
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

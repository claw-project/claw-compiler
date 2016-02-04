/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import cx2x.translator.misc.Utility;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.xelement.*;

import java.util.ArrayList;
import java.util.List;
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
  private static final String OPTION_EXTRACT_ACC = "acc";
  private static final String OPTION_EXTRACT_PARALLEL = "parallel";
  private static final String OPTION_FUSION = "fusion";
  private static final String INNER_OPTION = "\\(([^)]+)\\)";
  private static final String ANY_SPACES = "\\s*";
  private static final String RANGE_GLOBAL = "range\\(([^\\)]+)";
  private static final String RANGE = "([^=]+)=([^,]+),([^,]+),?(.+)?";
  private static final String SIMPLE_MAPPING = "\\(([^:]+):(.*)\\)";


  private static final String REGEX_MAPPING = "map\\(([^:]*:[^)]*)\\)";
  private static final String REGEX_LOOP_FUSION = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_FUSION;
  private static final String REGEX_LOOP_INTERCHANGE = PREFIX_CLAW + ANY_SPACES
    + DIRECTIVE_LOOP_INTERCHANGE;
  private static final String REGEX_LOOP_EXTRACT_FUSION = OPTION_FUSION +
      ANY_SPACES + OPTION_FUSION_GROUP + INNER_OPTION + ANY_SPACES;
  private static final String REGEX_OPTION_SIMPLE = ANY_SPACES + INNER_OPTION;
  private static final String REGEX_OPTION_GROUP = ANY_SPACES
    + OPTION_FUSION_GROUP + REGEX_OPTION_SIMPLE + ANY_SPACES;
  private static final String REGEX_OPTION_RANGE = ANY_SPACES
    + OPTION_EXTRACT_RANGE + ANY_SPACES + RANGE;
  private static final String REGEX_OPTION_MAP = ANY_SPACES
    + OPTION_EXTRACT_MAP + ANY_SPACES + SIMPLE_MAPPING;
  private static final String REGEX_OPTION_ACC = ANY_SPACES
      + OPTION_EXTRACT_ACC + ANY_SPACES + INNER_OPTION + ANY_SPACES;

  /**
   * Get a string representation of the enum.
   * @return String value of the enum.
   */
  public String getName() {
    return toString().toLowerCase();
  }

  /**
   * Get the directive based on the token directly after the CLAW keyword.
   * @param pragma  The Xpragma in which the directive is searched.
   * @return ClawPragma enum value if a directive is found. Null otherwise.
   */
  public static ClawPragma getDirective(Xpragma pragma)
      throws IllegalDirectiveException
  {
    if(pragma == null || pragma.getValue() == null){
      throw new IllegalDirectiveException("", "directive is null", 0);
    }
    String[] parts = pragma.getValue().split(" ");
    if(parts.length < 2){
      throw new IllegalDirectiveException("",
          "cannot get the directive keyword", pragma.getLineNo());
    }
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
        throw new IllegalDirectiveException("",
            "unknown directive", pragma.getLineNo());
    }
  }

  /**
   * Get the value of the acc option.
   * @param pragma The pragma string value.
   * @return The acc option. Null if no option is set.
   */
  public static String getAccOptionValue(Xpragma pragma){
    if(pragma == null){
      return null;
    }
    Matcher matchOption = Pattern.compile(REGEX_OPTION_ACC,
        Pattern.CASE_INSENSITIVE).matcher(pragma.getValue());
    if(matchOption.find()) {
      if(matchOption.group(1).trim().length() == 0){
        return null;
      } else {
        return matchOption.group(1).trim();
      }
    }
    return null;
  }

  /**
   * Check if fusion option is defined.
   * @param pragma Xpragma object to check.
   * @return True if fusion option is defined. False otherwise.
   */
  public static boolean hasFusionOption(Xpragma pragma) {
    return !(pragma == null || pragma.getValue() == null)
        && pragma.getValue().contains(OPTION_FUSION);
  }

  /**
   * Extract the optional fusion group option value of loop-extract directive.
   * @param pragma Xpragma object.
   * @return The fusion group value. Null if not specified.
   */
  public static String getExtractFusionOption(Xpragma pragma){
    if(pragma == null || pragma.getValue() == null){
      return null;
    }
    Matcher m = Pattern.compile(REGEX_LOOP_EXTRACT_FUSION)
        .matcher(pragma.getValue());
    if(m.find()){
      return m.group(1);
    }
    return null;
  }


  /**
   * Get the group option value.
   * @param pragma Xprama object.
   * @return The fusion group value. Null if not specified.
   */
  public static String getGroupOptionValue(Xpragma pragma){
    if(pragma == null || pragma.getValue() == null){
      return null;
    }
    Matcher matchFullDirective = Pattern.compile(REGEX_LOOP_FUSION +
      REGEX_OPTION_GROUP, Pattern.CASE_INSENSITIVE).matcher(pragma.getValue());

    if(!matchFullDirective.matches()){
      return null;
    }

    Matcher matchOption = Pattern.compile(INNER_OPTION,
      Pattern.CASE_INSENSITIVE).matcher(pragma.getValue());
    if(matchOption.find()) {
      if(matchOption.group(1).trim().length() == 0){
        return null;
      } else {
        return matchOption.group(1).trim();
      }
    }
    return null;
  }

  /**
   * Extract simple option from the pragma statement.
   * @param pragma The Xpragma object to extract from.
   * @return The option value if present. Null otherwise.
   */
  public static String getSimpleOptionValue(Xpragma pragma){
    if(pragma == null || pragma.getValue() == null){
      return null;
    }
    try {
      if (getDirective(pragma) != ClawPragma.LOOP_INTERCHANGE) {
        return null;
      }
    } catch (IllegalDirectiveException itex){
      return null;
    }

    Matcher matchFullDirective = Pattern.compile(REGEX_LOOP_INTERCHANGE
      + REGEX_OPTION_SIMPLE,
      Pattern.CASE_INSENSITIVE).matcher(pragma.getValue());

    if(!matchFullDirective.matches()){
      return null;
    }

    Matcher matchOption = Pattern.compile(INNER_OPTION,
      Pattern.CASE_INSENSITIVE).matcher(pragma.getValue());
    if(matchOption.find()) {
      return matchOption.group(1);
    }
    return null;
  }

  /**
   * Check if the pragma statement starts with the claw keyword.
   * @param pragma The Xpragma object to check.
   * @return True if the statement starts with claw keyword. False otherwise.
   */
  public static boolean startsWithClaw(Xpragma pragma) {
    return !(pragma == null || pragma.getValue() == null)
        && pragma.getValue().startsWith(PREFIX_CLAW);
  }

  /**
   * Check if the parallel option is activated in the directive.
   * @param pragma The XcodeML pragma object to check.
   * @return True if the option is activated. False otherwise.
   */
  public static boolean hasParallelOption(Xpragma pragma) {
    return !(pragma == null || pragma.getValue() == null)
        && pragma.getValue().toLowerCase().contains(OPTION_EXTRACT_PARALLEL);
  }

  /**
   * Check the validity of a CLAW directives
   * @param pragma Xpragma object.
   * @return True if the directive is valid. False otherwise.
   */
  public static boolean isValid(Xpragma pragma){
    if(pragma == null || pragma.getValue() == null){
      return false;
    }
    String[] parts = pragma.getValue().split(" ");

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
      options = Utility.join(" ", temp);
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
        return true; // TODO maybe for 0.2
      case LOOP_EXTRACT:
        return ClawPragma.isRangeOptionValid(option)
            && ClawPragma.isMapOptionValid(option);
      case BASE_END:
        // Only remove is associated with end directive now
        return option.contains(DIRECTIVE_UTILITIES_REMOVE);
      default:
        return false;
    }
  }

  /**
   * Check if the group option is valid.
   * @param option The option value to check.
   * @return True if the option is valid. False otherwise.
   */
  private static boolean isGroupOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_GROUP);
  }

  /**
   * Check if the order option is valid.
   * @param option The option value to check.
   * @return True if the option is valid. False otherwise.
   */
  private static boolean isOrderOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_SIMPLE);
  }

  /**
   * Check if the range option is valid.
   * @param option The option value to check.
   * @return True if the option is valid. False otherwise.
   */
  private static boolean isRangeOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_RANGE);
  }

  /**
   * Check if the map option is valid.
   * @param option The option value to check.
   * @return True if the option is valid. False otherwise.
   */
  private static boolean isMapOptionValid(String option){
    return ClawPragma.checkOptionalOption(option, REGEX_OPTION_MAP);
  }

  /**
   * Check the validity of optional option based on refular expression.
   * @param option          The option value to check.
   * @param regex           The regular expression to be used for the check.
   * @return True if the option is valid. False otherwise.
   */
  private static boolean checkOptionalOption(String option, String regex) {
    if(option == null){ // option is not mandatory
      return true;
    }
    Matcher matchOption = Pattern.compile(regex, Pattern.CASE_INSENSITIVE)
      .matcher(option);

    if(matchOption.find()) {
      String match =  matchOption.group(1);
      return match.trim().length() > 0;
    }

    return false;
  }

  /**
   * Extract the mapping information from a pragma statement.
   * @param pragma The Xpragma object to extract from.
   * @return A list of ClawMapping objects containing the mapping information.
   * @throws IllegalDirectiveException
   */
  public static List<ClawMapping> extractMappingInformation(Xpragma pragma)
      throws IllegalDirectiveException
  {
    List<String> allMappings = new ArrayList<>();
    List<ClawMapping> mappings = new ArrayList<>();

    if(pragma == null || pragma.getValue() == null){
      return mappings;
    }

    Matcher m = Pattern.compile(REGEX_MAPPING)
        .matcher(pragma.getValue());
    while (m.find()) {
      allMappings.add(m.group(1));
    }

    for(String mappingClause : allMappings){
      System.out.println("MAPPING " + mappingClause);
      ClawMapping mapping = new ClawMapping(mappingClause);
      mappings.add(mapping);
    }
    return mappings;
  }

  /**
   * Extract the range option from a pragma statement.
   * @param pragma The Xpragma object to extract from.
   * @return A ClawRange object containing the range information.
   */
  public static ClawRange extractRangeInformation(Xpragma pragma) {
    if(pragma == null || pragma.getValue() == null){
      return null;
    }

    Matcher m1 = Pattern.compile(RANGE_GLOBAL).matcher(pragma.getValue());
    if(m1.find()){
      Matcher m2 = Pattern.compile(RANGE)
          .matcher(m1.group(1));
      if(m2.find()) {
        ClawRange range = new ClawRange();
        range.setInductionVar(m2.group(1));
        range.setLowerBound(m2.group(2));
        range.setUpperBound(m2.group(3));
        range.setStep(m2.group(4));
        return range;
      }
    }

    return null;
  }

  /**
   * Check if the statement is a directive.
   * @return True if the statement is a directive. False otherwise.
   */
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

  /**
   * Check if the statement is in the loop category.
   * @return True if the statement is a loop statement. False otherwise.
   */
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

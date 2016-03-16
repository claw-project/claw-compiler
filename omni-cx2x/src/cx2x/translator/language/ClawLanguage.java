/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.misc.ParseCancellationException;

import java.util.List;

/**
 * ClawLanguage class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawLanguage {

  private ClawDirective _directive;
  private String _groupName;
  private List<String> _indexes;
  private boolean _valid, _hasGroup, _hasIndexes;

  /**
   * Constructs an empty ClawLanguage section.
   * WARNING: This ctor should only be used by the parser.
   */
  protected ClawLanguage(){
    _directive = null;
    _valid = false;
    _hasGroup = false;
    _hasIndexes = false;
    _groupName = null;
    _indexes = null;
  }


  /**
   * Analyze a raw string input and match it with the CLAW language definition.
   * @param rawInput A string line to be analyzed against the CLAW language.
   * @return A ClawLanguage object with the corresponding extracted information.
   */
  public static ClawLanguage analyze(String rawInput)
      throws IllegalDirectiveException
  {
    // Instantiate the lexer with the raw string input
    ClawLexer lexer = new ClawLexer(new ANTLRInputStream(rawInput));

    // Get a list of matched tokens
    CommonTokenStream tokens = new CommonTokenStream(lexer);

    // Pass the tokens to the parser
    ClawParser parser = new ClawParser(tokens);
    parser.setErrorHandler(new BailErrorStrategy());

    try {
      // Start the parser analysis from the "analyze" entry point
      ClawParser.AnalyzeContext ctx = parser.analyze();
      // Get the ClawLanguage object return by the parser after analysis.
      return ctx.language;
    } catch(ParseCancellationException pcex){
      throw new IllegalDirectiveException(rawInput, "Unvalid CLAW directive");
    }
  }

  /**
   * Check whether the current language section is valid.
   * @return True if the language is valid.
   */
  public boolean isValid(){
    return _valid;
  }



  // Group option specific methods

  /**
   * Check whether a group option was specified.
   * @return True if group option was specified.
   */
  public boolean hasGroupOption(){
    return _hasGroup;
  }

  /**
   * Set the group name and hasGroupOption to true
   * @param groupName The group name defined in the group option.
   */
  protected void setGroupOption(String groupName){
    _hasGroup = true;
    _groupName = groupName;
  }

  /**
   * Get the group name defined in the group option.
   * @return The group name as a String value.
   */
  public String getGroupName(){
    return _groupName;
  }

  // Loop interchange specific methods

  /**
   * Set the list of interhcnage indexes.
   * @param indexes List of indexes as string.
   */
  public void setIdList(List<String> indexes){
    _hasIndexes = true;
    _indexes = indexes;
  }

  /**
   * Get the loop index list
   * @return List of loop index
   */
  public List<String> getIndexes(){
    return _indexes;
  }

  /**
   * Check whether the interchange directive has indexes values.
   * @return True if the directive has interchange value.
   */
  public boolean hasIndexes(){
    return _hasIndexes;
  }

  // Directive generic method

  /**
   * Define the current directive of the language section.
   * @param directive A value of the ClawDirective enumeration.
   */
  public void setDirective(ClawDirective directive){
    _directive = directive;
  }

  /**
   * Get the current directive of the language section.
   * @return Value of the current directive.
   */
  public ClawDirective getDirective(){
    return _directive;
  }




}

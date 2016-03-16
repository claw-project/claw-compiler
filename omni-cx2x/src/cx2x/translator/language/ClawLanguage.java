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

/**
 * ClawLanguage class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawLanguage {

  private ClawDirective _directive;
  private String _groupName;
  private boolean _valid, _hasGroup;

  /**
   * Constructs an empty ClawLanguage section.
   * WARNING: This ctor should only be used by the parser.
   */
  protected ClawLanguage(){
    _directive = null;
    _valid = false;
    _hasGroup = false;
    _groupName = null;
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

  /**
   * Check whether a group option was specified.
   * @return True if group option was specified.
   */
  public boolean hasGroupOption(){
    return _hasGroup;
  }

  public void setGroupOption(String groupName){
    _hasGroup = true;
    _groupName = groupName;
  }

  public String getGroupName(){
    return _groupName;
  }

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

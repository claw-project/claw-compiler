/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.exception;

import cx2x.translator.common.Utility;
import org.antlr.v4.runtime.Token;

import java.util.List;

/**
 * Exception thrown during the analysis of a directive.
 *
 * @author clementval
 */
public class IllegalDirectiveException extends Exception {

  private int _directiveLine = 0;
  private int _charPos = 0;
  private String _directive;
  private Token _offendingToken = null;
  private List<String> _expectedTokens = null;

  /**
   * Constructs a new exception with a specific detail message and clause.
   *
   * @param directive Illegal directive
   * @param message   Specific exception message.
   */
  public IllegalDirectiveException(String directive, String message) {
    super(message);
    _directive = directive;
  }

  /**
   * Constructs a new exception with an offending token.
   *
   * @param offendingToken Token that break the parsing.
   * @param lineno         Line number where the parsing error occurred.
   * @param charPos        Char position where the parsing error occurred.
   */
  public IllegalDirectiveException(Token offendingToken, int lineno,
                                   int charPos)
  {
    _offendingToken = offendingToken;
    _directiveLine = lineno;
    _charPos = charPos;
  }

  /**
   * Constructs a new exception with a set of expected tokens.
   *
   * @param expectedTokens Set of expected tokens.
   * @param lineno         Line number where the parsing error occurred.
   * @param charPos        Char position where the parsing error occurred.
   */
  public IllegalDirectiveException(List<String> expectedTokens, int lineno,
                                   int charPos)
  {
    _expectedTokens = expectedTokens;
    _directiveLine = lineno;
    _charPos = charPos;
  }

  /**
   * Constructs a new exception with a specific detail message and line number.
   *
   * @param directive Illegal directive
   * @param message   Specific exception message.
   * @param lineno    Line number where the parsing error occurred.
   */
  public IllegalDirectiveException(String directive, String message, int lineno)
  {
    super(message);
    _directive = directive;
    _directiveLine = lineno;
  }

  /**
   * Constructs a new exception with a specific detail message, line number and
   * char position.
   *
   * @param directive Illegal directive
   * @param message   Specific exception message.
   * @param lineno    Line number where the parsing error occurred.
   * @param charPos   Char position where the parsing error occurred.
   */
  public IllegalDirectiveException(String directive, String message, int lineno,
                                   int charPos)
  {
    super(message);
    _directive = directive;
    _directiveLine = lineno;
    _charPos = charPos;
  }

  /**
   * Get the illegal directive.
   *
   * @return The illegal directive.
   */
  public String getDirective() {
    return _directive;
  }

  /**
   * Set the illegal directive.
   *
   * @param directive The directive.
   */
  public void setDirective(String directive) {
    _directive = directive;
  }

  /**
   * Get the line of the code that contains the illegal directive.
   *
   * @return Line number in the XcodeML that triggered the exception.
   */
  public int getDirectiveLine() {
    return _directiveLine;
  }

  /**
   * Set the line of the code that contains the illegal directive.
   *
   * @param lineno Line number in the XcodeML that triggered the exception.
   */
  public void setDirectiveLine(int lineno) {
    _directiveLine = lineno;
  }

  /**
   * Get the character position where the directive error happened.
   *
   * @return Character position in the line.
   */
  public int getCharPosition() {
    return _charPos;
  }


  /**
   * Get the offending token that triggered the Exception.
   *
   * @return Offending token (ANTLR object).
   */
  public Token getOffendingToken() {
    return _offendingToken;
  }

  /**
   * Get the list of expected token.
   *
   * @return List of expected tokens or NULL.
   */
  public List<String> getExpectedTokens() {
    return _expectedTokens;
  }

  @Override
  public String getMessage() {
    if(_expectedTokens != null) {
      return getExpectingTokenMsg();
    } else if(_offendingToken != null) {
      return getExpectingByFoundTokenMsg();
    }
    return getStdMsg();
  }

  /**
   * Get the standard error message if no offending token is provided.
   *
   * @return Standard error message.
   */
  private String getStdMsg() {
    String errorMessage = getMsgPrefix();
    errorMessage += ": " + super.getMessage();
    return errorMessage;
  }

  /**
   * Get a specific error message when an offending token is provided.
   *
   * @return Specific error message including offending and expecting tokens.
   */
  private String getExpectingByFoundTokenMsg() {
    String errorMessage = getMsgPrefix();
    errorMessage += ": Expected X but found \"" +
        _offendingToken.getText() + "\"";
    return errorMessage;
  }

  /**
   * Get a specific error message when an offending token is provided.
   *
   * @return Specific error message including offending and expecting tokens.
   */
  private String getExpectingTokenMsg() {
    String errorMessage = getMsgPrefix();
    errorMessage += ": Expecting " + Utility.join(",", _expectedTokens);
    return errorMessage;
  }

  /**
   * Get the standard prefix for the illegal directive error message.
   *
   * @return Standard error message prefix.
   */
  private String getMsgPrefix() {
    String errorMessage = "Illegal directive ";
    if(_directiveLine > 0) {
      errorMessage += _directiveLine + ":" + _charPos;
    } else {
      errorMessage += "-:" + _charPos;
    }
    return errorMessage;
  }
}

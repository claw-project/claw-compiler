/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.exception;

/**
 * Exception thrown during the analysis of a directive.
 *
 * @author clementval
 */
public class IllegalDirectiveException extends Exception {
  private int _directiveLine = 0;
  private int _charPos = 0;
  private String _directive;

  /**
   * Constructs a new exception with a specific detail message and clause.
   * @param directive Illegal directive
   * @param message   Specific exception message.
   */
  public IllegalDirectiveException(String directive, String message){
    super(message);
    _directive = directive;
  }

  /**
   * Constructs a new exception with a specific detail message and line number.
   * @param directive Illegal directive
   * @param message   Specific exception message.
   * @param lineno    Line number of the directive.
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
   * @param directive Illegal directive
   * @param message   Specific exception message.
   * @param lineno    Line number of the directive.
   * @param charPos   Character position where the directive error happened.
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
   * @return The illegal directive.
   */
  public String getDirective(){
    return _directive;
  }

  /**
   * Set the illegal directive.
   * @param directive The directive.
   */
  public void setDirective(String directive){
    _directive = directive;
  }

  /**
   * Set the line of the code that contains the illegal directive.
   * @param lineno Line number in the XcodeML that triggered the exception.
   */
  public void setDirectiveLine(int lineno) {
    _directiveLine = lineno;
  }

  /**
   * Get the line of the code that contains the illegale directive.
   * @return Line number in the XcodeML that triggered the exception.
   */
  public int getDirectiveLine() {
    return _directiveLine;
  }

  /**
   * Get the character position where the directive error happened.
   * @return Character position in the line.
   */
  public int getCharPosition() {
    return _charPos;
  }

  @Override
  public String getMessage() {
    String errorMessage = "Illegal directive ";

    if(_directiveLine > 0) {
      errorMessage += _directiveLine + ":" +  _charPos;
    } else {
      errorMessage += "-:" + _charPos;
    }
    errorMessage += " : " + super.getMessage();
    return errorMessage;
  }
}

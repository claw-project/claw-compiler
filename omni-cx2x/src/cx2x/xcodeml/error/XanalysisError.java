/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.error;

/**
 * This class hold information about error happening during transformation
 * analyzis.
 *
 * @author clementval
 */

public class XanalysisError {
  private final String _errorMsg;
  private final int _errorLineNumber;

  /**
   * Default ctor.
   * @param msg     Error message
   * @param lineno  Line number that triggered the error.
   */
  public XanalysisError(String msg, int lineno){
    _errorMsg = msg;
    _errorLineNumber = lineno;
  }

  /**
   * @return The error message
   */
  public String getMessage(){
    return _errorMsg;
  }

  /**
   * @return The line number that triggered the error.
   */
  public int getLine(){
    return _errorLineNumber;
  }
}

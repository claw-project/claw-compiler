/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.parser;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

/**
 * Listener that save the last error information from the CLAW language parser.
 *
 * @author clementval
 */
public class ClawErrorListener extends BaseErrorListener {

  private static IllegalDirectiveException ex = null;

  /**
   * Default ctor
   */
  public ClawErrorListener() {
  }

  @Override
  public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol,
                          int line, int charPositionInLine,
                          String msg, RecognitionException e)
  {
    ex = new IllegalDirectiveException("", msg, line, charPositionInLine);
  }

  /**
   * Get the last error information.
   *
   * @return IllegalDirectiveException encapsulating the last error information.
   */
  public IllegalDirectiveException getLastError() {
    return ex;
  }

}

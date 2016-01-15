package cx2x.translator.exception;

/**
 * Exception thrown during the transformation process
 *
 * @author Valentin Clement
 */
public class IllegalTransformationException extends Exception {
  private int _transformationStartLine = 0;

  public IllegalTransformationException() {
    super();
  }

  public IllegalTransformationException(int lineno) {
    super();
    _transformationStartLine = lineno;
  }

  public IllegalTransformationException(String message) {
    super(message);
  }

  public IllegalTransformationException(String message, int lineno) {
    super(message);
    _transformationStartLine = lineno;
  }

  public IllegalTransformationException(String message, Throwable cause) {
    super(message, cause);
  }

  public IllegalTransformationException(String message, Throwable cause,
    int lineno)
  {
    super(message, cause);
    _transformationStartLine = lineno;
  }

  public IllegalTransformationException(Throwable cause) {
    super(cause);
  }

  public IllegalTransformationException(Throwable cause, int lineno) {
    super(cause);
    _transformationStartLine = lineno;
  }

  /**
   * Set the start line of the transformation that triggered the exception.
   * @param lineno Line number
   */
  public void setStartLine(int lineno) {
    _transformationStartLine = lineno;
  }

  /**
   * Get the start line of the transformation that triggered the exception.
   * @return Line number
   */
  public int getStartLine() {
    return _transformationStartLine;
  }
}

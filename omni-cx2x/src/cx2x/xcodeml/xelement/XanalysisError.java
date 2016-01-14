package cx2x.xcodeml.xelement;

public class XanalysisError {
  private String _errorMsg;
  private int _errorLineNumber = 0;

  public XanalysisError(String msg, int lineno){
    _errorMsg = msg;
    _errorLineNumber = lineno;
  }

  public String getMessage(){
    return _errorMsg;
  }

  public int getLine(){
    return _errorLineNumber;
  }
}

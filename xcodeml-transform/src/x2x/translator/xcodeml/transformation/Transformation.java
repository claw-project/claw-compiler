package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.xelement.exception.*;
import x2x.translator.xcodeml.transformer.Transformer;


public abstract class Transformation<T> {
  protected boolean _transformed = false;
  protected Xpragma _pragma = null;
  protected int _startLine = 0;

  public abstract boolean analyze(XcodeProg xcodeml, Transformer translator);
  public abstract boolean canBeTransformedWith(T other);
  public abstract void transform(XcodeProg xcodeml, Transformer translator,
    T other) throws IllegalTransformationException;

  public Transformation(Xpragma pragma){
    _pragma = pragma;
    if(_pragma != null){
      _startLine = _pragma.getLine();
    }
  }

  public Xpragma getPragma(){
    return _pragma;
  }

  public int getStartLine(){
    return _startLine;
  }

  public void setStartLine(int lineno){
    _startLine = lineno;
  }

  public boolean isTransformed(){
    return _transformed;
  }

  public void transformed(){
    _transformed = true;
  }
}

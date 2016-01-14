package cx2x.translator.xcodeml.transformation;

import cx2x.xcodeml.xelement.*;
import cx2x.translator.exception.*;
import cx2x.translator.xcodeml.transformer.Transformer;

/**
 * A Transformation is an object capable of analyzing a possible code
 * transformation to be applied and the steps to apply it to the intermediate
 * representation. Normally, only derived classes of Transformation should be
 * applied as the base class does not implement the core methods.
 *
 * @author Valentin Clement
 */

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

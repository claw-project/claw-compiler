package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface Transformation<T> {
  public boolean analyze(XcodeProg xcodeml);
  public boolean isTransformed();
  public boolean canBeTransformedWith(T other);
  public void transform(XcodeProg xcodeml, T other);
}

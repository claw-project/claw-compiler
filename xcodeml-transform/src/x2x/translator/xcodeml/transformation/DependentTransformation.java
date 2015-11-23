package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface DependentTransformation<T> {
  public boolean analyze();
  public boolean isTransformed();
  public boolean canBeTransformedWith(T other);
  public void transform(XcodeProg xcodeml, T other);
}

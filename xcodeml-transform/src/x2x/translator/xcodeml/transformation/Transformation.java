package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;
import x2x.translator.xcodeml.transformer.Transformer;

public interface Transformation<T> {
  public boolean analyze(XcodeProg xcodeml, Transformer translator);
  public boolean isTransformed();
  public boolean canBeTransformedWith(T other);
  public void transform(XcodeProg xcodeml, Transformer translator, T other);
}

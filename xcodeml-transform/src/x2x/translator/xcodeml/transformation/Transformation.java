package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface Transformation {
  public boolean analyze();
  public void transform(XcodeProg xcodeml);
}

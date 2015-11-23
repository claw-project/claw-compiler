package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface TransformationGroup<T extends Transformation> {
  public int count();
  public void add(T transformation);
  public void applyTranslations(XcodeProg xcodeml);
}

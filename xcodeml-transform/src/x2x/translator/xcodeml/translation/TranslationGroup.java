package x2x.translator.xcodeml.translation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface TranslationGroup<T extends Translation> {
  public int count();
  public void add(T translation);
  public void applyTranslations(XcodeProg xcodeml);
}

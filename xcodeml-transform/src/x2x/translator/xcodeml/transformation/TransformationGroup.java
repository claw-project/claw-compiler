package x2x.translator.xcodeml.transformation;

import x2x.translator.xcodeml.xelement.XcodeProg;
import x2x.translator.xcodeml.transformer.Transformer;

import java.util.ArrayList;

public abstract class TransformationGroup<T extends Transformation> {
  private String _name = null;
  protected ArrayList<T> _translations = null;

  public TransformationGroup(String name){
    _name = name;
    _translations = new ArrayList<T>();
  }

  public int count(){
    return _translations.size();
  }

  public void add(T translation){
    _translations.add(translation);
  }

  public String transformationName(){
    return _name;
  }

  public abstract void applyTranslations(XcodeProg xcodeml,
    Transformer transformer);

}

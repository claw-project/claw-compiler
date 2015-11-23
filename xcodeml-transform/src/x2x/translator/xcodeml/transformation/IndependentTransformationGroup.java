package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.translator.xcodeml.xelement.XcodeProg;



public class IndependentTransformationGroup<T extends Transformation> implements TransformationGroup<T> {

  private ArrayList<T> _translations = null;

  public IndependentTransformationGroup() {
    _translations = new ArrayList<T>();
  }

  public int count(){
    return _translations.size();
  }

  public void add(T translation){
    _translations.add(translation);
  }

  public void applyTranslations(XcodeProg xcodeml){
    for(int i = 0; i < _translations.size(); ++i){
      T translation = _translations.get(i);
      translation.transform(xcodeml);
    }
  }
}

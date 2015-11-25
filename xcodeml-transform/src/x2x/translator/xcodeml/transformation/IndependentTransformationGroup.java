package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.translator.xcodeml.xelement.XcodeProg;



public class IndependentTransformationGroup<T extends Transformation<? super T>> extends TransformationGroup<T> {

  public IndependentTransformationGroup(String name) {
    super(name);
  }

  public void applyTranslations(XcodeProg xcodeml){
    for(int i = 0; i < _translations.size(); ++i){
      T translation = _translations.get(i);
      translation.transform(xcodeml, null);
    }
  }
}

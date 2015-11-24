package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.translator.xcodeml.xelement.XcodeProg;



public class DependentTransformationGroup<T extends Transformation> implements TransformationGroup<T> {

  private ArrayList<T> _translations = null;

  public DependentTransformationGroup() {
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
      T base = _translations.get(i);
      for(int j = i + 1; j < _translations.size(); ++j){
        T candidate = _translations.get(j);
        if(candidate.isTransformed()){
          continue;
        }
        if(base.canBeTransformedWith(candidate)){
          base.transform(xcodeml, candidate);
        }
      }
    }
  }
}

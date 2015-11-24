package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.translator.xcodeml.xelement.XcodeProg;



public class DependentTransformationGroup<T extends Transformation> extends TransformationGroup<T> {

  public DependentTransformationGroup(String name) {
    super(name);
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

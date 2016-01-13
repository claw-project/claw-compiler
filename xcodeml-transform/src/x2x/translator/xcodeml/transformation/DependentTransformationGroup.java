package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.translator.xcodeml.xelement.XcodeProg;
import x2x.translator.xcodeml.xelement.exception.*;
import x2x.translator.xcodeml.transformer.Transformer;

/**
 * An dependent transformation group check wether it can be transformed with
 * another pending transformation in the pipeline. Each transformation are
 * applied only once.
 */

public class DependentTransformationGroup<T extends Transformation<? super T>> extends TransformationGroup<T> {

  public DependentTransformationGroup(String name) {
    super(name);
  }

  public void applyTranslations(XcodeProg xcodeml, Transformer transformer)
    throws IllegalTransformationException
  {
    for(int i = 0; i < _translations.size(); ++i){
      T base = _translations.get(i);
      for(int j = i + 1; j < _translations.size(); ++j){
        T candidate = _translations.get(j);
        if(candidate.isTransformed()){
          continue;
        }
        if(base.canBeTransformedWith(candidate)){
          try {
            base.transform(xcodeml, transformer, candidate);
          } catch (IllegalTransformationException itex) {
            // Catch the exception to add line information and rethrow it
            if(itex.getStartLine() == 0){
              itex.setStartLine(base.getStartLine());
              throw itex;
            }
          }
        }
      }
    }
  }


  // In the dependent transformation group, the order in which transformation
  // appears is important
  public void add(T translation){
    int linePosition = translation.getStartLine();
    int insertIndex = 0;
    for(Transformation t : _translations){
      if(t.getStartLine() > linePosition){
        break;
      }
      ++insertIndex;
    }
    _translations.add(insertIndex, translation);
  }
}

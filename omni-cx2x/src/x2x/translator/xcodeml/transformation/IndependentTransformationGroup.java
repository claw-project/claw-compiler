package x2x.translator.xcodeml.transformation;

import java.util.ArrayList;

import x2x.xcodeml.xelement.XcodeProg;
import x2x.translator.exception.*;
import x2x.translator.xcodeml.transformer.Transformer;

/**
 * An independent transformation group applies each transformation without
 * checking with any other transformation in the pipeline.
 */

public class IndependentTransformationGroup<T extends Transformation<? super T>> extends TransformationGroup<T> {

  public IndependentTransformationGroup(String name) {
    super(name);
  }

  public void applyTranslations(XcodeProg xcodeml, Transformer transformer)
    throws IllegalTransformationException
  {
    for(T translation : _translations){

      try {
        translation.transform(xcodeml, transformer, null);
      } catch (IllegalTransformationException itex) {
        // Catch the exception to add line information and rethrow it
        if(itex.getStartLine() == 0){
          itex.setStartLine(translation.getStartLine());
          throw itex;
        }
      }

    }
  }
}

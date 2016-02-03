/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.exception.*;

/**
 * An dependent transformation group check wether it can be transformed with
 * another pending transformation in the pipeline. Each transformation are
 * applied only once.
 *
 * @author clementval
 */

public class DependentTransformationGroup<T extends Transformation<? super T>>
    extends TransformationGroup<T>
{

  /**
   * DependentTransformationGroup ctor.
   * @param name A friendly name to describe the transformation group.
   */
  public DependentTransformationGroup(String name) {
    super(name);
  }

  /**
   * @see TransformationGroup#applyTranslations(XcodeProgram, Transformer)
   */
  public void applyTranslations(XcodeProgram xcodeml, Transformer transformer)
    throws Exception
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
            }
            throw itex;
          } catch(Exception ex){
            throw ex;
          }
        }
      }
    }
  }

  /**
   * Add a new transformation in the group. As transformation are dependent
   * between each other, the position in the list is determined by the
   * transformation's start line.
   * @see TransformationGroup#add(Transformation)
   */
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

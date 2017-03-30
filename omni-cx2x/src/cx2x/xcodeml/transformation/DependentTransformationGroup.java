/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.xnode.XcodeProgram;

import java.util.List;

/**
 * An dependent transformation group check whether it can be transformed with
 * another pending transformation in the pipeline. Each transformation are
 * applied only once.
 *
 * @author clementval
 */

public class DependentTransformationGroup extends TransformationGroup {

  /**
   * DependentTransformationGroup ctor.
   *
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
    List<Transformation> transformations = getTransformations();
    for(int i = 0; i < transformations.size(); ++i) {
      Transformation base = transformations.get(i);
      for(int j = i + 1; j < transformations.size(); ++j) {
        Transformation candidate = transformations.get(j);
        if(candidate.isTransformed()) {
          continue;
        }
        if(base.canBeTransformedWith(xcodeml, candidate)) {
          try {
            base.transform(xcodeml, transformer, candidate);
          } catch(IllegalTransformationException itex) {
            // Catch the exception to add line information and rethrow it
            if(itex.getStartLine() == 0) {
              itex.setStartLine(base.getStartLine());
            }
            throw itex;
          }
        }
      }
    }
  }

  /**
   * Add a new transformation in the group. As transformation are dependent
   * between each other, the position in the list is determined by the
   * transformation's start line.
   *
   * @see TransformationGroup#add(Transformation)
   */
  @Override
  public void add(Transformation transformation) {
    int linePosition = transformation.getStartLine();
    int insertIndex = 0;
    for(Transformation t : getTransformations()) {
      if(t.getStartLine() > linePosition) {
        break;
      }
      ++insertIndex;
    }
    getTransformations().add(insertIndex, transformation);
  }
}

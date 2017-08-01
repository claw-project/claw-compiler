/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.xnode.XcodeProgram;

/**
 * An independent transformation group applies each transformation without
 * checking with any other transformation in the pipeline.
 *
 * @author clementval
 */

public class IndependentTransformationGroup extends TransformationGroup {

  /**
   * IndependentTransformationGroup ctor
   *
   * @param name A friendly name to describe the transformation group.
   */
  public IndependentTransformationGroup(String name) {
    super(name);
  }

  /**
   * @see TransformationGroup#applyTranslations(XcodeProgram, Translator)
   */
  public void applyTranslations(XcodeProgram xcodeml, Translator translator)
      throws Exception
  {
    for(Transformation trans : getTransformations()) {
      try {
        trans.transform(xcodeml, translator, null);
        if(trans.isTransformed()) {
          incrementAppliedTransformation();
        }
      } catch(IllegalTransformationException itex) {
        // Catch the exception to add line information and rethrow it
        if(itex.getStartLine() == 0) {
          itex.setStartLine(trans.getStartLine());
        }
        throw itex;
      }
    }
  }
}

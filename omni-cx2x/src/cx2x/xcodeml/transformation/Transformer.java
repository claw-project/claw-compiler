/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import java.util.Map;

/**
 * Transformer interface
 *
 * Transformer stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */

public interface Transformer {

  /**
   * Add a transformation in a transformation group stored in the transformer.
   * @param t The transformation to be added.
   */
  void addTransformation(Transformation t);

  /**
   * Get all transformation groups stored in this transformer.
   * @return A list of all transformation groups.
   */
  //List<TransformationGroup> getGroups();
  Map<Class, TransformationGroup> getGroups();



  /**
   * Get the next transformation counter value.
   * @return Transformation counter value.
   */
  int getNextTransformationCounter();
}

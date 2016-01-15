/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformer;

import cx2x.translator.transformation.*;
import java.util.List;

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
   * Add a transformation group to the transformer.
   * @param tg        The transformation group to be added.
   * @param position  The insert position of the transformation group. If the
   *                  position is out of bound, the transformation group is
   *                  added at the end of the list.
   */
  void addTransformationGroup(TransformationGroup tg, int position);

  /**
   * Get all transformation groups stored in this transformer.
   * @return A list of all transformation groups.
   */
  List<TransformationGroup> getGroups();
}

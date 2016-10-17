/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import java.util.Map;

/**
 * Transformer interface
 * <p>
 * Transformer stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */

public interface Transformer {

  /**
   * Add a transformation in a transformation group stored in the transformer.
   *
   * @param t The transformation to be added.
   */
  void addTransformation(Transformation t);

  /**
   * Get all transformation groups stored in this transformer.
   *
   * @return A list of all transformation groups.
   */
  Map<Class, TransformationGroup> getGroups();

  /**
   * Get the next transformation counter value.
   *
   * @return Transformation counter value.
   */
  int getNextTransformationCounter();

  /**
   * Get the internal module cache.
   *
   * @return Module cache.
   */
  ModuleCache getModCache();

  /**
   * Get the maximum number of columns.
   *
   * @return Max number of columns.
   */
  int getMaxColumns();

  /**
   * Set the maximum number of columns.
   *
   * @param max Max number of columns.
   */
  void setMaxColumns(int max);
}

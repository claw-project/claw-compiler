/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

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
   * Generate transformation according to the pragma.
   *
   * @param pragma Pragma that can trigger a transformation.
   */
  void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
      throws IllegalDirectiveException;

  /**
   * Add a transformation to the transformer.
   *
   * @param xcodeml Current translation unit.
   * @param t       Transformation to add.
   */
  void addTransformation(XcodeProgram xcodeml, Transformation t);

  /**
   * Check if the given pragma can be handled by the current transformer.
   *
   * @param pragma Pragma statement node.
   * @return True if the transformer can handle the pragma. False otherwise.
   */
  boolean isHandledPragma(Xnode pragma);

  /**
   * Perform last tasks before applying transformations.
   */
  void finalize(XcodeProgram xcodeml);

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

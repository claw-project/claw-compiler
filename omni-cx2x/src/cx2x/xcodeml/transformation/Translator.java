/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.transformation;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

import java.util.Map;

/**
 * Translator interface
 * <p>
 * Translator stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */

public interface Translator {

  /**
   * Generate transformation according to the pragma.
   *
   * @param pragma Pragma that can trigger a transformation.
   */
  void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
      throws IllegalDirectiveException, IllegalTransformationException;

  /**
   * Add a transformation to the translator.
   *
   * @param xcodeml Current translation unit.
   * @param t       Transformation to add.
   */
  void addTransformation(XcodeProgram xcodeml, Transformation t)
      throws IllegalTransformationException;

  /**
   * Check if the given pragma can be handled by the current translator.
   *
   * @param pragma Pragma statement node.
   * @return True if the translator can handle the pragma. False otherwise.
   */
  boolean isHandledPragma(Xnode pragma);

  /**
   * Perform last tasks before applying transformations.
   */
  void finalize(XcodeProgram xcodeml) throws IllegalTransformationException;

  /**
   * Get all transformation groups stored in this translator.
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
}

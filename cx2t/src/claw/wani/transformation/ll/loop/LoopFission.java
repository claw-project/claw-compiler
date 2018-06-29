/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

/**
 * A LoopFission transformation is an independent transformation that can
 * be used to split a large loop into multiple smaller ones. If the
 * proposed split violates data dependencies within the original loop
 * body, the transformation will attempt to promote the according variables
 * to maintain the correctness of the code.
 *
 * Please note that this is still experimental at this stage!
 *
 * @author mlange05
 */

public class LoopFission extends ClawTransformation {

  /**
   * Constructs a new LoopFission triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fission
   *                  transformation.
   */
  public LoopFission(ClawPragma directive) {
    super(directive);
  }

  /**
   * Loop fission analysis:
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    return true;
  }

  /**
   * Apply the loop fission transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are
   *                       applied.
   * @param translator     The translator used to applied the transformations.
   * @param transformation The other loop fission unit to be merge with this
   *                       one.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    if(!(transformation instanceof LoopFission)) {
      throw new IllegalTransformationException("Incompatible transformation",
          _claw.getPragma().lineNo());
    }
    LoopFission other = (LoopFission) transformation;

    // Clean up
    other.removePragma();
    this.removePragma();

    other.transformed();
  }

  /**
   * Check whether two loop fission units can be split after one another
   * 
   * @param transformation The other loop fission unit to be split.
   * @return True since the splits are independent (for now)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    return true;  // Independent transformation
  }

}

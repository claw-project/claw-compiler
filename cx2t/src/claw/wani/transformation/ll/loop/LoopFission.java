/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Loop;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeML;
import claw.tatsu.xcodeml.xnode.common.Xnode;
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

  // The do loop that we want to split
  private Xnode _loop;
  private Xnode _marker;

  /**
   * Constructs a new LoopFission triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fission
   *                  transformation.
   */
  public LoopFission(ClawPragma directive) throws IllegalTransformationException
    {
    super(directive);

    this._marker = directive.getPragma();
    this._loop = this._marker.ancestor().ancestor();
    if (this._loop.opcode() != Xcode.F_DO_STATEMENT) {
      throw new IllegalTransformationException("Incompatible transformation",
          _claw.getPragma().lineNo());
    };
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
   * @param transformation Only for dependent transformation. The other
   *                       transformation part of the transformation.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    // Create new loop with identical variable and index range
    Xnode inductionVar = this._loop.matchDirectDescendant(Xcode.VAR).cloneNode();
    Xnode indexRange = this._loop.matchDirectDescendant(Xcode.INDEX_RANGE).cloneNode();
    Xnode newLoop = xcodeml.createDoStmt(inductionVar, indexRange);

    // Split the loop and insert new loop as a sibling
    Xnode parent = this._loop.ancestor();
    Loop.split(this._loop, this._marker, newLoop);
    parent.insertAfter(newLoop, this._loop);

    // Clean up
    removePragma();
    transformed();
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
    return false;  // Independent transformation
  }

}

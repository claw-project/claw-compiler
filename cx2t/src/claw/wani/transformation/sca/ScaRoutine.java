/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

/**
 * @author clementval
 */
public class ScaRoutine extends ClawTransformation {

  /**
   * Constructs a new Sca transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaRoutine(ClawPragma directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    removePragma();
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // independent transformation
  }

}

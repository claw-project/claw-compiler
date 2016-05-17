/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;

/**
 * The parallelize transformation transforms the code contained in a
 * subtourine/function by adding necessary dimensions and parallelism to the
 * defined data.
 *
 * @author clementval
 */
public class Parallelize extends Transformation {

  private final ClawLanguage _claw;

  /**
   * Constructs a new Parallelize transfomration triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
    _claw = directive; // Keep information about the claw directive here
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other)
      throws Exception
  {

  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // This is an independent transformation
  }
}

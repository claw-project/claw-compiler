package claw.wani.transformation.serialization;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

public class Serialize extends ClawTransformation {

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public Serialize(ClawPragma directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // Independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {

  }
}

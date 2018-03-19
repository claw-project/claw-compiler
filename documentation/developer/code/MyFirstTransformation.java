/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.wani.transformation;

import cx2x.wani.language.ClawPragma;
import cx2x.wani.transformation.ClawTransformation;
import cx2x.shenron.transformation.Transformation;
import cx2x.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common;

/**
 * Simple transformation for documentation example
 */
public class MyFirstTransformation extends ClawTransformation {

  // Constructor that received the analyzed pragma as argument
  public MyFirstTransformation(ClawPragma directive) {
    super(directive);
  }

  // The analyzis step
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    return true;
  }

  // The transformation step
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other) throws Exception
  {
    removePragma();
  }

  // Only used by dependent transformation
  public boolean canBeTransformedWith(Transformation other) {
    return false; // Independent transformation
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.XcodeProgram;

/**
 * Simple transformation for documentation example
 */
public class MyFirstTransformation extends ClawTransformation {

  // Constructor that received the analyzed pragma as argument
  public MyFirstTransformation(ClawLanguage directive) {
    super(directive);
  }

  // The analyzis step
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  // The transformation step
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
    _claw.delete();
  }

  // Only used by dependent transformation
  public boolean canBeTransformedWith(Transformation other) {
    return false; // Independent transformation
  }
}

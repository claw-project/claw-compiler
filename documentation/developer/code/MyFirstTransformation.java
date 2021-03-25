/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author: not specified
 */
package claw.wani.transformation;

import claw.wani.transformation.ClawTransformation;
import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;  

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
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other) {
    return false; // Independent transformation
  }
}

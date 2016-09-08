/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.utility;

import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.XcodeProgram;

/**
 * Transformation to fix some issues raised in XcodeML.
 *
 * @author clementval
 */
public class XcodeMLWorkaround extends Transformation {

  /**
   * XcodeMLWorkaround ctor.
   * @param directive The directive that triggered the transformation.
   */
  public XcodeMLWorkaround(AnalyzedPragma directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // Independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {

  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.utility;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.XcodeProgram;

/**
 * Transformation to fix some issues raised in XcodeML.
 *
 * @author clementval
 */
public class XcodeMLWorkaround extends Transformation {

  private final ClawLanguage _claw;

  /**
   * XcodeMLWorkaround ctor.
   * @param claw The directive that triggered the transformation. In case of
   *             this specific transformation, there is no directive.
   */
  public XcodeMLWorkaround(ClawLanguage claw) {
    super(claw);
    _claw = claw;
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

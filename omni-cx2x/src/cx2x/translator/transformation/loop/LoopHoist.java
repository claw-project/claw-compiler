/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;

/**
 * A LoopHoist transformation is an independent transformation. It
 * performs loop fusion in a given structured block as well as do start
 * statement hoisting.
 *
 * @author clementval
 */
public class LoopHoist extends Transformation<LoopHoist> {
  /**
   * Constructs a new LoopHoist triggered from a specific directive.
   * @param directive The directive that triggered the loop hoist
   *                  transformation.
   */
  public LoopHoist(ClawLanguage directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(LoopHoist other) {
    return false;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        LoopHoist other) throws Exception
  {

  }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.xcodeml.transformation.BlockTransformation;

/**
 * A BlockTransformation is an extension of the standard Transformation that is
 * defined by a start directive and an additional end directive.
 * The transformation is then applied on the structured block between the two
 * directive.
 *
 * @author clementval
 */
public abstract class ClawBlockTransformation extends BlockTransformation {

  protected ClawLanguage _clawStart, _clawEnd;

  protected ClawBlockTransformation(ClawLanguage startDirective,
                                    ClawLanguage endDirective)
  {
    super(startDirective, endDirective);
    _clawStart = startDirective;
    _clawEnd = endDirective;
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.translator.language.ClawLanguage;

/**
 * A BlockTransformation is an extension of the standard Transformation that is
 * defined by a start directive and an additional end directive.
 * The transformation is then applied on the structured block between the two
 * directive.
 *
 * @author clementval
 */
public abstract class BlockTransformation<T> extends Transformation<T> {
  protected ClawLanguage _endDirective = null;

  /**
   * BlockTransformation ctor.
   * @param startDirective The directive that triggered the transformation.
   * @param endDirective   The end directive that close the structured block.
   */
  public BlockTransformation(ClawLanguage startDirective,
                             ClawLanguage endDirective)
  {
    super(startDirective);
    _endDirective = endDirective;
  }

  /**
   * Get the end directive that triggered the transformation.
   * @return The analyzed directive as a language object.
   */
  public ClawLanguage getEndDirective(){
    return _endDirective;
  }
}

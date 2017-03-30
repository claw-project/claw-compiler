/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.xcodeml.transformation.Transformation;

/**
 * A Transformation is an object capable of analyzing a possible code
 * transformation to be applied and the steps to apply it to the intermediate
 * representation. Normally, only derived classes of Transformation should be
 * applied as the base class does not implement the core methods.
 *
 * @author clementval
 */

public abstract class ClawTransformation extends Transformation {

  protected final ClawLanguage _claw;

  public ClawTransformation(ClawLanguage directive) {
    super(directive);
    _claw = directive;
  }

  /**
   * Get the language information object.
   * @return ClawLanguage with information gathered at parsing time.
   */
  public ClawLanguage getLanguageInfo(){
    return _claw;
  }

}

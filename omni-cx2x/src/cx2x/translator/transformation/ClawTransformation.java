/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation;

import cx2x.translator.language.ClawPragma;
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

  protected final ClawPragma _claw;

  /**
   * Default ctor for translation_unit transformation.
   */
  public ClawTransformation() {
    super();
    _claw = null;
  }

  /**
   * Ctor for directive triggered transformation.
   *
   * @param directive Directive that trigger the transformation.
   */
  public ClawTransformation(ClawPragma directive) {
    super(directive);
    _claw = directive;
  }

  /**
   * Get the language information object.
   *
   * @return ClawPragma with information gathered at parsing time.
   */
  public ClawPragma getLanguageInfo() {
    return _claw;
  }

  /**
   * Delete the associated pragma statement.
   */
  protected void removePragma() {
    if(_claw != null && _claw.getPragma() != null) {
      _claw.getPragma().delete();
    }
  }
}

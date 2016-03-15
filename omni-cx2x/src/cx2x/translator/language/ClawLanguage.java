/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

/**
 * ClawLanguage class represent an analyzed pragma statement.
 *
 * @author clementval
 */
public class ClawLanguage {

  private ClawDirective _directive;
  private boolean _valid;

  /**
   * Constructs an empty ClawLanguage section.
   */
  public ClawLanguage(){
    _directive = null;
    _valid = false;
  }

  /**
   * Define the current language section as valid.
   */
  public void setValid(){
    _valid = true;
  }

  /**
   * Check whether the current language section is valid.
   * @return True if the language is valid.
   */
  public boolean isValid(){
    return _valid;
  }

  /**
   * Define the current directive of the language section.
   * @param directive A value of the ClawDirective enumeration.
   */
  public void setDirective(ClawDirective directive){
    _directive = directive;
  }

  /**
   * Get the current directive of the language section.
   * @return Value of the current directive.
   */
  public ClawDirective getDirective(){
    return _directive;
  }




}

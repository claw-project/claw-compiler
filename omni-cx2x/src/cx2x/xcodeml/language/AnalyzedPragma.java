/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.language;

import cx2x.xcodeml.xelement.Xpragma;
import exc.object.Xobject;

/**
 * Base class for any analyzed pragma. This object is then passed to a
 * transformation. Base implementation only stores the raw Xpragma object.
 *
 * @author clementval
 */
public class AnalyzedPragma {
  protected Xpragma _pragma;
  protected Xobject _pragmaXobject;
  private boolean _isEndPragma;

  /**
   * Default ctor.
   */
  public AnalyzedPragma(){
    _isEndPragma = false;
  }

  /**
   * Contructs an AnalyzedPragma object with a Xpragma object attached.
   * @param rawPragma Pragma object to be attached.
   */
  public AnalyzedPragma(Xpragma rawPragma){
    _pragma = rawPragma;
    _isEndPragma = false;
  }

  /**
   * Contructs an AnalyzedPragma object with a Xobject object attached.
   * @param rawPragma Pragma object to be attached.
   */
  public AnalyzedPragma(Xobject rawPragma){
    _pragmaXobject = rawPragma;
    _isEndPragma = false;
  }

  /**
   * Get the attached pragma object.
   * @return Attached pragma object.
   */
  public Xpragma getPragma(){
    return _pragma;
  }

  /**
   * Get the attached pargma object.
   * @return Attached pragma object.
   */
  public Xobject getPragmaXobject() {
    return _pragmaXobject;
  }

  /**
   * Attach a pragma object.
   * @param rawPragma Pragma object to be attached.
   */
  public void setPragma(Xpragma rawPragma){
    _pragma = rawPragma;
  }

  /**
   * Attach a pargma object.
   * @param rawPragma Pragma object to be attached.
   */
  public void setPragma(Xobject rawPragma) {
    _pragmaXobject = rawPragma;
  }

  /**
   * Check whether the pragma is an end block pragma.
   * @return True if the pragma ends a block. False otherwise.
   */
  public boolean isEndPragma(){
    return _isEndPragma;
  }

  /**
   * Set valu to the endPragma flag.
   */
  public void setEndPragma(){
    _isEndPragma = true;
  }

}

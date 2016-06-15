/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.Xnode;

/**
 * Base class for any analyzed pragma. This object is then passed to a
 * transformation. Base implementation only stores the raw Xpragma object.
 *
 * @author clementval
 */
public class AnalyzedPragma {
  protected Xnode _pragma;
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
  public AnalyzedPragma(Xnode rawPragma){
    _pragma = rawPragma;
    _isEndPragma = false;
  }

  /**
   * Get the attached pragma object.
   * @return Attached pragma object.
   */
  public Xnode getPragma(){
    return _pragma;
  }

  /**
   * Attach a pragma object.
   * @param rawPragma Pragma object to be attached.
   */
  public void setPragma(Xnode rawPragma){
    _pragma = rawPragma;
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

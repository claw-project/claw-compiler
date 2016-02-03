/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.exception.*;

/**
 * A Transformation is an object capable of analyzing a possible code
 * transformation to be applied and the steps to apply it to the intermediate
 * representation. Normally, only derived classes of Transformation should be
 * applied as the base class does not implement the core methods.
 *
 * @author clementval
 */

public abstract class Transformation<T> {
  protected boolean _transformed = false;
  protected Xpragma _pragma = null;
  protected int _startLine = 0;

  /**
   * Transformation ctor.
   * @param pragma The pragma that triggered the transformation.
   */
  public Transformation(Xpragma pragma){
    _pragma = pragma;
    if(_pragma != null){
      _startLine = _pragma.getLine();
    }
  }

  /**
   * Analyze the possibility to apply the transformation. Gather information to
   * be able to apply the transformation in when calling #transform.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if analysis succeeded. False otherwise.
   */
  public abstract boolean analyze(XcodeProgram xcodeml, Transformer transformer);

  /**
   * Check whether the current transformation can be transformed together with
   * the given transformation. Useful only for dependent transformation.
   * @see DependentTransformationGroup
   * @param other The other transformation part of the dependent transformation.
   * @return True if the two transformation can be transform together. False
   * otherwise.
   */
  public abstract boolean canBeTransformedWith(T other);

  /**
   * Apply the actual transformation.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @param other       Only for dependent transformation. The other
   *                    transformation part of the transformation.
   * @throws IllegalTransformationException
   */
  public abstract void transform(XcodeProgram xcodeml, Transformer transformer,
                                 T other) throws Exception;


  /**
   * Get the pragma that triggered the transformation.
   * @return A Xpragma element.
   */
  public Xpragma getPragma(){
    return _pragma;
  }

  /**
   * Get the line number where the pragma was found.
   * @return Line number of the pragma.
   */
  public int getStartLine(){
    return _startLine;
  }

  /**
   * Set the start line of the transformation. Normally, this is the line where
   * the pragma was found.
   * @param lineno  An positive integer value representing the line number.
   */
  public void setStartLine(int lineno){
    _startLine = lineno;
  }

  /**
   * Get the information whether the transformation has been applied or not.
   * @return True is the transformation has been applied. False otherwise.
   */
  public boolean isTransformed(){
    return _transformed;
  }

  /**
   * Set the transformation as transformed.
   */
  public void transformed(){
    _transformed = true;
  }
}

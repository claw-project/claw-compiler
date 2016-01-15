/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XexprStatement represents the FpragmaStatement (6.2) element in XcodeML
 * intermediate representation.
 *
 * Elements: exprModel
 *
 * @author clementval
 */

public class XexprStatement extends XbaseElement {

  public XexprStatement(Element exprStatementElement){
    super(exprStatementElement);
  }
}

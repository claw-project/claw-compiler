/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XlowerBound represents the lowerBound (8.12) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - Required: exprModel (XexprModel) TODO
 *
 * @author clementval
 */

public class XlowerBound extends Xbound {

  public XlowerBound(Element baseElement){
    super(baseElement);
  }

}

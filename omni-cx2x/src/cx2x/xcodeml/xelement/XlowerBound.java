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
 * - Required: exprModel (XexprModel) defined in Xbound class.
 *
 * @author clementval
 */

public class XlowerBound extends Xbound implements Xclonable<XlowerBound> {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XlowerBound(Element baseElement){
    super(baseElement);
  }

  @Override
  public XlowerBound cloneObject() {
    Element clone = (Element)cloneNode();
    return new XlowerBound(clone);
  }
}

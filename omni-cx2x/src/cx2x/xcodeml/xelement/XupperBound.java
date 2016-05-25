/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XupperBound represents the upperBound (8.13) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - Required: exprModel (XexprModel) defined in Xbound class.
 *
 * @author clementval 
 */

public class XupperBound extends Xbound implements Xclonable<XupperBound> {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XupperBound(Element baseElement){
    super(baseElement);
  }

  @Override
  public XupperBound cloneObject() {
    Element clone = (Element)cloneNode();
    return new XupperBound(clone);
  }
}

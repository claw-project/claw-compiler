/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XstructConstructor represents the FstructConstructor (7.3.1) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - exprModel* (XexprModel) TODO
 *
 * Attributes:
 * - Optional:
 *   - type (text)
 *
 * @author clementval
 */

public class XstructConstructor extends XbaseElement {

  private String _type = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XstructConstructor(Element baseElement){
    super(baseElement);
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
  }

}

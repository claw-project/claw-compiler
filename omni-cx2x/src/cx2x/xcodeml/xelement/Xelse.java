/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xelse represents the else (6.29) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - body (Xbody)
 *
 * @author clementval
 */

public class Xelse extends XbaseElement {
  private Xbody _body = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xelse(Element baseElement){
    super(baseElement);
    _body = XelementHelper.findBody(this, false);
  }

  /**
   * Get the else statement's body.
   * @return A Xbody object for the else statement.
   */
  public Xbody getBody(){
    return _body;
  }

}

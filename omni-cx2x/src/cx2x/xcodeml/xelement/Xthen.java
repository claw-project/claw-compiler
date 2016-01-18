/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xthen represents the then (6.28) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - body (Xbody)
 *
 * @author clementval
 */

public class Xthen extends XbaseElement {
  private Xbody _body = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xthen(Element baseElement){
    super(baseElement);
    _body = XelementHelper.findBody(this, false);
  }

  /**
   * Get the then statement's body.
   * @return A Xbody object for the then statement.
   */
  public Xbody getBody(){
    return _body;
  }

}

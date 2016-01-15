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

  public Xthen(Element baseElement){
    super(baseElement);
    _body = XelementHelper.findBody(this, false);
  }

  public Xbody getBody(){
    return _body;
  }

}

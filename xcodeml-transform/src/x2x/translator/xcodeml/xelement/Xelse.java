package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xelse represents the else (6.29) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - body (Xbody)
 */

public class Xelse extends XbaseElement {
  private Xbody _body = null;

  public Xelse(Element baseElement){
    super(baseElement);
    _body = XelementHelper.findBody(this);
  }

  public Xbody getBody(){
    return _body;
  }

}

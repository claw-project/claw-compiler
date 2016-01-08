package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;


/**
 * The Xthen represents the then (6.28) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - body (Xbody)
 */

public class Xthen extends XbaseElement {
  private Xbody _body = null;

  public Xthen(Element baseElement){
    super(baseElement);
    Element bodyElement = XelementHelper.getBody(baseElement);
    if(bodyElement != null){
      _body = new Xbody(bodyElement);
    }
  }

  public Xbody getBody(){
    return _body;
  }

}

package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Xdecl is the base class for element in the XdeclTable (XvarDecl, TODO)
 */

public class Xdecl {
  protected Element _element;

  public Xdecl(Element element){
    _element = element;
  }

  public Node clone(){
    return _element.cloneNode(true);
  }
}

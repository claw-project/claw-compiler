package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XbaseElement represents an element in XcodeML intermediate
 * representation.
 */

public class XbaseElement {
  protected Element baseElement = null;

  public XbaseElement(Element element){
    baseElement = element;
  }

  public Element getBaseElement(){
    return baseElement;
  }

  public Node clone(){
    if(baseElement != null){
      return baseElement.cloneNode(true);
    }
    return null;
  }

  public String getData(){
    return baseElement.getTextContent();
  }

  public void delete(){
    XelementHelper.delete(baseElement);
  }
}

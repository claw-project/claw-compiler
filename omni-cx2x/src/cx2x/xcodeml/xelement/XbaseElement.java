/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XbaseElement represents an element in XcodeML intermediate
 * representation.
 *
 * @author
 */

public class XbaseElement {
  protected Element baseElement = null;

  public XbaseElement(Element element){
    baseElement = element;
  }

  public Element getBaseElement(){
    return baseElement;
  }

  protected Node clone(){
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
    baseElement = null;
  }
}

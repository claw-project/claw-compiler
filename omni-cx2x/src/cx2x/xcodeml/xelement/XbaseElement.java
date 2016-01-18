/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XbaseElement represents an element in XcodeML intermediate
 * representation. This class stores the root element of any Xelement.
 *
 * @author clementval
 */

public class XbaseElement {
  protected Element baseElement = null;

  /**
   * XbaseElement standard ctor. Base element is stored in this class.
   */
  public XbaseElement(Element element){
    baseElement = element;
  }

  /**
   * Get the root element of this XbaseElement
   * @return The root element
   */
  public Element getBaseElement(){
    return baseElement;
  }

  /**
   * Create an identical copy of the element and its children.
   * @return A node representing the root element of the clone.
   */
  protected Node clone(){
    if(baseElement != null){
      return baseElement.cloneNode(true);
    }
    return null;
  }

  /**
   * Get the text content of the stored root element.
   * @return A string value representing the content of the element.
   */
  public String getData(){
    return baseElement.getTextContent();
  }

  /**
   * Delete the stored root element and all its children.
   */
  public void delete(){
    XelementHelper.delete(baseElement);
    baseElement = null;
  }
}

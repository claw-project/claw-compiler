/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import cx2x.xcodeml.helper.*;

/**
 * The XbaseElement represents an element in XcodeML intermediate
 * representation. This class stores the root element of any Xelement.
 *
 * @author clementval
 */

public class XbaseElement {
  // TODO code review private final Element baseElement ... to avoid null checking
  protected Element baseElement = null;

  /**
   * XbaseElement standard ctor. Base element is stored in this class.
   * @param element Root element to create the XbaseElement.
   */
  public XbaseElement(Element element){
    if(element == null){
      throw new IllegalArgumentException();
    }
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
  public Node cloneNode(){
    if(baseElement != null){
      return baseElement.cloneNode(true);
    }
    return null;
  }

  /**
   * Get the text content of the stored root element.
   * @return A string value representing the content of the element.
   */
  public String getValue(){
    return baseElement.getTextContent();
  }

  /**
   * Set the element value.
   * @param value The element value.
   */
  public void setValue(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
    }
  }

  /**
   * Delete the stored root element and all its children.
   */
  public void delete(){
    XelementHelper.delete(baseElement);
    baseElement = null;
  }

  /**
   * Append an element ot the children of this element.
   * @param element The element to append.
   * @param clone   If true, the element is cloned before being appened. If
   *                false, the element is directly appened.
   */
  public void appendToChildren(XbaseElement element, boolean clone){
    if(baseElement != null && element != null){
      if(clone){
        baseElement.appendChild(element.cloneNode());
      } else {
        baseElement.appendChild(element.getBaseElement());
      }
    }
  }
}

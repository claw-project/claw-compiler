/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.ArrayList;
import java.util.List;

import cx2x.xcodeml.helper.*;


/**
 * The XarrayRef represents the FarrayRef (7.4.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * ( varRef, (arrayIndex | indexRange | FarrayConstructor | FarrayRef)* )
 * - Required:
 *   - varRef
 * - Optional:
 *   - arrayIndex (XarrayIndex)
 *   - indexRange (XindexRange)
 *   - FarrayConstructor TODO
 *   - FarrayRef (XarrayRef)
 * Attributes:
 * - Optional: type (text)
 *
 * @author clementval
 */

public class XarrayRef extends XbaseElement {

  private String _type = null;
  private XvarRef _varRef = null;
  private List<XbaseElement> _innerElement = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XarrayRef(Element baseElement){
    super(baseElement);
    _innerElement = new ArrayList<>();
    readElementInformation();
  }

  /**
   * Get a list of all inner elements.
   * @return A list of XbaseElement.
   */
  public List<XbaseElement> getInnerElements(){
    return _innerElement;
  }

  /**
   *  Read inner element information.
   */
  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);

    // Find Var element
    _varRef = XelementHelper.findVarRef(this, false);

    // Read all inner elements
    Node crtNode = baseElement.getFirstChild();
    while(crtNode != null){
      if (crtNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element)crtNode;
        switch (element.getTagName()){
          case XelementName.ARRAY_INDEX:
            XarrayIndex arrayIndex = new XarrayIndex(element);
            _innerElement.add(arrayIndex);
            break;
          case XelementName.INDEX_RANGE:
            XindexRange indexRange = new XindexRange(element);
            _innerElement.add(indexRange);
            break;
          case XelementName.F_ARRAY_REF:
            XarrayRef arrayRef = new XarrayRef(element);
            _innerElement.add(arrayRef);
            break;
          // TODO read FarrayConstructor
        }
      }
      crtNode = crtNode.getNextSibling();
    }
  }

  /**
   * Get the inner varRef object.
   * @return The inner varRef object.
   */
  public XvarRef getVarRef(){
    return _varRef;
  }

  /**
   * Get the arrayRef type value.
   * @return The type value as String. Null if the type value is not set.
   */
  public String getType(){
    return _type;
  }


  /**
   * Set the type attribute of the arrayRef element.
   * @param type Type of the arrayRef.
   */
  public void setType(String type){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, type);
    }
  }

  /**
   * Append a XbaseElement as the last children of XarrayRef.
   * @param element The element to append.
   */
  public void append(XbaseElement element){
    append(element, false);
  }

  /**
   * Append a XbaseElement as the last children of XarrayRef.
   * @param element       The element to append.
   * @param cloneElement  If true, the element is cloned before being added. If
   *                      false, the element is directly added.
   */
  public void append(XbaseElement element, boolean cloneElement){
    // TODO if varRef, replace the element.

    if(cloneElement){
      Node clone = element.cloneNode();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    if(element instanceof XvarRef){
      _varRef = (XvarRef)element;
    }
  }

}

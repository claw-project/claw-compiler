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
 * - Required:
 *   - varRef
 * - Optional:
 *   - arrayIndex TODO
 *   - indexRange TODO
 *   - FarrayConstructor TODO
 *   - FarrayRef TODO (XarrayRef)
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

    // Read potential arrayIndex
    NodeList nodeList = baseElement.
      getElementsByTagName(XelementName.ARRAY_INDEX);

    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XarrayIndex arrayIndex = new XarrayIndex(el);
        _innerElement.add(arrayIndex);
      }
    }

    // TODO read indexRange

    // TODO read FarrayConstructor

    // TODO read FarrayRef

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

  /**
   * Create an empty arrayRef element in the given program
   * param type attribute of the element. If null, no attribute is set.
   * @param xcodeml XcodeML program in which the new object is created.
   * @param type    Optional type of the new arrayRef object.
   * @return A new empty XarrayRef object with optional type.
   */
  public static XarrayRef createEmpty(XcodeProg xcodeml, String type){
    Element arrayRef = xcodeml.getDocument().
      createElement(XelementName.F_ARRAY_REF);
    if(type != null){
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    return new XarrayRef(arrayRef);
  }
}

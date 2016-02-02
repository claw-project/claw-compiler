/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XvarRef represents the varRef (7.4.6) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required: one of the followings
 *   - Var (Xvar)
 *   - FmemberRef TODO
 *   - FarrayRef (XarrayRef)
 *   - FcharacterRef TODO
 *   - FcoArrayRef TODO (not priority)
 * Attributes:
 * - Optional: type (text) TODO
 *
 * @author clementval
 */

public class XvarRef extends XbaseElement implements Xclonable<XvarRef> {
  private String _type = null;
  private XbaseElement _innerElement = null;


  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XvarRef(Element baseElement){
    super(baseElement);
    _innerElement = XelementHelper.findVarRefInnerElement(this);
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
  }


  /**
   * Check whether the varRef has a type.
   * @return True if a type is associated. False otherwise.
   */
  public boolean hasType(){
    return _type != null;
  }

  /**
   * Get the associated type.
   * @return Type value.
   */
  public String getType(){
    return _type;
  }

  /**
   * Set the associated type.
   * @param type The type.
   */
  public void setType(String type){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, type);
    }
  }

  /**
   * Check if the inner element is a var element.
   * @return True if the inner element is a var.
   */
  public boolean isVar(){
    return _innerElement instanceof Xvar;
  }

  /**
   * Get the var element.
   * @return Var element. Null if the inner element is not a var element.
   */
  public Xvar getVar(){
    if(isVar()){
      return (Xvar)_innerElement;
    }
    return null;
  }

  /**
   * Check if the inner element is a arrayRef element.
   * @return True if the inner element is a arrayRef.
   */
  public boolean isArrayRef(){
    return _innerElement instanceof XarrayRef;
  }

  /**
   * Get the arrayRef element.
   * @return arrayRef element. Null if the inner element is not a arrayRef
   * element.
   */
  public XarrayRef getArrayRef(){
    if(isArrayRef()){
      return (XarrayRef)_innerElement;
    }
    return null;
  }

  /**
   * Insert an element as the last child of the XvarRef.
   * @param element      The element to be inserted.
   * @param cloneElement If true, the element is cloned and then inserted as the
   *                     last child. The clone is inserted.
   */
  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.cloneNode();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    // TODO set the correct variable once they are there
  }

  /**
   * Clone this object.
   * @return A cloned copy of the XvarRef object.
   */
  public XvarRef cloneObject(){
    Element clone = (Element)cloneNode();
    return new XvarRef(clone);
  }
}

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
  private XbaseElement _innerElement = null;


  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XvarRef(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read the inner element information.
   */
  private void readElementInformation(){
    _innerElement = XelementHelper.findVar(this, false);
  }

  /**
   * Check if the inner element is a var element
   * @return True if the inner element is a var
   */
  public boolean isVar(){
    return _innerElement instanceof Xvar;
  }

  public Xvar getVar(){
    if(isVar()){
      return (Xvar)_innerElement;
    }
    return null;
  }


  /**
   * Create an empty varRef element in the given program
   * @param type Attribute of the element. If null, no attribute is set
   * @return A new varRef element with no children.
   */
  public static XvarRef createEmpty(XcodeProg xcodeml, String type){
    Element arrayRef = xcodeml.getDocument().
      createElement(XelementName.VAR_REF);
    if(type != null){
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    return new XvarRef(arrayRef);
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

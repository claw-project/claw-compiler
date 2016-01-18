/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xconstant represents the base of constants elements (7.1.1) element in
 * XcodeML intermediate representation. It includes (FintConstant,
 * FrealConstant, FcomplexConstant, FcharacterConstant, FlogicalConstant)
 *
 * Elements: contains value
 * Attributes:
 * - Optional: type (text), kind (text)
 *
 * @author clementval
 */

public class Xconstant extends XbaseElement {
  private String _value = null;
  private String _type = null;
  private String _kind = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xconstant(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _value = baseElement.getTextContent();
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
    _kind = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
  }

  /**
   * Check whether the element has kind attribute.
   * @return True if the element has kind attribute. False otherwise.
   */
  public boolean hasKind(){
    return _kind != null;
  }

  /**
   * Set the kind attribute.
   * @param value Kind value.
   */
  public void setKind(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_KIND, value);
      _kind = value;
    }
  }

  /**
   * Check whether the element has type attribute.
   * @return True if the element has type attribute. False otherwise.
   */
  public boolean hasType(){
    return _type != null;
  }

  /**
   * Set the type attribute.
   * @param value The type value.
   */
  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  /**
   * Get the constant value.
   * @return The constant value as String.
   */
  public String getValue(){
    return _value;
  }

  /**
   * Set the constant value.
   * @param value The constant value.
   */
  public void setValue(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _value = value;
    }
  }
}

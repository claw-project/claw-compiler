/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The Xname represents the name (8.3) element in XcodeML intermediate
 * representation.
 * Elements: the base element can contains text data
 * Attributes:
 * - Requited: type (text)
 *
 * @author clementval
 */

public class Xname extends XbaseElement {
  private String _type = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xname(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Get the type attribute value.
   * @param value Type value.
   */
  public void setType(String value){
    if(baseElement != null && value != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
  }

  /**
   * Get the type attribute value.
   * @return Type value.
   */
  public String getType(){
    return _type;
  }

  /**
   * Check whether a given Xname object is identical with the current one.
   * @param other The other object to compare with.
   * @return True if the two objects are identical. False otherwise.
   */
  public boolean isIdentical(Xname other){
    if(other == null){
      return false;
    }
    return getValue().equals(other.getValue());
  }

  /**
   * Check whether a given name is identical with the current one.
   * @param name The name to compare with.
   * @return True if the name is identical with the name of this Xname.
   * False otherwise.
   */
  public boolean isIdentical(String name){
    return getValue().equals(name);
  }

}

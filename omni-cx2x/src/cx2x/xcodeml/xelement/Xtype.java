/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * Xtype is the base class for element in the XtypeTable (XbasicType, XfctType)
 *
 * @author clementval
 */

public class Xtype extends XbaseElement {
  protected String _type;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xtype(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
  }

  /**
   * Set type value.
   * @param value New type value.
   */
  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  /**
   * Get type value.
   * @return Type value.
   */
  public String getType(){
    return _type;
  }
}

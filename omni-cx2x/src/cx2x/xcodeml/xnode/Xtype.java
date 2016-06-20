/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

/**
 * Xtype is the base class for element in the XtypeTable
 * (XbasicType, XfunctionType)
 *
 * @author clementval
 */

public class Xtype extends Xnode {
  private String _type;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root of the element.
   */
  public Xtype(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _type = getAttribute(Xattr.TYPE);
  }

  /**
   * Set type value.
   * @param value New type value.
   */
  public void setType(String value){
    if(_baseElement != null){
      _baseElement.setAttribute(Xname.ATTR_TYPE, value);
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

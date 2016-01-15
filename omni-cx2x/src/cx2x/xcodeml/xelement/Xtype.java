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

  public Xtype(Element typeElement){
    super(typeElement);
    readElementInformation();
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
  }

  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public String getType(){
    return _type;
  }
}

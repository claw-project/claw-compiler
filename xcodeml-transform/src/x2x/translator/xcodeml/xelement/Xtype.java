package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Xtype is the base class for element in the XtypeTable (XbasicType, XfctType)
 */

public class Xtype {
  protected Element _element;
  protected String _type;

  public Xtype(Element element){
    _element = element;
    readElementInformation();
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_TYPE);
  }

  public Node clone(){
    return _element.cloneNode(true);
  }

  public void setType(String value){
    if(_element != null){
      _element.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public String getType(){
    return _type;
  }
}

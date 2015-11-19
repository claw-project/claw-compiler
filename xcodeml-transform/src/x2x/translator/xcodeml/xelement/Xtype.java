package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

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

  public String getType(){
    return _type;
  }
}

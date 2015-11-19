package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/*<id type="A7fd318c079e0" sclass="flocal">
  <name>value1</name>
</id>*/
public class Xid {
  private String _type = null;
  private String _sclass = null;
  private String _name = null;

  private Element _idElement;

  public Xid(Element idElement){
    _idElement = idElement;
    readElementInformation();
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(_idElement,
      XelementName.ATTR_TYPE);
    _sclass = XelementHelper.getAttributeValue(_idElement,
      XelementName.ATTR_SCLASS);
    Element nameElement = XelementHelper.findFirstElement(_idElement,
      XelementName.NAME);
    _name = nameElement.getTextContent();
  }

  public String getName(){
    return _name;
  }

  public Node clone(){
    return _idElement.cloneNode(true);
  }

}

package x2x.translator.xcodeml;

import org.w3c.dom.Element;

/*<id type="A7fd318c079e0" sclass="flocal">
  <name>value1</name>
</id>*/
public class CLAWid {
  private String _type = null;
  private String _sclass = null;
  private String _name = null;

  private Element _idElement;

  public CLAWid(Element idElement){
    _idElement = idElement;
    readElementInformation();
  }

  private void readElementInformation(){
    _type = CLAWelementHelper.getAttributeValue(_idElement,
      XelementName.ATTR_TYPE);
    _sclass = CLAWelementHelper.getAttributeValue(_idElement,
      XelementName.ATTR_SCLASS);
    Element nameElement = CLAWelementHelper.findFirstElement(_idElement,
      XelementName.NAME);
    _name = nameElement.getTextContent();
  }

  public String getName(){
    return _name;
  }

}

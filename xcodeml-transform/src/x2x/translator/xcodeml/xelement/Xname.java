package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The Xname represents the name (8.3) element in XcodeML intermediate
 * representation.
 * Elements: the base element can contains text data
 * Attributes:
 * - Requited: type (text)
 */

public class Xname extends XbaseElement {
  private String _nameValue = null;
  private String _nameType = null;

  public Xname(Element nameElement){
    super(nameElement);
    readElementInformation();
  }

  public void setName(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _nameValue = value;
    }
  }

  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _nameType = value;
    }
  }

  private void readElementInformation(){
    _nameType = XelementHelper.getAttributeValue(baseElement
      , XelementName.ATTR_TYPE);
    _nameValue = baseElement.getTextContent();
  }

  public String getValue(){
    return _nameValue;
  }

  public String getType(){
    return _nameType;
  }

  public boolean isIdentical(Xname other){
    return isIdentical(other.getValue(), other.getType());
  }

  public boolean isIdentical(String name, String type){
    return _nameValue.equals(name) && _nameType.equals(type);
  }

}

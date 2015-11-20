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

public class Xname {
  private Element _nameElement = null;
  private String _nameValue = null;
  private String _nameType = null;

  public Xname(Element name){
    _nameElement = name;
    readElementInformation();
  }

  public void setName(String value){
    if(_nameElement != null){
      _nameElement.setTextContent(value);
      _nameValue = value;
    }
  }

  private void readElementInformation(){
    _nameType = XelementHelper.getAttributeValue(_nameElement
      , XelementName.ATTR_TYPE);
    _nameValue = _nameElement.getTextContent();
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

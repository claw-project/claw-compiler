package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/*
 * Example of XcodeML representation
 *
 * <name type="F7fdd2b600350">clawloop</name>
 */

public class Xname {
  private Element _nameElement = null;
  private String _nameValue = null;
  private String _nameType = null;

  // TODO move constant to a single file
  private static final String TYPE = "type";

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
    _nameType = XelementHelper.getAttributeValue(_nameElement, TYPE);
    _nameValue = _nameElement.getTextContent();
  }

  public String getName(){
    return _nameValue;
  }

  public String getType(){
    return _nameType;
  }

  public boolean isIdentical(Xname other){
    return isIdentical(other.getName(), other.getType());
  }

  public boolean isIdentical(String name, String type){
    return _nameValue.equals(name) && _nameType.equals(type);
  }

}

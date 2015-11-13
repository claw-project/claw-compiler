package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/*
 * Example of XcodeML representation
 *
 * <name type="F7fdd2b600350">clawloop</name>
 */

public class CLAWname {
  private Element _nameElement = null;
  private String _nameValue = null;
  private String _nameType = null;

  // TODO move constant to a single file
  private static final String TYPE = "type";

  public CLAWname(Element name){
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
    _nameType = CLAWelementHelper.getAttributeValue(_nameElement, TYPE);
    _nameValue = _nameElement.getTextContent();
  }

  public String getName(){
    return _nameValue;
  }

  public String getType(){
    return _nameType;
  }

  public boolean isIdentical(CLAWname other){
    return isIdentical(other.getName(), other.getType());
  }

  public boolean isIdentical(String name, String type){
    return _nameValue.equals(name) && _nameType.equals(type);
  }

}

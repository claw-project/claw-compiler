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
  private String _type = null;

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
      _type = value;
    }
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(baseElement
      , XelementName.ATTR_TYPE);
    _nameValue = baseElement.getTextContent();
  }

  public String getValue(){
    return _nameValue;
  }

  public String getType(){
    return _type;
  }

  public boolean isIdentical(Xname other){
    return isIdentical(other.getValue(), other.getType());
  }

  public boolean isIdentical(String name, String type){
    return _nameValue.equals(name) && _type.equals(type);
  }

  /**
   * Create a name element with value and type in the given program
   * @param name  Value of the element
   * @param type  Type of the name element
   */
  public static Xname createEmpty(XcodeProg xcodeml, String value,
    String type)
  {
    Element nameElement = xcodeml.getDocument().
      createElement(XelementName.NAME);
    Xname name = new Xname(nameElement);
    name.setName(value);
    name.setType(type);
    return name;
  }

}

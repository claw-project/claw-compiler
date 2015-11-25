package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The Xconstant represents the base of constants elements (7.1.1) element in
 * XcodeML intermediate representation. It includes (FintConstant,
 * FrealConstant, FcomplexConstant, FcharacterConstant, FlogicalConstant)
 *
 * Elements: contains value
 * Attributes:
 * - Optional: type (text), kind (text)
 */

public class Xconstant {
  private Element _element = null;
  private String _value = null;
  private String _type = null;
  private String _kind = null;

  public Xconstant(Element element){
    _element = element;
    readElementInformation();
  }

  private void readElementInformation(){
    _value = _element.getTextContent();
    _type = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_TYPE);
    _kind = XelementHelper.getAttributeValue(_element,
      XelementName.ATTR_TYPE);
  }

  public boolean hasKind(){
    return _kind != null;
  }

  public void setKind(String value){
    if(_element != null){
      _element.setAttribute(XelementName.ATTR_KIND, value);
      _kind = value;
    }
  }

  public boolean hasType(){
    return _type != null;
  }

  public void setType(String value){
    if(_element != null){
      _element.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public String getValue(){
    return _value;
  }

  public void setValue(String value){
    if(_element != null){
      _element.setTextContent(value);
      _value = value;
    }
  }

  public Node clone(){
    if(_element != null){
      return _element.cloneNode(true);
    }
    return null;
  }
}

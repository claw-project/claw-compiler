package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The XtypeTable represents the typeTable (7.4.1) element in XcodeML intermediate
 * representation.
 *
 * Elements: the base element can contains text data
 * Attributes:
 * - Required: scope (text: local, global, param) // TODO move to enum
 * - Optional: type (text)
 */

public class Xvar {
  private Element _varElement = null;
  private String _identity = null;
  private String _type = null;
  private String _scope = null;

  public Xvar(Element var){
    _varElement = var;
    readElementInformation();
  }

  public Node clone(){
    return _varElement.cloneNode(true);
  }

  public void setValue(String value){
    if(_varElement != null){
      _varElement.setTextContent(value);
      _identity = value;
    }
  }

  public void setType(String value){
    if(_varElement != null){
      _varElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public void setScope(String value){
    if(_varElement != null){
      _varElement.setAttribute(XelementName.ATTR_SCOPE, value);
      _scope = value;
    }
  }

  public String getValue(){
    return _identity;
  }

  public String getScope(){
    return _scope;
  }

  public String getType(){
    return _type;
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(_varElement, XelementName.ATTR_TYPE);
    _scope = XelementHelper.getAttributeValue(_varElement, XelementName.ATTR_SCOPE);
    _identity = _varElement.getTextContent();
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    Xvar other = (Xvar)ob;

    if(!_identity.toLowerCase().equals(other.getValue().toLowerCase())){
      return false;
    }

    if(!_type.equals(other.getType())){
      return false;
    }

    if(!_scope.equals(other.getScope())){
      return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return _identity.hashCode() ^ _scope.hashCode() ^ _type.hashCode();
  }
}

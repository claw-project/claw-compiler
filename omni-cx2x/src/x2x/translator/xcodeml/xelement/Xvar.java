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
 * - Required: scope (Xscope: LOCAL, GLOBAL, PARAM)
 * - Optional: type (text)
 */

public class Xvar extends XbaseElement implements Xclonable<Xvar> {
  private String _identity = null;
  private String _type = null;
  private Xscope _scope = null;

  public Xvar(Element varElement){
    super(varElement);
    readElementInformation();
  }

  public void setValue(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _identity = value;
    }
  }

  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public void setScope(Xscope value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_SCOPE, value.toString());
      _scope = value;
    }
  }

  public String getValue(){
    return _identity;
  }

  public Xscope getScope(){
    return _scope;
  }

  public String getType(){
    return _type;
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
    String scope = XelementHelper.getAttributeValue(this, XelementName.ATTR_SCOPE);
    _scope = Xscope.fromString(scope);
    _identity = baseElement.getTextContent();
  }

  public Xvar cloneObject(){
    Node clone = clone();
    return new Xvar((Element)clone);
  }

  /**
   * Create an empty arrayRef element in the given program
   * param type attribute of the element. If null, no attribute is set
   */
  public static Xvar createEmpty(XcodeProg xcodeml, String scope){
    Element var = xcodeml.getDocument().
      createElement(XelementName.VAR);
    if(scope == null){
      // TODO trigger an error, scope is required
    } else {
      var.setAttribute(XelementName.ATTR_SCOPE, scope);
    }
    return new Xvar(var);
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

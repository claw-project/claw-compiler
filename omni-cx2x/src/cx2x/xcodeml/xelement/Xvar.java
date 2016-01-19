/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import cx2x.xcodeml.helper.*;

/**
 * The XtypeTable represents the typeTable (7.4.1) element in XcodeML intermediate
 * representation.
 *
 * Elements: the base element can contains text data
 * Attributes:
 * - Required: scope (Xscope: LOCAL, GLOBAL, PARAM)
 * - Optional: type (text)
 *
 * @author clementval
 */

public class Xvar extends XbaseElement implements Xclonable<Xvar> {
  private String _identity = null;
  private String _type = null;
  private Xscope _scope = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xvar(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Set var value.
   * @param value Var value.
   */
  public void setValue(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _identity = value;
    }
  }

  /**
   * Set var type.
   * @param value var type.
   */
  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  /**
   * Set var scope.
   * @param value var scope.
   */
  public void setScope(Xscope value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_SCOPE, value.toString());
      _scope = value;
    }
  }

  /**
   * Get var value.
   * @return var value.
   */
  public String getValue(){
    return _identity;
  }

  /**
   * Get var scope.
   * @return var scope.
   */
  public Xscope getScope(){
    return _scope;
  }

  /**
   * Get var type.
   * @return var type.
   */
  public String getType(){
    return _type;
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
    String scope = XelementHelper.getAttributeValue(this, XelementName.ATTR_SCOPE);
    _scope = Xscope.fromString(scope);
    _identity = baseElement.getTextContent();
  }

  /**
   * Clone the current object
   * @return  A new object Xvar that is the clone of the current object.
   */
  public Xvar cloneObject(){
    Node clone = cloneNode();
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

    return _scope.equals(other.getScope());

  }

  @Override
  public int hashCode() {
    return _identity.hashCode() ^ _scope.hashCode() ^ _type.hashCode();
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import cx2x.xcodeml.helper.*;

/**
 * The XtypeTable represents the typeTable (7.4.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: the base element can contains text data
 * Attributes:
 * - Required: scope (Xscope: LOCAL, GLOBAL, PARAM)
 * - Optional: type (text)
 *
 * @author clementval
 */

public class Xvar extends XbaseElement implements Xclonable<Xvar> {
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
   * Set var type.
   * @param value var type.
   */
  public void setType(String value){
    if(baseElement != null && value != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  /**
   * Set var scope.
   * @param value var scope.
   */
  public void setScope(Xscope value){
    if(baseElement != null && value != null){
      baseElement.setAttribute(XelementName.ATTR_SCOPE, value.toString());
      _scope = value;
    }
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
  }

  /**
   * Clone the current object
   * @return  A new object Xvar that is the clone of the current object.
   */
  public Xvar cloneObject(){
    Node clone = cloneNode();
    return new Xvar((Element)clone);
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    Xvar other = (Xvar)ob;

    if(!getValue().toLowerCase().equals(other.getValue().toLowerCase())){
      return false;
    }

    if(!_type.equals(other.getType())){
      return false;
    }

    return _scope.equals(other.getScope());

  }

  @Override
  public int hashCode() {
    return getValue().hashCode() ^ _scope.hashCode() ^ _type.hashCode();
  }

  /**
   * Create a new Xvar object with all the underlying elements.
   * @param type    Value for the attribute type.
   * @param value   Value of the var element.
   * @param scope   Value for the attribute scope.
   * @param xcodeml XcodeML program.
   * @return A newly constructs Xvar element with all the information loaded.
   * @throws IllegalTransformationException can be thrown while constrcuting
   * empty elements.
   */
  public static Xvar create(String type, String value, Xscope scope,
                            XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    Xvar var = XelementHelper.createEmpty(Xvar.class, xcodeml);
    var.setType(type);
    var.setValue(value);
    var.setScope(scope);
    var.readElementInformation();
    return var;
  }
}

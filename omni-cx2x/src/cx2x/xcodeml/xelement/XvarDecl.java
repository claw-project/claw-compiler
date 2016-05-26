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
 * The XvarDecl represents the varDecl (5.4) element in XcodeML intermediate
 * representation.
 *
 * Elements: (name, value?)
 * - Required:
 *   - name (Xname)
 * - Optional:
 *   - value (text)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class XvarDecl extends Xdecl {
  private Xname _name = null;
  private Xvalue _value = null;
  private boolean _hasValue = false;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XvarDecl(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read the inner element information.
   */
  private void readElementInformation(){
    _name = XelementHelper.findName(this, false);
    _value = XelementHelper.findValue(this, false);
    if(_value != null){
      _hasValue = true;
    }
  }

  /**
   * Check whether the XvarDecl has a value set.
   * @return True if there is a value set. False otherwise.
   */
  public boolean hasValue(){
    return _hasValue;
  }

  /**
   * Get the var value.
   * @return Value assigned to the var declaration.
   */
  @Override
  public String getValue(){
    if(_hasValue){
      return _value.getValue();
    }
    return null;
  }

  /**
   * Get the inner Xname element.
   * @return Xname element.
   */
  public Xname getName(){
    return _name;
  }

  /**
   * Check whether the var declaration is using a built-in type or a type
   * defined in the type table.
   * @return True if the type is built-in. False otherwise.
   */
  public boolean isBuiltInType(){
    return XelementHelper.isBuiltInType(getName().getType());
  }

  /**
   * Insert the given element as the last child of the XvarDecl.
   * @param element The element to be inserted.
   */
  public void append(XbaseElement element){
    append(element, false);
  }

  /**
   * Insert an element as the last child of the XvarDecl.
   * @param element      The element to be inserted.
   * @param cloneElement If true, the element is cloned and then inserted as the
   *                     last child. The clone is inserted.
   */
  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.cloneNode();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    if(element instanceof Xname){
      _name = (Xname)element; // TODO error if there is a name already
    }
  }

  /**
   * Create a new XvarDecl object with all the underlying elements.
   * @param nameType  Value for the attribute type of the name element.
   * @param nameValue Value of the name inner element.
   * @param xcodeml   XcodeML program.
   * @return A newly constructs XvarDecl element with all the information
   * loaded.
   * @throws IllegalTransformationException can be thrown while constrcuting
   * empty elements.
   */
  public static XvarDecl create(String nameType, String nameValue,
                                XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    XvarDecl varD = XelementHelper.createEmpty(XvarDecl.class, xcodeml);
    Xname internalName = XelementHelper.createEmpty(Xname.class, xcodeml);
    internalName.setValue(nameValue);
    internalName.setType(nameType);
    varD.appendToChildren(internalName, false);
    varD.readElementInformation();
    return varD;
  }

  @Override
  public XvarDecl cloneObject() {
    Element clone = (Element)cloneNode();
    return new XvarDecl(clone);
  }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import cx2x.xcodeml.helper.*;

/**
 * The XvarDecl represents the varDecl (5.4) element in XcodeML intermediate
 * representation.
 *
 * Elements: (name, value?)
 * - Required:
 *   - name
 * - Optional:
 *   - value
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class XvarDecl extends Xdecl {
  private Xnode _name = null;
  private Xnode _value = null;
  private boolean _hasValue = false;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement Root of the element.
   */
  public XvarDecl(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read the inner element information.
   */
  private void readElementInformation(){
    _name = find(Xcode.NAME);
    _value = find(Xcode.VALUE);
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
   * Get the inner name element.
   * @return Name element.
   */
  public Xnode getName(){
    return _name;
  }

  /**
   * Insert the given element as the last child of the XvarDecl.
   * @param element The element to be inserted.
   */
  public void append(Xnode element){
    append(element, false);
  }

  /**
   * Insert an element as the last child of the XvarDecl.
   * @param element      The element to be inserted.
   * @param cloneElement If true, the element is cloned and then inserted as the
   *                     last child. The clone is inserted.
   */
  public void append(Xnode element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.cloneNode();
      _baseElement.appendChild(clone);
    } else {
      _baseElement.appendChild(element.getElement());
    }

    if(element.Opcode() == Xcode.NAME){
      _name = element; // TODO error if there is a name already
    }
  }

}

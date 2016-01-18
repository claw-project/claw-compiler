/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xbound represents the lowerBound and upperBound (8.12, 8.13) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - exprModel TODO move to exprModel
 *
 * @author clementval
 */

public class Xbound extends XbaseElement {
  private String _value = null;
  private boolean _constant = false;
  private boolean _isVar = false;
  private Xvar _var = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xbound(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  // TODO to be removed when moved to exprModel
  public boolean isContant(){
    return _constant;
  }

  // TODO to be removed when moved to exprModel
  public boolean isVar(){
    return _isVar;
  }

  // TODO to be removed when moved to exprModel
  public String getValue(){
    return _value;
  }

  // TODO to be removed when moved to exprModel
  public String getType(){
    if(isVar() && _var != null) {
      return _var.getType();
    }
    return null;
  }

  /**
   * Read inner element information
   */
  private void readElementInformation(){
    XintConstant constant = XelementHelper.findIntConstant(this, false);
    Xvar var = XelementHelper.findVar(this, false);
    if(constant != null){
      _constant = true;
      _value = constant.getValue();
    } else if(var != null){
      _isVar = true;
      _var = var;
      _value = var.getValue();
    }
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    Xbound other = (Xbound)ob;

    if(isVar() != other.isVar()){
      return false;
    }

    if(isContant() != isContant()){
      return false;
    }

    return getValue().toLowerCase().equals(other.getValue());

  }

  @Override
  public int hashCode() {
    return _value.hashCode();
  }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The Xbound represents the lowerBound and upperBound (8.12, 8.13) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - exprModel
 *
 * @author clementval
 */

public class Xbound extends XbaseElement {
  private String _value = null;
  private boolean _constant = false;
  private boolean _isVar = false;
  private Xvar _var = null;
  private XexprModel _exprModel = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xbound(Element baseElement){
    super(baseElement);
    _exprModel = XelementHelper.findExprModel(this, false);
  }

  /**
   * Get the inner element
   * @return Inner element as exprModel
   */
  public XexprModel getExprModel(){
    return _exprModel;
  }

  public String getValue(){
    if(_exprModel.isVar()){
      return _exprModel.getVar().getValue();
    }
    if(_exprModel.isConstant()){
      return _exprModel.getConstant().getValue();
    }
    return null;
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    Xbound other = (Xbound)ob;

    if(getValue() != null && other.getValue() != null
        && getValue().toLowerCase().equals(other.getValue().toLowerCase()))
    {
      return true;
    }
    return false;
  }

  @Override
  public int hashCode() {
    return _value.hashCode();
  }

}

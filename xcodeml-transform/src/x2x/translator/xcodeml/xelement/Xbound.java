package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The Xbound represents the lowerBound and upperBound (8.12, 8.13) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - exprModel TODO move to exprModel
 */

// TODO have a derived class for upper and lower bound

public class Xbound {
  private String _value = null;
  private boolean _constant = false;
  private boolean _var = false;
  private Element _boundElement = null;

  public Xbound(Element boundElement){
    _boundElement = boundElement;
    readRangeValue();
  }

  public boolean isContant(){
    return _constant;
  }

  public boolean isVar(){
    return _var;
  }

  public String getValue(){
    return _value;
  }

  private void readRangeValue(){
    Element constant = XelementHelper
      .findFirstElement(_boundElement, XelementName.INT_CONST);
    Element var = XelementHelper
      .findFirstElement(_boundElement, XelementName.VAR);
    if(constant != null){
      _constant = true;
      _value = constant.getTextContent();
    } else if(var != null){
      _var = true;
      _value = var.getTextContent();
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

    if (!getValue().toLowerCase().equals(other.getValue())){
      return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return _value.hashCode();
  }

}

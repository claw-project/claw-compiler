/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The Xfct is the base class for the XfctCall and the XfctDef classes
 *
 * @author clementval
 */

public class Xfct extends XbaseElement {
  private Xname _fctName = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xfct(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read the inner element information.
   */
  private void readElementInformation(){
    NodeList names = baseElement.getElementsByTagName(XelementName.NAME);
    Element nameElement = (Element) names.item(0);
    _fctName = new Xname(nameElement);
  }

  /**
   * Update the function name.
   * @param value New name of the function
   */
  public void updateName(String value){
    if(_fctName != null) {
      _fctName.setName(value);
    }
  }

  /**
   * Update function type.
   * @param value New type of the function.
   */
  public void updateType(String value){
    if(_fctName != null){
      _fctName.setType(value);
    }
  }
  
  /**
   * Get the function name.
   * @return Name of the function as a String value.
   */
  public String getFctName(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getValue();
  }

  /**
   * Get the function type.
   * @return Type of the function as a String value.
   */
  public String getFctType(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getType();
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The Xlength represents the len (8.6) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( exprModel )
 * - exprModel (XexprModel)
 *
 * @author clementval
 */

public class Xlength extends XbaseElement {

  private XexprModel _exprModel = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xlength(Element baseElement){
    super(baseElement);
    _exprModel = XelementHelper.findExprModel(this, 0);
  }

  /**
   * Get the inner exprModel element.
   * @return XexprModel object contains in this object.
   */
  public XexprModel getExprModel(){
    return _exprModel;
  }
}

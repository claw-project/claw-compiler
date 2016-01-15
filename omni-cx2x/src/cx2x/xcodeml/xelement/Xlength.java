/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xlength represents the len (8.6) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel (XexprModel)
 *
 * @author clementval
 */

public class Xlength extends XbaseElement {

  private XexprModel _exprModel = null;

  public Xlength(Element element){
    super(element);
    _exprModel = XelementHelper.findExprModel(this);
  }

  public XexprModel getExprModel(){
    return _exprModel;
  }
}

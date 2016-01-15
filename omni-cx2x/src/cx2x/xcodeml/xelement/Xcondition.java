/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;


/**
 * The Xcondition represents the condition (6.27) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - exprModel (XexprModel)
 *
 * @author clementval
 */

public class Xcondition extends XbaseElement {
  private XexprModel _exprModel;

  public Xcondition(Element baseElement){
    super(baseElement);
    _exprModel = XelementHelper.findExprModel(this);
  }

  public XexprModel getExprModel(){
    return _exprModel;
  }
}

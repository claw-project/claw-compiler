/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * The XunaryExpr represents all unary expression element in XcodeML
 * intermediate representation. Those elements are:
 * logNotExpr, unaryMinusExpr, userUnaryExpr
 *
 * Elements: ( exprModel )
 * - Required:
 *   - exprModel (XexprModel)
 * Attributes:
 * - Optional: type (text)
 *
 * @author clementval
 */
public class XunaryExpr extends XbaseElement implements Xclonable<XunaryExpr> {

  private XexprModel _expr = null;
  private String _type = null;

  /**
   * Constructs new XunaryExpr by reading element information.
   * @param baseElement Root element of the binary expression.
   */
  public XunaryExpr(Element baseElement){
    super(baseElement);
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
    _expr = XelementHelper.findExprModel(this, 0);
  }

  /**
   * Get the inner expression.
   * @return XexprModel.
   */
  public XexprModel getLhsExpr(){
    return _expr;
  }


  /**
   * Get the type value.
   * @return Type value.
   */
  public String getType(){
    return _type;
  }

  /**
   * Set new type value.
   * @param type Type value.
   */
  public void setType(String type){
    _type = type;
  }

  @Override
  public XunaryExpr cloneObject() {
    Element clone = (Element)cloneNode();
    return new XunaryExpr(clone);
  }
}

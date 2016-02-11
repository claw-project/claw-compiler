/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * The XbinaryExpr represents all binary expression element in XcodeML
 * intermediate representation. Those elements are divExpr, FconcatExpr,
 * FpowerExpr, logAndExpr, logEQExpr, logEQVExpr, logGEExpr, logGTExpr,
 * logLEExpr, logLTExpr, logNEQExpr, logNEWVExpr, logOrExpr, minusExpr,
 * mulExpr, plusExpr, userBinaryExpr
 *
 * Elements: ( exprModel, exprModel )
 * - Required:
 *   - exprModel (XexprModel)
 * Attributes:
 * - Optional: type (text)
 *
 * @author clementval
 */
public class XbinaryExpr extends XbaseElement {

  private XexprModel _lhs;
  private XexprModel _rhs;
  private String _type;

  /**
   * Constructs new XbinaryExpr by reading element information.
   * @param baseElement Root element of the binary expression.
   */
  public XbinaryExpr(Element baseElement){
    super(baseElement);
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
    
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

  /**
   * Get the left hand side expression.
   * @return XexprModel.
   */
  public XexprModel getLhsExpr(){
    return _lhs;
  }

  /**
   * Get the right hand side expression.
   * @return XexprModel.
   */
  public XexprModel getRhsExpr(){
    return _rhs;
  }
}

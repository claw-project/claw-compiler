/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * The XassignStatement represents the FassignStatement (6.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: (lValueModel, exprModel)
 * - Required:
 *   - IValueModel (XLValueModel)
 *   - exprModel (XexprModel)
 *
 * @author clementval
 */
public class XassignStatement extends XenhancedElement {

  private XLValueModel _lValueModel = null;
  private XexprModel _exprModel = null;


  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XassignStatement(Element baseElement) {
    super(baseElement);

    _lValueModel = XelementHelper.findLValueModel(this, 0);
    _exprModel = XelementHelper.findExprModel(this, 1);
  }


  /**
   * Get the LValueModel object from the lhs of the assign statement.
   * @return An XLValueModel object.
   */
  public XLValueModel getLValueModel(){
    return _lValueModel;
  }

  /**
   * Get the exprModel object from the rhs of the assign statement.
   * @return An XexprModel object.
   */
  public XexprModel getExprModel(){
    return _exprModel;
  }
}

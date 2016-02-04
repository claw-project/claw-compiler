/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * The Xvalue represents the value (8.4) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( exprModel )
 * - Required:
 *   - exprModel (XexprModel)
 *
 * Attributes:
 * - Optional: repeat_count (text)
 *
 * @author clementval
 */

public class Xvalue extends XbaseElement {

  private String _repeatCount = null;
  private XexprModel _exprModel = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xvalue(Element baseElement){
    super(baseElement);
    _repeatCount = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_REPEAT_COUNT);
    _exprModel = XelementHelper.findExprModel(this, false);
  }

  /**
   * Get the exprModel object
   * @return XexprModel object.
   */
  public XexprModel getExprModel(){
    return _exprModel;
  }

  /**
   * Get repeat_count attribute value.
   * @return Attribute value. Null if not defined.
   */
  public String getRepeatCount(){
    return _repeatCount;
  }

  /**
   * Check whether the attribute repeat_count is defined.
   * @return True if the attribute is defined. False otherwise.
   */
  public boolean hasRepeatCount(){
    return _repeatCount != null;
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;


/**
 * The XifStatement represents the FifStatement (6.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - condition (Xcondition)
 *   - then (Xthen)
 * - Optional:
 *   - else (Xelse)
 * Attributes:
 * - Optional: construct_name (text)
 *
 * @author clementval
 */

public class XifStatement extends XbaseElement {

  private Xcondition _cond = null;
  private Xthen _then = null;
  private Xelse _else = null;

  // attributes
  private String _construct_name = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XifStatement(Element baseElement){
    super(baseElement);
    _cond = XelementHelper.findCondition(this, false);
    _then = XelementHelper.findThen(this, false);
    _else = XelementHelper.findElse(this, false);


    // read optional attributes
    _construct_name = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_CONSTRUCT_NAME);
  }


  /**
   * Check whether the element has a construct name attribute defined.
   * @return True the attribute is defined. False otherwise.
   */
  public boolean hasConstructName(){
    return _construct_name != null;
  }

  /**
   * Get the construct name attribute value.
   * @return Construct name value. Null if the attribute is not defined.
   */
  public String getConstructName(){
    return _construct_name;
  }
}

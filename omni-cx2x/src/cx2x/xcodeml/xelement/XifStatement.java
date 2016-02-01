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
 * Elements: (condition, then, else?)
 * - Required:
 *   - condition (Xcondition)
 *   - then (Xthen)
 * - Optional:
 *   - else (Xelse)
 * Attributes:
 * - Optional: construct_name (text)
 *
 * Can have lineno and file attribtues
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

  /**
   * Get the condition part of the if statement.
   * @return A Xcondition object.
   */
  public Xcondition getCondition(){
    return _cond;
  }

  /**
   * Get the then block of the if statement.
   * @return A Xthen object.
   */
  public Xthen getThen(){
    return _then;
  }

  /**
   * Get the else block of the if statement
   * @return A Xelse object. Null if there is no else block. 
   */
  public Xelse getElse(){
    return _else;
  }
}

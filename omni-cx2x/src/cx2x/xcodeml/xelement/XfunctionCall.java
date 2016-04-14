/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The XfunctionCall represents the functionCall (7.5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: ( name, arguments? )
 * - Required:
 *   - name (Xname)
 * - Optional:
 *   - arguments (XargumentsTable)
 * Attribute:
 * - Optional: type (text), is_intrinsic (bool)
 *
 * @author clementval
 */

public class XfunctionCall extends XbaseElement {
  // Elements
  private XargumentsTable _arguments = null;
  private Xname _name = null;

  // Attributes
  private boolean _isInstrinsic = false;
  private String _type = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XfunctionCall(Element baseElement){
    super(baseElement);

    // Read attributes
    _isInstrinsic = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_INTRINSIC);
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);

    // Read element
    _arguments = XelementHelper.findArgumentsTable(this, false);
    _name = XelementHelper.findName(this, false);
  }

  /**
   * Check whether the function is an intrinsic function.
   * @return True if the fct is intrinsic. False otherwise.
   */
  public boolean isIntrinsic(){
    return _isInstrinsic;
  }


  /**
   * Set the value if the is_instrinsic attribute.
   * @param value boolean value of the attribute.
   */
  public void setIntrinsic(boolean value){
    if(value){
      baseElement.setAttribute(XelementName.ATTR_IS_INTRINSIC,
          XelementName.TRUE);
    } else {
      baseElement.setAttribute(XelementName.ATTR_IS_INTRINSIC,
          XelementName.FALSE);
    }
  }

  /**
   * Get the function type.
   * @return Type of the function as a String value.
   */
  public String getType(){
    return _type;
  }

  /**
   * Set the value of the type attribute.
   * @param type New value to be set.
   */
  public void setType(String type){
      baseElement.setAttribute(XelementName.ATTR_TYPE, type);
  }

  /**
   * Get the function's arguments table.
   * @return A XargumentsTable object containing the function's arguments.
   */
  public XargumentsTable getArgumentsTable(){
    return _arguments;
  }

  /**
   * Get the function name.
   * @return Name of the function as an Xname object.
   */
  public Xname getName(){
    return _name;
  }
}

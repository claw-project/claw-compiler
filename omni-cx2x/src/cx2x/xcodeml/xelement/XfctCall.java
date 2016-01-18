/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XfctCall represents the functionCall (7.5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - name (Xname) defined in the Xfct class
 * - Optional:
 *   - arguments (XexprModel) // TODO
 * Attribute:
 * - Optional: type (text), is_intrinsic (bool)
 *
 * @author clementval
 */

public class XfctCall extends Xfct {
  // Elements
  private XargumentsTable _arguments = null;

  // Attributes
  private boolean _isInstrinsic = false;
  private String _type = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XfctCall(Element baseElement){
    super(baseElement);

    // Read attributes
    _isInstrinsic = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_INTRINSIC);
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);

    // Read element
    _arguments = XelementHelper.findArgumentsTable(this, false);
  }

  /**
   * @return True if the fct is intrinsic. False otherwise.
   */
  public boolean isIntrinsic(){
    return _isInstrinsic;
  }

  /**
   * Get the function type.
   * @return Type of the function as a String value.
   */
  public String getType(){
    return _type;
  }

  /**
   * Get the function's arguments table.
   * @return A XargumentsTable object containing the function's arguments.
   */
  public XargumentsTable getArgumentsTable(){
    return _arguments;
  }
}

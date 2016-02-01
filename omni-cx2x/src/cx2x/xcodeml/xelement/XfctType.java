/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import cx2x.xcodeml.helper.*;

/**
 * The XfctType represents the FfunctionType (3.4) element in XcodeML
 * intermediate representation.
 *
 * Elements: (params?)
 * - Optional:
 *   - params (Xparams)
 * Attributes:
 * - Required: type (text), return_type (text)
 * - Optional: result_name (text), is_recursive (bool), is_program (bool),
 *   is_internal (bool)
 *
 * @author clementval
 */

public class XfctType extends Xtype implements Xclonable<XfctType> {

  // Elements
  private Xparams _params = null;

  private String _returnType = null;

  // optional attributes
  private String _resultName = null;
  private boolean _isRecursive = false;
  private boolean _isProgram = false;
  private boolean _isInternal = false;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XfctType(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _params = XelementHelper.findParams(this, false);

    _returnType = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_RETURN_TYPE);

    String value = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_IS_PROGRAM);

    if(value != null && value.equals(XelementName.TRUE)){
      _isProgram = true;
    }

    // read optional attributes
    _resultName = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_RESULT_NAME);
    _isRecursive = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_RECURSIVE);
    _isProgram = XelementHelper.getBooleanAttributeValue(this,
        XelementName.ATTR_IS_PROGRAM);
    _isInternal = XelementHelper.getBooleanAttributeValue(this,
        XelementName.ATTR_IS_INTERNAL);
  }

  /**
   * Get the function result name.
   * @return Result name value.
   */
  public String getResultName(){
    return _resultName;
  }

  /**
   * Check whether function is recursive.
   * @return True if the function is recursive. False otherwise.
   */
  public boolean isRecursive(){
    return _isRecursive;
  }

  /**
   * Check whether function is internal.
   * @return True if the function is internal. False otherwise.
   */
  public boolean isInternal(){
    return _isInternal;
  }

  /**
   * Get the function return type.
   * @return The function's return type as String.
   */
  public String getReturnType(){
    return _returnType;
  }

  /**
   * Check whether function is the program function.
   * @return True if the function is the program function. False otherwise.
   */
  public boolean isProgram(){
    return _isProgram;
  }


  /**
   * Get the params element.
   * @return Params element.
   */
  public Xparams getParams(){
    return _params;
  }

  /**
   * A new object XfctType that is the clone of the current object.
   * @return A new XfctType that is a clone of the current one.
   */
  public XfctType cloneObject() {
    Node clone = cloneNode();
    return new XfctType((Element) clone);
  }

}

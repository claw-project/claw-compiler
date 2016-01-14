package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XfctType represents the FfunctionType (3.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - params TODO
 * Attributes:
 * - Required: type (text), return_type (text)
 * - Optional: result_name (text), is_recursive (bool), is_program (bool),
 *   is_internal (bool)
 */

public class XfctType extends Xtype implements Xclonable<XfctType> {

  private String _returnType = null;

  // optional attributes
  private String _resultName = null;
  private boolean _isRecursive = false;
  private boolean _isProgram = false;
  private boolean _isInternal = false;

  public XfctType(Element element){
    super(element);
    readFctTypeInformation();
  }

  private void readFctTypeInformation(){
    _returnType = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_RETURN_TYPE);

    String value = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_IS_PROGRAM);

    if(value != null && value.equals(XelementName.TRUE)){
      _isProgram = true;
    }

    // TODO read parameters as Xname element

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

  public String getResultName(){
    return _resultName;
  }

  public boolean isRecursive(){
    return _isRecursive;
  }

  public boolean isInternal(){
    return _isInternal;
  }

  public String getReturnType(){
    return _returnType;
  }

  public boolean isProgram(){
    return _isProgram;
  }

  public XfctType cloneObject(){
    Node clone = clone();
    return new XfctType((Element)clone);
  }

}

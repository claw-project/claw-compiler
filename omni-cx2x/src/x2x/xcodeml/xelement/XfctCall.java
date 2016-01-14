package x2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

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
 */

public class XfctCall extends Xfct {

  private XargumentsTable _arguments = null;

  private boolean _isInstrinsic = false;
  private String _type = null;

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

  public boolean isIntrinsic(){
    return _isInstrinsic;
  }

  public String getType(){
    return _type;
  }

  public XargumentsTable getArgumentsTable(){
    return _arguments;
  }
}

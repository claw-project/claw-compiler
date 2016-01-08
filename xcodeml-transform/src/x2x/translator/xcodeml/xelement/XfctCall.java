package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The XfctCall represents the functionCall (7.5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - name (Xname)
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

    _isInstrinsic = XelementHelper.getBooleanAttributeValue(baseElement,
      XelementName.ATTR_IS_INTRINSIC);
    _type = XelementHelper.getAttributeValue(baseElement,
      XelementName.ATTR_TYPE);

    Element arguments = XelementHelper.findFirstElement(baseElement,
      XelementName.ARGUMENTS);
    _arguments = new XargumentsTable(arguments);


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

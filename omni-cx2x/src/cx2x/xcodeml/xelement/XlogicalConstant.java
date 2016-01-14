package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XlogicalConstant represents the FlogicalConstant elements (7.1.1)
 * element in XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text), kind (text)
 */

public class XlogicalConstant extends Xconstant {
  public XlogicalConstant(Element logicalConstantElement){
    super(logicalConstantElement);
  }

  public XlogicalConstant create(XcodeProg xcodeml, String value, String type,
    String kind)
  {
    Xconstant constant = create(xcodeml, value, type, kind, XconstType.LOGICAL);
    return constant == null ? null : (XlogicalConstant)constant;
  }
}

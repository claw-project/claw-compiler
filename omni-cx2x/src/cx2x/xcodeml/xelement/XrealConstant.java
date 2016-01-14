package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XrealConstant represents the FintConstant elements (7.1.1) element in
 * XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text) defined in Xconstant, kind (text) defined in
 *             Xconstant
 */

public class XrealConstant extends Xconstant {
  public XrealConstant(Element element){
    super(element);
  }

  public XrealConstant create(XcodeProg xcodeml, String value, String type,
    String kind)
  {
    Xconstant constant = create(xcodeml, value, type, kind, XconstType.REAL);
    return constant == null ? null : (XrealConstant)constant;
  }
}

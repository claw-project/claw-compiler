package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XintConstant represents the FintConstant elements (7.1.1) element in
 * XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text), kind (text)
 */

public class XintConstant extends Xconstant {
  public XintConstant(Element intConstantElement){
    super(intConstantElement);
  }

  public Element create(XcodeProg xcodeml, String value, String type, String kind){
    // TODO
    return null;
  }
}

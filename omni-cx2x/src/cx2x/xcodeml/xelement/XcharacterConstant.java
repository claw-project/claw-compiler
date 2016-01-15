/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XcharacterConstant represents the FcharacterConstant elements (7.1.1)
 * element in XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text), kind (text)
 *
 * @author clementval
 */

public class XcharacterConstant extends Xconstant {
  public XcharacterConstant(Element charConstantElement){
    super(charConstantElement);
  }

  public XcharacterConstant create(XcodeProg xcodeml, String value, String type,
    String kind)
  {
    Xconstant constant = create(xcodeml, value, type, kind, XconstType.CHAR);
    return constant == null ? null : (XcharacterConstant)constant;
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XintConstant represents the FintConstant elements (7.1.1) element in
 * XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text) defined in Xconstant, kind (text) defined
 *             in Xconstant
 *
 * @author clementval
 */

public class XintConstant extends Xconstant {

  public XintConstant(Element intConstantElement){
    super(intConstantElement);
  }

  public XintConstant create(XcodeProg xcodeml, String value, String type,
    String kind)
  {
    Xconstant constant = create(xcodeml, value, type, kind, XconstType.INT);
    return constant == null ? null : (XintConstant)constant;
  }
}

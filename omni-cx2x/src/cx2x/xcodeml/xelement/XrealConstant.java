/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

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
 *
 * @author clementval
 */

public class XrealConstant extends Xconstant {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XrealConstant(Element baseElement){
    super(baseElement);
  }

}

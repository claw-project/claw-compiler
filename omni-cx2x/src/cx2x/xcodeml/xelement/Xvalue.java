/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xvalue represents the value (8.4) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel TODO
 *
 * @author clementval
 */

public class Xvalue extends XbaseElement {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xvalue(Element baseElement){
    super(baseElement);
  }
}

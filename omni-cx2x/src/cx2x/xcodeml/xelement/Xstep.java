/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xstep represents the step (8.14) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel (XexprModel) defined in Xbound class.
 *
 * @author clementval
 */

public class Xstep extends Xbound {
  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xstep(Element baseElement){
    super(baseElement);
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XupperBound represents the upperBound (8.13) element in
 * XcodeML intermediate representation.
 *
 * @author clementval 
 */

public class XupperBound extends Xbound {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XupperBound(Element baseElement){
    super(baseElement);
  }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xbody represents the body (8.7) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( statementModel* )
 * - statementModel TODO not useful at the moment
 *
 * @author clementval
 */

public class Xbody extends XbaseElement {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xbody(Element baseElement){
    super(baseElement);
  }
}

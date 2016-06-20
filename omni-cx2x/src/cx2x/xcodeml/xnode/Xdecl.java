/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

/**
 * Xdecl is the base class for element in the XdeclTable (XvarDecl, TODO)
 *
 * @author clementval
 */

public class Xdecl extends Xnode {

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root of the element.
   */
  public Xdecl(Element baseElement){
    super(baseElement);
  }
}

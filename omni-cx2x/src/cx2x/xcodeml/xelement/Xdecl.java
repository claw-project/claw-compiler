/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Xdecl is the base class for element in the XdeclTable (XvarDecl, TODO)
 *
 * @author clementval
 */

public class Xdecl extends XbaseElement {

  public Xdecl(Element element){
    super(element);
  }
}

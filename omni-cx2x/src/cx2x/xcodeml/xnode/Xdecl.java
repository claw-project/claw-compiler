/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.helper.XnodeUtil;
import org.w3c.dom.Element;

/**
 * Xdecl is the base class for element in the XdeclTable
 *
 * @author clementval
 */

public class Xdecl extends Xnode {

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public Xdecl(Element baseElement) {
    super(baseElement);
  }

  /**
   * Check whether the var declaration is using a built-in type or a type
   * defined in the type table.
   *
   * @return True if the type is built-in. False otherwise.
   */
  public boolean isBuiltInType() {
    return opcode() == Xcode.VARDECL
        && XnodeUtil.isBuiltInType(matchSeq(Xcode.NAME).getAttribute(Xattr.TYPE));
  }
}

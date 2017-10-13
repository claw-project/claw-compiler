/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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
   * @param node The raw node.
   */
  public Xdecl(Xnode node) {
    super(node.element());
  }
}

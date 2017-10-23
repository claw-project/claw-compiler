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
   * Xnode standard ctor. Pass the Xnode.
   *
   * @param node The raw node.
   */
  public Xdecl(Xnode node) {
    super(node.element());
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

/**
 * Primitive transformation applied on body node. This included:
 * - append a body sub-tree to another one.
 *
 * @author clementval
 */
public final class Body {

  // Avoid instantiation of this class
  private Body() {
  }

  /**
   * Append the slave body to the master body.
   *
   * @param masterBody Master body node.
   * @param slaveBody  Slave body bode.
   * @throws IllegalTransformationException If given nodes are null or not body
   *                                        nodes.
   */
  public static void append(Xnode masterBody, Xnode slaveBody)
      throws IllegalTransformationException
  {
    if(masterBody == null || masterBody.element() == null
        || slaveBody == null || slaveBody.element() == null
        || masterBody.opcode() != Xcode.BODY
        || slaveBody.opcode() != Xcode.BODY)
    {
      throw new IllegalTransformationException("Unable to append body.");
    }

    // Move all nodes to master body
    Xnode crtNode = slaveBody.firstChild();
    while(crtNode != null) {
      Xnode nextSibling = crtNode.nextSibling();
      masterBody.append(crtNode);
      crtNode = nextSibling;
    }
  }
}

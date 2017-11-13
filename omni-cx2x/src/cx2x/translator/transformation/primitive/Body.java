/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;
import org.w3c.dom.Node;

/**
 * Primitive transformation and test applied on body node. This included:
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
    if(masterBody == null || slaveBody == null
        || masterBody.opcode() != Xcode.BODY
        || slaveBody.opcode() != Xcode.BODY)
    {
      throw new IllegalTransformationException(ClawConstant.ERROR_INCOMPATIBLE);
    }

    // Move all nodes to master body
    Xnode crtNode = slaveBody.firstChild();
    while(crtNode != null) {
      Xnode nextSibling = crtNode.nextSibling();
      masterBody.append(crtNode);
      crtNode = nextSibling;
    }
  }

  /**
   * Shift all statements from the first siblings of the "from" element until
   * the "until" element (not included).
   *
   * @param from       Start element for the swifting.
   * @param until      End element for the swifting.
   * @param targetBody Body element in which statements are inserted.
   */
  public static void shiftIn(Xnode from, Xnode until, Xnode targetBody,
                             boolean included)
      throws IllegalTransformationException
  {
    if(from == null || until == null || targetBody == null
        || targetBody.opcode() != Xcode.BODY)
    {
      throw new IllegalTransformationException(ClawConstant.ERROR_INCOMPATIBLE);
    }

    Node currentSibling = from.element();
    if(!included) {
      currentSibling = from.element().getNextSibling();
    }

    Node firstStatementInBody = targetBody.element().getFirstChild();
    while(currentSibling != null && currentSibling != until.element()) {
      Node nextSibling = currentSibling.getNextSibling();
      targetBody.element().insertBefore(currentSibling,
          firstStatementInBody);
      currentSibling = nextSibling;
    }
    if(included && currentSibling == until.element()) {
      targetBody.element().insertBefore(currentSibling,
          firstStatementInBody);
    }
  }
}

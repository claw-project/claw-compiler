/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.helper;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.NestedDoStatement;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;
import org.w3c.dom.Node;

/**
 * Low-level transformation applied on do statements. This included:
 * - loop fusion (merge)
 *
 * @author clementval
 */
public class LoopTransform {

  private static final String[] prevToDelete = {"acc loop", "omp do"};
  private static final String[] nextToDelete = {"omp end do"};

  /**
   * Merge two do statements together. Body of the slave do statement will be
   * append to the body of the master do statement.
   *
   * @param masterDoStmt Master do statement for the merge operation. Stay in
   *                     place. Other do statement will be merged into it.
   * @param slaveDoStmt  Slave do statement for the merge operation. Will be
   *                     merged into the master do statement.
   * @throws IllegalTransformationException If given node are null or not
   *                                        FdoStatement nodes.
   */
  public static void merge(Xnode masterDoStmt, Xnode slaveDoStmt)
      throws IllegalTransformationException
  {
    if(masterDoStmt == null || slaveDoStmt == null
        || masterDoStmt.opcode() != Xcode.FDOSTATEMENT
        || slaveDoStmt.opcode() != Xcode.FDOSTATEMENT)
    {
      throw new IllegalTransformationException(
          "Incompatible node to perform a merge");
    }

    // Merge slave body into the master body
    appendBody(masterDoStmt.body(), slaveDoStmt.body());

    // Delete any acc loop / omp do pragma before/after the do statements.
    cleanPragmas(slaveDoStmt, prevToDelete, nextToDelete);
    slaveDoStmt.delete();
  }

  /**
   * Merge two nested do statements group together. Inner most body of the slave
   * group is merged into the inner most body of the master group. Slave group
   * is then deleted.
   *
   * @param master Master nested do statements group for the merge operation.
   * @param slave  Slave nested do statements group for the merge operation.
   * @throws IllegalTransformationException If given node are null or not
   *                                        FdoStatement nodes.
   */
  public static void merge(NestedDoStatement master, NestedDoStatement slave)
      throws IllegalTransformationException
  {
    if(master == null || master.size() == 0 || slave == null
        || slave.size() == 0)
    {
      throw new IllegalTransformationException(
          "Incompatible node to perform a merge");
    }
    merge(master.getInnerStatement(), slave.getInnerStatement());
    slave.getOuterStatement().delete();
  }

  /**
   * Append the slave body to the master body.
   *
   * @param masterBody Master body node.
   * @param slaveBody  Slave body bode.
   * @throws IllegalTransformationException If given nodes are null or not body
   *                                        nodes.
   */
  private static void appendBody(Xnode masterBody, Xnode slaveBody)
      throws IllegalTransformationException
  {
    if(masterBody == null || masterBody.element() == null
        || slaveBody == null || slaveBody.element() == null
        || masterBody.opcode() != Xcode.BODY
        || slaveBody.opcode() != Xcode.BODY)
    {
      throw new IllegalTransformationException("Unable to append body.");
    }

    // Append content of slave body master body
    Node childNode = slaveBody.element().getFirstChild();
    while(childNode != null) {
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE) {
        masterBody.element().appendChild(childNode);
      }
      childNode = nextChild;
    }
  }

  /**
   * Clean up extra pragma that have no more sense after transformation.
   *
   * @param node     Do statement that will be removed.
   * @param previous List of pragma to be removed before the do statement.
   * @param next     List of pragmas to be removed after the do statement.
   */
  private static void cleanPragmas(Xnode node, String[] previous, String[] next)
  {
    if(node.opcode() != Xcode.FDOSTATEMENT) {
      return;
    }

    Xnode doStatement = node;

    while(node.prevSibling() != null
        && node.prevSibling().opcode() == Xcode.FPRAGMASTATEMENT) {
      String pragma = node.prevSibling().value();
      Xnode toDelete = null;

      for(String p : previous) {
        if(!pragma.startsWith(ClawConstant.CLAW) && pragma.contains(p)) {
          toDelete = node.prevSibling();
          break;
        }
      }

      node = node.prevSibling();
      XnodeUtil.safeDelete(toDelete);
    }

    node = doStatement; // Reset node to the initial position.
    while(node.nextSibling() != null
        && node.nextSibling().opcode() == Xcode.FPRAGMASTATEMENT) {
      String pragma = node.nextSibling().value();
      Xnode toDelete = null;

      for(String n : next) {
        if(!pragma.startsWith(ClawConstant.CLAW) && pragma.contains(n)) {
          toDelete = node.nextSibling();
          break;
        }
      }

      node = node.nextSibling();
      XnodeUtil.safeDelete(toDelete);
    }
  }
}

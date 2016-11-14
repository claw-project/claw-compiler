/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

/**
 * Class holding information about a group of nested loop in the loop-hoist
 * transformation
 *
 * @author clementval
 */
class LoopHoistDoStmtGroup {

  private boolean _needExtraction = false;
  private boolean _needIfication = false;
  private Xnode[] _doStmts = null;

  /**
   * Constructs a do statements group with the given do statements.
   *
   * @param doStmts Array of do statements.
   */
  public LoopHoistDoStmtGroup(Xnode[] doStmts) {
    _doStmts = doStmts;
  }

  /**
   * Set the IF extraction flag to true.
   */
  void setExtraction() {
    _needExtraction = true;
  }

  /**
   * Set the IF statement creation flag to true.
   */
  void setIfStatement() {
    _needIfication = true;
  }

  /**
   * Check whether this group of do statements needs the extraction from na IF
   * statement.
   *
   * @return True if the group need the extraction. False otherwise.
   */
  boolean needExtraction() {
    return _needExtraction;
  }

  /**
   * Check whether this group of do statements needs the creation of na IF
   * statement.
   *
   * @return True if the group need the creation. False otherwise.
   */
  boolean needIfStatement() {
    return _needIfication;
  }

  /**
   * Get the array of do statements associated with this object.
   *
   * @return Do statements array associated with this group.
   */
  Xnode[] getDoStmts() {
    return _doStmts;
  }

  /**
   * Clone the object and the element referred to in the AST.
   *
   * @return A clone of the current object with referring to clone of its
   * elements.
   */
  LoopHoistDoStmtGroup cloneObjectAndElement() {
    Xnode newDoStmt = _doStmts[0].cloneNode();
    Xnode[] nestedDoStmts = new Xnode[_doStmts.length];
    nestedDoStmts[0] = newDoStmt;
    for(int j = 1; j < nestedDoStmts.length; ++j) {
      Xnode next = XnodeUtil.matchDescendant(Xcode.FDOSTATEMENT,
          nestedDoStmts[j - 1].body(), false);
      nestedDoStmts[j] = next;
    }
    return new LoopHoistDoStmtGroup(nestedDoStmts);
  }
}

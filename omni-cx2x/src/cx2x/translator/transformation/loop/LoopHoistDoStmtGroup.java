/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XdoStatement;

/**
 * Class holding information about a group of nested loop in the loop-hoist
 * transformation
 *
 * @author clementval
 */
public class LoopHoistDoStmtGroup {
  private boolean _needExtraction = false;
  private boolean _needIfication = false;
  private XdoStatement[] _doStmts = null;

  /**
   * Constrcuts a do statements group with the given do statements.
   * @param doStmts Array of do statements.
   */
  public LoopHoistDoStmtGroup(XdoStatement[] doStmts){
    _doStmts = doStmts;
  }

  /**
   * Set the IF extraction flag to true.
   */
  void setExtraction(){
    _needExtraction = true;
  }

  /**
   * Set the IF statement creation flag to true.
   */
  void setIfStatement(){
    _needIfication = true;
  }

  /**
   * Check whether this group of do statements needs the extraction from na IF
   * statement.
   * @return True if the group need the extraction. False otherwise.
   */
  boolean needExtraction(){
    return _needExtraction;
  }

  /**
   * Check whether this group of do statements needs the creation of na IF
   * statement.
   * @return True if the group need the creation. False otherwise.
   */
  boolean needIfStatement(){
    return _needIfication;
  }

  /**
   * Get the array of do statements associated with this object.
   * @return Do statements array associated with this group. 
   */
  XdoStatement[] getDoStmts(){
    return _doStmts;
  }

  /**
   * Clone the object and the element referred to in the AST.
   * @return A clone of the current object with referring to clone of its
   * elements.
   */
  LoopHoistDoStmtGroup cloneObjectAndElement() {
    XdoStatement newDoStmt = _doStmts[0].cloneObject();
    XdoStatement[] nestedDoStmts = new XdoStatement[_doStmts.length];
    nestedDoStmts[0] = newDoStmt;
    for(int j = 1; j < nestedDoStmts.length; ++j){
      XdoStatement next =
          XelementHelper.findDoStatement(nestedDoStmts[j-1].getBody(), false);
      nestedDoStmts[j] = next;
    }
    return new LoopHoistDoStmtGroup(nestedDoStmts);
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.xcodeml.exception.IllegalTransformationException;
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

  public LoopHoistDoStmtGroup(XdoStatement[] doStmts){
    _doStmts = doStmts;
  }

  public void setExtraction(){
    _needExtraction = true;
  }

  public void setIfStatement(){
    _needIfication = true;
  }

  public boolean needExtraction(){
    return _needExtraction;
  }

  public boolean needIfStatement(){
    return _needIfication;
  }

  public XdoStatement[] getDoStmts(){
    return _doStmts;
  }

  /**
   *
   * @return
   */
  public LoopHoistDoStmtGroup cloneObjectAndElement() {
    XdoStatement newDoStmt = _doStmts[0].cloneObject();
    XdoStatement[] nestedDoStmts = new XdoStatement[_doStmts.length];
    nestedDoStmts[0] = newDoStmt;
    for(int j = 1; j < nestedDoStmts.length; ++j){
      XdoStatement next =
          XelementHelper.findDoStatement(nestedDoStmts[j-1].getBody(), false);
      nestedDoStmts[j] = next;
    }
    LoopHoistDoStmtGroup newDoStmtGroup =
        new LoopHoistDoStmtGroup(nestedDoStmts);
    return newDoStmtGroup;
  }
}

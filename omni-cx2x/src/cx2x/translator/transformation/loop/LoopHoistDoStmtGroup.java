/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

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

}

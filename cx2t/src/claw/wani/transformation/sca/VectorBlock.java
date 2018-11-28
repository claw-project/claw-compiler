/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author clementval
 */
public class VectorBlock {

  private final Xnode _startStmt;
  private Xnode _endStmt;

  private final Set<String> _usedVars;

  public VectorBlock(Xnode startStmt) {
    _startStmt = startStmt;
    _endStmt = null;
    _usedVars = new HashSet<>();
  }

  public boolean isSingleStatement() {
    return _endStmt == null;
  }

  public void setEndStmt(Xnode endStmt) {
    _endStmt = endStmt;
  }

  public Xnode getStartStmt() {
    return _startStmt;
  }

  public Xnode getEndStmt() {
    return _endStmt;
  }

  public void gatherUsedVariables() {

    if(isSingleStatement()) {

      List<Xnode> vars = _startStmt.matchAll(Xcode.VAR);
      for(Xnode var : vars) {
        _usedVars.add(var.value());
      }
    } else {
      Xnode crt = getStartStmt();
      while(!crt.equals(getEndStmt())) {
        List<Xnode> vars = crt.matchAll(Xcode.VAR);
        for(Xnode var : vars) {
          _usedVars.add(var.value());
        }
        crt = crt.nextSibling();
      }

      if(crt.equals(getEndStmt())) {
        List<Xnode> vars = crt.matchAll(Xcode.VAR);
        for(Xnode var : vars) {
          _usedVars.add(var.value());
        }
      }
    }
  }

  public Set<String> getUsedVariables() {
    return _usedVars;
  }

  /**
   * Check if the given node is the direct sibling of this node.
   *
   * @param potentialSibling Potential next sibling to test for.
   * @return True if the given node is the direct next sibling. False otherwise.
   */
  public boolean canMergeNextNode(Xnode potentialSibling) {
    if(isSingleStatement()) {
      return getStartStmt().nextSibling() != null
          && getStartStmt().nextSibling().equals(potentialSibling);
    } else {
      return getEndStmt().nextSibling() != null
          && getEndStmt().nextSibling().equals(potentialSibling);
    }
  }

}

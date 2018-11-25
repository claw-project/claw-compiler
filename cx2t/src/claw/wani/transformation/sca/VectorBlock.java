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

  private final Set<String> _writtenVariables;

  public VectorBlock(Xnode startStmt) {
    _startStmt = startStmt;
    _endStmt = null;
    _writtenVariables = new HashSet<>();
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

  public void gatherUsedVars() {



    if(isSingleStatement()) {
      if(_startStmt.opcode() == Xcode.F_ASSIGN_STATEMENT) {
        AssignStatement as = new AssignStatement(_startStmt.element());
        _writtenVariables.add(as.getLhsName());
        _writtenVariables.addAll(as.getReadNames());
      } else {
        populateWrittenVars(_startStmt.matchAll(Xcode.F_ASSIGN_STATEMENT));
      }
    } else {
      populateWrittenVars(XnodeUtil.getNodes(getStartStmt(), getEndStmt(),
          Collections.singletonList(Xcode.F_ASSIGN_STATEMENT)));
    }
  }

  private void populateWrittenVars(List<Xnode> assignStatements) {
    for(Xnode node : assignStatements) {
      AssignStatement as = new AssignStatement(node.element());
      _writtenVariables.add(as.getLhsName());
      _writtenVariables.addAll(as.getReadNames());
    }
  }

  public Set<String> getWrittenVariables() {
    return _writtenVariables;
  }

}

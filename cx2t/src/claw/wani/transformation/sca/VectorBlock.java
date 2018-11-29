/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Class representing a set of contiguous statements that can be wrapped in
 * a do statement and should vectorize.
 *
 * @author clementval
 */
public class VectorBlock {

  private final Xnode _startStmt;
  private Xnode _endStmt;

  private Set<String> _usedVars = null;

  /**
   * Create a new VectorBlock instance with a single statement.
   *
   * @param startStmt Statement used as the start statement of the created
   *                  block.
   */
  public VectorBlock(Xnode startStmt) {
    _startStmt = startStmt;
    _endStmt = null;
  }

  /**
   * Check whether the VectorBlock is composed of a single statement.
   *
   * @return True if the block is composed by a single statement.
   */
  public boolean isSingleStatement() {
    return _endStmt == null;
  }

  /**
   * Set the end statement of the block.
   *
   * @param endStmt Statement to be set as end statement.
   */
  public void setEndStmt(Xnode endStmt) {
    _endStmt = endStmt;
  }

  /**
   * Get the start statement of the block.
   *
   * @return Start node.
   */
  public Xnode getStartStmt() {
    return _startStmt;
  }

  /**
   * Get the end statement of the block.
   *
   * @return End node.
   */
  public Xnode getEndStmt() {
    return _endStmt;
  }

  /**
   * Collect all variables names used within the block.
   */
  private void gatherUsedVariables() {

    if(_usedVars == null) {
      _usedVars = new HashSet<>();
    }

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

  /**
   * Get all variable names used within the block.
   *
   * @return Set containing variables names.
   */
  public Set<String> getUsedVariables() {
    if(_usedVars == null) {
      gatherUsedVariables();
    }
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

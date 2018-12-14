/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.*;

/**
 * Class representing a set of contiguous statements that can be wrapped in
 * a do statement and should vectorize.
 *
 * @author clementval
 */
public class VectorBlock {

  private final Xnode _startStmt;
  private Xnode _endStmt;

  private Set<String> _readAndWrittenVariables = null;
  private Set<String> _writtenVariables = null;

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

    if(_readAndWrittenVariables == null) {
      _readAndWrittenVariables = new HashSet<>();
    }

    if(isSingleStatement()) {

      List<Xnode> vars = _startStmt.matchAll(Xcode.VAR);
      for(Xnode var : vars) {
        _readAndWrittenVariables.add(var.value());
      }
    } else {
      Xnode crt = getStartStmt();
      while(!crt.equals(getEndStmt())) {
        List<Xnode> vars = crt.matchAll(Xcode.VAR);
        for(Xnode var : vars) {
          _readAndWrittenVariables.add(var.value());
        }
        crt = crt.nextSibling();
      }

      if(crt.equals(getEndStmt())) {
        List<Xnode> vars = crt.matchAll(Xcode.VAR);
        for(Xnode var : vars) {
          _readAndWrittenVariables.add(var.value());
        }
      }
    }
  }

  /**
   * Collect all variables written within the block.
   */
  private void gatherWrittenVariables() {
    if(_writtenVariables == null) {
      _writtenVariables = new HashSet<>();
    }

    if(isSingleStatement()) {
      fillUpWrittenVariables(_startStmt);
    } else {
      Xnode crt = getStartStmt();
      while(!crt.equals(getEndStmt())) {
        fillUpWrittenVariables(crt);
        crt = crt.nextSibling();
      }

      if(crt.equals(getEndStmt())) {
        fillUpWrittenVariables(crt);
      }
    }
  }

  private void fillUpWrittenVariables(Xnode root) {
    List<Xnode> assignStatements = root.matchAll(Xcode.F_ASSIGN_STATEMENT);
    for(Xnode assign : assignStatements) {
      AssignStatement as = new AssignStatement(assign.element());
      _writtenVariables.add(as.getLhsName());
    }
  }

  public Set<String> getWrittenVariables() {
    if(_writtenVariables == null) {
      gatherWrittenVariables();
    }
    return _writtenVariables;
  }

  /**
   * Get all variable names used within the block.
   *
   * @return Set containing variables names.
   */
  public Set<String> getReadAndWrittentVariables() {
    if(_readAndWrittenVariables == null) {
      gatherUsedVariables();
    }
    return _readAndWrittenVariables;
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

  /**
   * Merge adjacent block together to maximize vectorization and data locality.
   *
   * @param blocks Set of flagged blocks containing a single statement.
   * @return List of merged blocks.
   */
  public static List<VectorBlock> mergeAdjacent(Set<VectorBlock> blocks) {

    List<VectorBlock> sortedVectorBlocks = sortBlockByLineOrder(blocks);
    List<VectorBlock> toBeRemoved = new ArrayList<>();

    if(blocks.isEmpty()) {
      return sortedVectorBlocks;
    }

    VectorBlock crtBlock = sortedVectorBlocks.get(0);
    for(int i = 1; i < sortedVectorBlocks.size(); ++i) {
      VectorBlock nextBlock = sortedVectorBlocks.get(i);
      if(nextBlock.getStartStmt().opcode() == Xcode.F_ASSIGN_STATEMENT
          && crtBlock.canMergeNextNode(nextBlock.getStartStmt()))
      {
        toBeRemoved.add(nextBlock);
        crtBlock.setEndStmt(nextBlock.getStartStmt());
      } else {
        crtBlock = nextBlock;
      }
    }

    sortedVectorBlocks.removeAll(toBeRemoved);
    return sortedVectorBlocks;
  }



  /**
   * Sort the vector blocks according to their position in the code.
   *
   * @param blocks Set of vector blocks
   * @return List of ordered vector block.
   */
  private static List<VectorBlock> sortBlockByLineOrder(Set<VectorBlock> blocks)
  {
    List<VectorBlock> sortedVectorBlocks = new ArrayList<>(blocks);
    Collections.sort(sortedVectorBlocks, new Comparator<VectorBlock>() {
      @Override
      public int compare(VectorBlock s1, VectorBlock s2) {
        if(s1.getStartStmt().lineNo() < s2.getStartStmt().lineNo()) {
          return -1;
        } else if(s1.getStartStmt().lineNo() > s2.getStartStmt().lineNo()) {
          return 1;
        }
        return 0;
      }
    });
    return sortedVectorBlocks;
  }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.primitive.Loop;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;

import java.util.ArrayList;
import java.util.List;

/**
 * Class holding information about nested do statements.
 *
 * @author clementval
 */
public class NestedDoStatement {

  private final List<Xnode> _statements;
  private List<String> _inductionVariables = null;

  /**
   * Constructs a group of nested do statements from the outer statement.
   *
   * @param outerDoStatement Do statement on the outer level.
   */
  public NestedDoStatement(Xnode outerDoStatement) {
    this(outerDoStatement, 0);
  }

  /**
   * Constructs a group of nested do statements from the outer statement with
   * maximum Nb do statements.
   *
   * @param outerDoStatement Do statement node to start the nested group.
   * @param nb               Number of do statements. If nb &le; 0 match all
   *                         possible nested do statement.
   */
  public NestedDoStatement(Xnode outerDoStatement, int nb) {
    _statements = new ArrayList<>();
    if(!Xnode.isOfCode(outerDoStatement, Xcode.F_DO_STATEMENT)) {
      return;
    }

    Xnode crtDoStatement = outerDoStatement;
    while(crtDoStatement != null) {
      _statements.add(crtDoStatement);
      crtDoStatement = crtDoStatement.body().
          matchDirectDescendant(Xcode.F_DO_STATEMENT);
      if(_statements.size() == nb) {
        break;
      }
    }
  }

  /**
   * Constructs a group of nested do statements from a list of dimension
   * objects. The outer statement represents the first element of the list and
   * the inner statement represents the last element of the list.
   *
   * @param dimensions A list of dimension objects.
   * @param xcodeml    The current XcodeML translation unit in which the
   *                   elements will be created.
   */
  public NestedDoStatement(List<DimensionDefinition> dimensions,
                           XcodeProgram xcodeml)
  {
    _statements = new ArrayList<>();
    for(DimensionDefinition dim : dimensions) {
      Xnode induction = xcodeml.createVar(FortranType.INTEGER,
          dim.getIdentifier(), Xscope.LOCAL);
      Xnode range = dim.generateIterationRange(xcodeml, true);
      Xnode doStmt = xcodeml.createDoStmt(induction, range);
      if(!_statements.isEmpty()) {
        _statements.get(_statements.size() - 1).body().append(doStmt);
      }
      _statements.add(doStmt);
    }
  }

  /**
   * Constructs a group of nested do statements from a promotion information
   * object to iterate over assumed shape arrays.
   *
   * @param pi      Promotion information of one array taken as basis for the
   *                iteration.
   * @param xcodeml The current XcodeML translation unit in which the
   *                elements will be created.
   */
  public NestedDoStatement(List<DimensionDefinition> dimensions,
                           PromotionInfo pi, XcodeProgram xcodeml)
  {
    _statements = new ArrayList<>();
    int index = 1;
    for(DimensionDefinition dim : dimensions) {
      _statements.add(Loop.createDoStmtOverAssumedShapeArray(
          pi.getTargetType(), pi.getIdentifier(), dim.getIdentifier(),
          index, xcodeml));
      ++index;
    }
  }

  /**
   * Get the outer do statements. First do statement in the nested group.
   *
   * @return XdoStatement holding information about the outer do statement.
   */
  public Xnode getOuterStatement() {
    return _statements.isEmpty() ? null : _statements.get(0);
  }

  /**
   * Get statement at level i
   *
   * @param i Level index starts at 0 for outer do statement.
   * @return The do statement at level i.
   */
  public Xnode get(int i) {
    return _statements.get(i);
  }

  /**
   * Get all the induction variable in the nested do statement group.
   *
   * @return List of induction variable as string.
   */
  public List<String> getInductionVariables() {
    if(_inductionVariables == null) {
      _inductionVariables = new ArrayList<>();
      for(Xnode doStmt : _statements) {
        _inductionVariables.add(Loop.extractInductionVariable(doStmt));
      }
    }
    return _inductionVariables;
  }

  /**
   * Compute the swapping indices for the new ordering. Gives new position
   * position of the xth index from right to left.
   * E.g. current to new position: i,j,k &rarr; k,i,j = 120 i goes to pos 1, j
   * goes to pos 2 and k goes to pos 0.
   *
   * @param newInductionVarOrder List of induction variable in their new order.
   * @return Computed new position indices.
   */
  public int computeSwappingIndices(List<String> newInductionVarOrder) {
    int swapIndices = 0;
    if(newInductionVarOrder.size() != size()) {
      return swapIndices;
    }

    for(int i = 0; i < newInductionVarOrder.size(); ++i) {
      newInductionVarOrder.set(i, newInductionVarOrder.get(i).toLowerCase());
    }

    int crtShift = (int) Math.pow(10, newInductionVarOrder.size() - 1.0);
    List<String> crtInductionVarOrder = getInductionVariables();
    for(String inductionVar : crtInductionVarOrder) {
      int pos = newInductionVarOrder.indexOf(inductionVar.toLowerCase());
      if(pos >= 0) {
        swapIndices += pos * crtShift;
      }
      if(crtShift != 0) {
        crtShift /= 10;
      }
    }
    return swapIndices;
  }

  /**
   * Get the inner do statements. Last do statement in the nested group.
   *
   * @return XdoStatement holding information about the inner do statement.
   */
  public Xnode getInnerStatement() {
    return _statements.isEmpty() ?
        null : _statements.get(_statements.size() - 1);
  }

  /**
   * Get the size of the group of nested do statements.
   *
   * @return Size of the group.
   */
  public int size() {
    return _statements.size();
  }

  /**
   * Clone current nested group with all its elements.
   *
   * @return Newly created nested do statements group.
   */
  public NestedDoStatement cloneNestedGroup() {
    Xnode newDoStmt = _statements.get(0).cloneNode();
    return new NestedDoStatement(newDoStmt, size());
  }
}

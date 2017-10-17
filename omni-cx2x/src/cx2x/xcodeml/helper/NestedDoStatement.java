/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.helper;

import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.xnode.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Class holding information about nested do statements.
 * <p>
 * Created by clementval on 20/05/16.
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
    _statements = new ArrayList<>();
    if(outerDoStatement.opcode() == Xcode.FDOSTATEMENT) {
      _statements.add(outerDoStatement);
      List<Xnode> childDoStatements =
          outerDoStatement.matchAll(Xcode.FDOSTATEMENT);
      _statements.addAll(childDoStatements);
      // TODO do only truly nested group
    }
  }

  /**
   * Constructs a group of nested do statements from a list of dimension
   * objects. The outer statement represents the first element of the list and
   * the inner statement represents the last element of the list.
   *
   * @param dimensions A list of dimension objects.
   * @param xcodeml    The current XcodeML program unit in which the elements
   *                   will be created.
   */
  public NestedDoStatement(List<DimensionDefinition> dimensions,
                           XcodeProgram xcodeml)
  {
    _statements = new ArrayList<>();
    for(DimensionDefinition dim : dimensions) {
      Xnode induction = xcodeml.createVar(Xname.TYPE_F_INT, dim.getIdentifier(),
          Xscope.LOCAL);
      Xnode range = dim.generateIndexRange(xcodeml, true);
      Xnode doSt = xcodeml.createDoStmt(induction, range);
      if(_statements.size() != 0) {
        _statements.get(_statements.size() - 1).body().
            append(doSt, false);
      }
      _statements.add(doSt);
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
        _inductionVariables.add(XnodeUtil.extractInductionVariable(doStmt));
      }
    }
    return _inductionVariables;
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
  public int getGroupSize() {
    return _statements.size();
  }

}

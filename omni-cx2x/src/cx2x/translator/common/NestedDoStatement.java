package cx2x.translator.common;

import cx2x.translator.language.ClawDimension;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XelementName;
import cx2x.xcodeml.xelement.Xscope;
import cx2x.xcodeml.xnode.Xnode;

import java.util.ArrayList;
import java.util.List;

/**
 * Class holding information about nested do statements.
 *
 * Created by clementval on 20/05/16.
 */
public class NestedDoStatement {

  private final List<Xnode> _statements;

  /**
   * Constructs a group of nested do statements from a list of dimension
   * objects. The outer statement represents the first element of the list and
   * the inner statement represents the last element of the list.
   * @param dimensions A list of dimension objects.
   * @param xcodeml    The current XcodeML program unit in which the elements
   *                   will be created.
   */
  public NestedDoStatement(List<ClawDimension> dimensions,
                           XcodeProgram xcodeml)
  {
    _statements = new ArrayList<>();
    for (ClawDimension dim : dimensions) {
      Xnode induction = XelementHelper.createVar(XelementName.TYPE_F_INT,
          dim.getIdentifier(), Xscope.LOCAL, xcodeml);
      Xnode range = dim.generateIndexRange(xcodeml, true);
      Xnode doSt = XelementHelper.createDoStmt(induction, range, xcodeml);
      if (_statements.size() != 0) {
        _statements.get(_statements.size() - 1).getBody().
            appendToChildren(doSt, false);
      }
      _statements.add(doSt);
    }
  }

  /**
   * Get the outer do statements. First do statement in the nested group.
   * @return XdoStatement holding information about the outer do statement.
   */
  public Xnode getOuterStatement(){
    return _statements.isEmpty() ? null : _statements.get(0);
  }

  /**
   * Get the inner do statements. Last do statement in the nested group.
   * @return XdoStatement holding information about the inner do statement.
   */
  public Xnode getInnerStatement(){
    return _statements.isEmpty() ? null : _statements.get(_statements.size()-1);
  }

  /**
   * Get the size of the group of nested do statements.
   * @return Size of the group.
   */
  public int getGroupSize(){
    return _statements.size();
  }

}

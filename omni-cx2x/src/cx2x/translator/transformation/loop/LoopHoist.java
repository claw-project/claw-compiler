/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.ClawReshapeInfo;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.BlockTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.XfunctionDefinition;
import cx2x.xcodeml.xnode.Xnode;

import java.util.ArrayList;
import java.util.List;

/**
 * A LoopHoist transformation is an independent transformation over a
 * structured block. It performs loop fusion in the given block as well as do
 * start statement hoisting.
 *
 * @author clementval
 */
public class LoopHoist extends BlockTransformation {

  private final List<LoopHoistDoStmtGroup> _doGroup;
  private final ClawLanguage _startClaw, _endClaw;
  private int _nestedLevel;

  /**
   * Constructs a new LoopHoist triggered from a specific directive.
   * @param startDirective The directive that triggered the loop hoist
   *                       transformation.
   * @param endDirective   The directive that end the structured block.
   */
  public LoopHoist(ClawLanguage startDirective, ClawLanguage endDirective) {
    super(startDirective, endDirective);
    _startClaw = startDirective;
    _endClaw = endDirective;
    _doGroup = new ArrayList<>();
  }

  /**
   * @see Transformation#analyze(XcodeProgram, Transformer)
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    int _pragmaDepthLevel = XnodeUtil.getDepth(_startClaw.getPragma());
    _nestedLevel = _startClaw.getHoistInductionVars().size();

    // Find all the group of nested loops that can be part of the hoisting
    List<Xnode> statements =
        XnodeUtil.findDoStatement(_startClaw.getPragma(),
            _endClaw.getPragma(), _startClaw.getHoistInductionVars());

    if(statements.size() == 0){
      xcodeml.addError("No do statement group meets the criteria of hoisting.",
          _startClaw.getPragma().getLineNo());
      return false;
    }

    for(int i = 0; i < statements.size(); i++){
      Xnode[] group = new Xnode[_nestedLevel];
      LoopHoistDoStmtGroup g = new LoopHoistDoStmtGroup(group);
      try {
        reloadDoStmts(g, statements.get(i));
      } catch (IllegalTransformationException e) {
        xcodeml.addError("Group " + i + " of do statements do not meet the" +
                " criteria of loop hoisting (Group index starts at 0).",
            _startClaw.getPragma().getLineNo());
        return false;
      }

      LoopHoistDoStmtGroup crtGroup = new LoopHoistDoStmtGroup(group);
      int depth = XnodeUtil.getDepth(group[0]);
      if(depth != _pragmaDepthLevel){
        Xnode tmpIf = XnodeUtil.findParent(Xcode.FIFSTATEMENT, group[0]);
        Xnode tmpSelect = XnodeUtil.findParent(Xcode.FSELECTCASESTATEMENT, group[0]);
        if(tmpIf == null && tmpSelect == null){
          xcodeml.addError("Group " + i + " is nested in an unsupported " +
              "statement for loop hoisting (Group index starts at 0).",
              _startClaw.getPragma().getLineNo());
          return false;
        }

        int ifDepth = XnodeUtil.getDepth(tmpIf);
        int selectDepth = XnodeUtil.getDepth(tmpSelect);
        if ((_pragmaDepthLevel <= ifDepth || _pragmaDepthLevel <= selectDepth)
            && (ifDepth < depth || selectDepth < depth)){
          crtGroup.setExtraction();
        } else {
          xcodeml.addError("Group " + i + " is nested in an unsupported " +
              "statement for loop hoisting or depth is too high " +
              "(Group index starts at 0).",
              _startClaw.getPragma().getLineNo());
          return false;
        }
      }
      _doGroup.add(crtGroup);
    }

    LoopHoistDoStmtGroup master = _doGroup.get(0);
    for (int i = 1; i < _doGroup.size(); ++i){
      LoopHoistDoStmtGroup next = _doGroup.get(i);
      for(int j = 0; j < master.getDoStmts().length; ++j){
        // Iteration range are identical, just merge
        if(j == 0
            && (
              !XnodeUtil.hasSameIndexRange(master.getDoStmts()[j],
                next.getDoStmts()[j])
              && XnodeUtil.hasSameIndexRangeBesidesLower(master.getDoStmts()[j],
                next.getDoStmts()[j])
            )
          )
        {
          // Iteration range are identical besides lower-bound, if creation
          next.setIfStatement();
        } else if(!XnodeUtil.hasSameIndexRange(master.getDoStmts()[j],
            next.getDoStmts()[j]))
        {
          // Iteration range are too different, stop analysis
          xcodeml.addError("Iteration range of do statements group " + i +
                  " differs from group 0. Loop hoisting aborted.",
              _startClaw.getPragma().getLineNo());
          return false;
        }
      }
    }


    // Check reshape mandatory points
    if(_startClaw.hasReshapeClause()) {
      XfunctionDefinition fctDef =
          XnodeUtil.findParentFunction(_startClaw.getPragma());
      if(fctDef == null){
        xcodeml.addError("Unable to find the function/subroutine/module " +
            "definition including the current directive",
            _startClaw.getPragma().getLineNo()
        );
        return false;
      }

      for (ClawReshapeInfo r : _startClaw.getReshapeClauseValues()) {
        if (!fctDef.getSymbolTable().contains(r.getArrayName()) ||
            !fctDef.getDeclarationTable().contains(r.getArrayName()))
        {
          // Check in the parent def if present
          if(!checkUpperDefinition(fctDef, r.getArrayName())){
            xcodeml.addError(String.format("Reshape variable %s not found in " +
                    "the definition of %s", r.getArrayName(),
                fctDef.getName().getValue()), _startClaw.getPragma().getLineNo()
            );
            return false;
          }
        }
      }
    }
    return true;
  }

  /**
   * Check whether the id is present in the parent function definition if the
   * current fct definition is nested.
   * @param fctDef Current function definition.
   * @param name   Id to be looked for.
   * @return True if the id has been found. False otherwise.
   */
  private boolean checkUpperDefinition(XfunctionDefinition fctDef, String name)
  {
    XfunctionDefinition upperDef = XnodeUtil.findParentFunction(fctDef);
    if (upperDef == null) {
      return false;
    }
    return !(!upperDef.getSymbolTable().contains(name)
        || !upperDef.getDeclarationTable().contains(name))
        || checkUpperDefinition(upperDef, name);
  }

  /**
   * @see Transformation#canBeTransformedWith(Transformation)
   */
  @Override
  public boolean canBeTransformedWith(Transformation transformation) {
    return false;
  }

  /**
   * @see Transformation#transform(XcodeProgram, Transformer, Transformation)
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation) throws Exception
  {

    List<LoopFusion> fusions = new ArrayList<>();
    // Perform IF extraction and IF creation for lower-bound
    for(LoopHoistDoStmtGroup g : _doGroup){
      if(g.needIfStatement()){
        createIfStatementForLowerBound(xcodeml, g);
      }
      XnodeUtil.extractBody(g.getDoStmts()[_nestedLevel-1], g.getDoStmts()[0]);
      g.getDoStmts()[0].delete();
    }

    // Do the hoisting
    LoopHoistDoStmtGroup hoisted = _doGroup.get(0).cloneObjectAndElement();
    hoisted.getDoStmts()[_nestedLevel-1].getBody().delete();
    Xnode newBody = new Xnode(Xcode.BODY, xcodeml);
    hoisted.getDoStmts()[_nestedLevel-1].appendToChildren(newBody, false);
    XnodeUtil.shiftStatementsInBody(_startClaw.getPragma(), _endClaw.getPragma(), newBody);
    XnodeUtil.insertAfter(_startClaw.getPragma(), hoisted.getDoStmts()[0]);

    // Generate dynamic transformation (interchange)
    TransformationHelper.generateAdditionalTransformation(_startClaw,
        xcodeml, transformer, hoisted.getDoStmts()[0]);

    // Apply reshape clause
    TransformationHelper.applyReshapeClause(_startClaw, xcodeml);

    // Delete pragmas
    _startClaw.getPragma().delete();
    _endClaw.getPragma().delete();
  }

  /**
   * Create an IF statement surrounding the entire most inner do statement body.
   * Condition if made from the lower bound (if(induction_var >= lower_bound).
   * @param xcodeml Current XcodeML program
   * @param g       The group of do statements.
   */
  private void createIfStatementForLowerBound(XcodeProgram xcodeml,
                                              LoopHoistDoStmtGroup g)
  {
    int nestedDepth = g.getDoStmts().length;
    Xnode ifStmt = new Xnode(Xcode.FIFSTATEMENT, xcodeml);
    Xnode condition = new Xnode(Xcode.CONDITION, xcodeml);
    Xnode thenBlock = new Xnode(Xcode.THEN, xcodeml);
    XnodeUtil.copyEnhancedInfo(g.getDoStmts()[0], ifStmt);
    Xnode cond = new Xnode(Xcode.LOGGEEXPR, xcodeml);
    Xnode inductionVar = XnodeUtil.find(Xcode.VAR, g.getDoStmts()[0], false);
    cond.appendToChildren(inductionVar, true);
    cond.appendToChildren(g.getDoStmts()[0].findNode(Xcode.INDEXRANGE).findNode(Xcode.LOWERBOUND).getChild(0), true);
    ifStmt.appendToChildren(condition, false);
    ifStmt.appendToChildren(thenBlock, false);
    condition.appendToChildren(cond, false);
    thenBlock.appendToChildren(g.getDoStmts()[nestedDepth-1].getBody(), true);
    g.getDoStmts()[nestedDepth-1].getBody().delete();
    Xnode body = new Xnode(Xcode.BODY, xcodeml);
    body.appendToChildren(ifStmt, false);
    g.getDoStmts()[nestedDepth-1].appendToChildren(body, false);
  }


  /**
   * Relocated nested do statement inside a group of do statement.
   * @param g        The group of do statement.
   * @param newStart The new outer do statement.
   * @throws IllegalTransformationException If the nested group doesn't match the
   * correct size.
   */
  private void reloadDoStmts(LoopHoistDoStmtGroup g, Xnode newStart)
      throws IllegalTransformationException
  {
    g.getDoStmts()[0] = newStart;
    for(int j = 1; j < g.getDoStmts().length; ++j){
      Xnode next = XnodeUtil.find(Xcode.FDOSTATEMENT,
          g.getDoStmts()[j-1].getBody(), false);
      if(next == null){
        throw new IllegalTransformationException(
            "Unable to find enough nested do statements",
            _startClaw.getPragma().getLineNo()
        );
      }
      g.getDoStmts()[j] = next;
    }
  }
}

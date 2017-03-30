/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawReshapeInfo;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.transformation.ClawBlockTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
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
public class LoopHoist extends ClawBlockTransformation {

  private final List<LoopHoistDoStmtGroup> _doGroup;
  private int _nestedLevel;

  /**
   * Constructs a new LoopHoist triggered from a specific directive.
   *
   * @param startDirective The directive that triggered the loop hoist
   *                       transformation.
   * @param endDirective   The directive that end the structured block.
   */
  public LoopHoist(ClawLanguage startDirective, ClawLanguage endDirective) {
    super(startDirective, endDirective);
    _doGroup = new ArrayList<>();
  }

  /**
   * @see Transformation#analyze(XcodeProgram, Transformer)
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    int _pragmaDepthLevel = _clawStart.getPragma().depth();
    _nestedLevel = _clawStart.getHoistInductionVars().size();

    // Find all the group of nested loops that can be part of the hoisting
    List<Xnode> statements =
        XnodeUtil.findDoStatement(_clawStart.getPragma(),
            _clawEnd.getPragma(), _clawStart.getHoistInductionVars());

    if(statements.size() == 0) {
      xcodeml.addError("No do statement group meets the criteria of hoisting.",
          _clawStart.getPragma().lineNo());
      return false;
    }

    for(int i = 0; i < statements.size(); i++) {
      Xnode[] group = new Xnode[_nestedLevel];
      LoopHoistDoStmtGroup g = new LoopHoistDoStmtGroup(group);
      try {
        reloadDoStmts(g, statements.get(i));
      } catch(IllegalTransformationException e) {
        xcodeml.addError("Group " + i + " of do statements do not meet the" +
                " criteria of loop hoisting (Group index starts at 0).",
            _clawStart.getPragma().lineNo());
        return false;
      }

      LoopHoistDoStmtGroup crtGroup = new LoopHoistDoStmtGroup(group);
      int depth = group[0].depth();
      if(depth != _pragmaDepthLevel) {
        Xnode tmpIf = group[0].matchAncestor(Xcode.FIFSTATEMENT);
        Xnode tmpSelect = group[0].matchAncestor(Xcode.FSELECTCASESTATEMENT);
        Xnode tmpDo = group[0].matchAncestor(Xcode.FDOSTATEMENT);
        if(tmpIf == null && tmpSelect == null && tmpDo == null) {
          xcodeml.addError("Group " + i + " is nested in an unsupported " +
                  "statement for loop hoisting (Group index starts at 0).",
              _clawStart.getPragma().lineNo());
          return false;
        }

        int ifDepth =
            (tmpIf != null) ? tmpIf.depth() : Xnode.UNDEF_DEPTH;
        int selectDepth =
            (tmpSelect != null) ? tmpSelect.depth() : Xnode.UNDEF_DEPTH;
        int doDepth =
            (tmpDo != null) ? tmpDo.depth() : Xnode.UNDEF_DEPTH;

        if((_pragmaDepthLevel <= ifDepth || _pragmaDepthLevel <= selectDepth
            || _pragmaDepthLevel <= doDepth)
            && (ifDepth < depth || selectDepth < depth || doDepth < depth))
        {
          crtGroup.setExtraction();
        } else {
          xcodeml.addError("Group " + i + " is nested in an unsupported " +
                  "statement for loop hoisting or depth is too high " +
                  "(Group index starts at 0).",
              _clawStart.getPragma().lineNo());
          return false;
        }
      }
      _doGroup.add(crtGroup);
    }

    LoopHoistDoStmtGroup master = _doGroup.get(0);
    for(int i = 1; i < _doGroup.size(); ++i) {
      LoopHoistDoStmtGroup next = _doGroup.get(i);
      for(int j = 0; j < master.getDoStmts().length; ++j) {
        // Iteration range are identical, just merge
        if(j == 0
            && (
            !XnodeUtil.hasSameIndexRange(master.getDoStmts()[j],
                next.getDoStmts()[j]) &&
                XnodeUtil.hasSameIndexRangeBesidesLower(master.getDoStmts()[j],
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
              _clawStart.getPragma().lineNo());
          return false;
        }
      }
    }


    // Check reshape mandatory points
    if(_clawStart.hasReshapeClause()) {
      XfunctionDefinition fctDef =
          XnodeUtil.findParentFunction(_clawStart.getPragma());
      if(fctDef == null) {
        xcodeml.addError("Unable to matchSeq the function/subroutine/module " +
                "definition including the current directive",
            _clawStart.getPragma().lineNo()
        );
        return false;
      }

      for(ClawReshapeInfo r : _clawStart.getReshapeClauseValues()) {
        if(!fctDef.getSymbolTable().contains(r.getArrayName()) ||
            !fctDef.getDeclarationTable().contains(r.getArrayName()))
        {
          // Check in the parent def if present
          if(!checkUpperDefinition(fctDef, r.getArrayName())) {
            xcodeml.addError(String.format("Reshape variable %s not found in " +
                    "the definition of %s", r.getArrayName(),
                fctDef.getName().value()), _clawStart.getPragma().lineNo()
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
   *
   * @param fctDef Current function definition.
   * @param name   Id to be looked for.
   * @return True if the id has been found. False otherwise.
   */
  private boolean checkUpperDefinition(XfunctionDefinition fctDef, String name)
  {
    XfunctionDefinition upperDef = XnodeUtil.findParentFunction(fctDef);
    return upperDef != null
        && (!(!upperDef.getSymbolTable().contains(name)
        || !upperDef.getDeclarationTable().contains(name))
        || checkUpperDefinition(upperDef, name)
    );
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    return false;
  }

  /**
   * @see Transformation#transform(XcodeProgram, Transformer, Transformation)
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation) throws Exception
  {

    // Perform IF extraction and IF creation for lower-bound
    for(LoopHoistDoStmtGroup g : _doGroup) {
      if(g.needIfStatement()) {
        createIfStatementForLowerBound(xcodeml, g);
      }
      XnodeUtil.extractBody(g.getDoStmts()[_nestedLevel - 1], g.getDoStmts()[0]);
      g.getDoStmts()[0].delete();
    }

    // Do the hoisting
    LoopHoistDoStmtGroup hoisted = _doGroup.get(0).cloneObjectAndElement();
    hoisted.getDoStmts()[_nestedLevel - 1].body().delete();
    Xnode newBody = new Xnode(Xcode.BODY, xcodeml);
    hoisted.getDoStmts()[_nestedLevel - 1].append(newBody, false);
    XnodeUtil.shiftStatementsInBody(_clawStart.getPragma(),
        _clawEnd.getPragma(), newBody);
    _clawStart.getPragma().insertAfter(hoisted.getDoStmts()[0]);

    // Generate dynamic transformation (interchange)
    TransformationHelper.generateAdditionalTransformation(_clawStart,
        xcodeml, transformer, hoisted.getDoStmts()[0]);

    // Apply reshape clause
    TransformationHelper.applyReshapeClause(_clawStart, xcodeml);

    // Delete pragmas
    _clawStart.getPragma().delete();
    _clawEnd.getPragma().delete();
  }

  /**
   * Create an IF statement surrounding the entire most inner do statement body.
   * Condition if made from the lower bound (if(induction_var >= lower_bound).
   *
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
    Xnode inductionVar = g.getDoStmts()[0].matchDirectDescendant(Xcode.VAR);
    cond.append(inductionVar, true);
    cond.append(g.getDoStmts()[0].matchDirectDescendant(Xcode.INDEXRANGE).
        matchDirectDescendant(Xcode.LOWERBOUND).child(0), true
    );
    ifStmt.append(condition, false);
    ifStmt.append(thenBlock, false);
    condition.append(cond, false);
    thenBlock.append(g.getDoStmts()[nestedDepth - 1].body(), true);
    g.getDoStmts()[nestedDepth - 1].body().delete();
    Xnode body = new Xnode(Xcode.BODY, xcodeml);
    body.append(ifStmt, false);
    g.getDoStmts()[nestedDepth - 1].append(body, false);
  }


  /**
   * Relocated nested do statement inside a group of do statement.
   *
   * @param g        The group of do statement.
   * @param newStart The new outer do statement.
   * @throws IllegalTransformationException If the nested group doesn't match
   *                                        the correct size.
   */
  private void reloadDoStmts(LoopHoistDoStmtGroup g, Xnode newStart)
      throws IllegalTransformationException
  {
    g.getDoStmts()[0] = newStart;
    for(int j = 1; j < g.getDoStmts().length; ++j) {
      Xnode next = g.getDoStmts()[j - 1].body().
          matchDirectDescendant(Xcode.FDOSTATEMENT);
      if(next == null) {
        throw new IllegalTransformationException(
            "Unable to matchSeq enough nested do statements",
            _clawStart.getPragma().lineNo()
        );
      }
      g.getDoStmts()[j] = next;
    }
  }
}

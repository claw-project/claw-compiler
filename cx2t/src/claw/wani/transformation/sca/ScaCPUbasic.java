/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Utility;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Condition;
import claw.tatsu.primitive.Field;
import claw.tatsu.primitive.Function;
import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawPragma;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 * Specific SCA transformation for CPU target. Naive algorithm is used here.
 *
 * Transformation for the CPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Propagated promotion is applied to all scalars or arrays used in an
 * assign statement at the lhs and where a promoted variable is used on the
 * rhs.
 * <li> Do statements over the additional dimensions are added as an inner
 * loop wrapping each assign statements including promoted variables.
 * </ul>
 *
 * @author clementval
 */
public class ScaCPUbasic extends Sca {

  /**
   * Constructs a new SCA transformation triggered from a specific
   * pragma for a CPU target.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaCPUbasic(ClawPragma directive) {
    super(directive);
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Apply the common transformation
    super.transform(xcodeml, translator, other);

    // Apply specific steps for CPU
    applySpecificTransformation(xcodeml);

    // Finalize the common steps
    super.finalizeTransformation(xcodeml);
  }

  /**
   * Apply specific step of the transformation for a CPU target with naive DO
   * statement generation.
   *
   * @param xcodeml Current translation unit.
   * @throws IllegalTransformationException If any transformation fails.
   */
  private void applySpecificTransformation(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    /* Create a group of nested loop with the newly defined dimension and wrap
     * every assignment statement in the column loop or including data with it.
     * This is for the moment a really naive transformation idea but it is our
     * start point.
     * Use the first over clause to do it. */
    List<AssignStatement> assignStatements =
        Function.gatherAssignStatements(_fctDef);

    detectIndirectPromotion(xcodeml, assignStatements);

    // Hold hooks on which do statement will be inserted
    Set<Xnode> hooks = new HashSet<>();

    /* Check whether condition includes a promoted variables. If so, the
     * ancestor node using the condition must be wrapped in a do statement. */
    List<Xnode> conditions = _fctDef.body().matchAll(Xcode.CONDITION);
    for(Xnode condition : conditions) {
      if(Condition.dependsOn(condition, _arrayFieldsInOut)) {
        Xnode ancestor = condition.ancestor();
        Iterator<Xnode> iter = hooks.iterator();
        boolean addHook = true;
        while(iter.hasNext()) {
          if(ancestor.isNestedIn(iter.next())) {
            addHook = false;
            break;
          }
        }
        if(addHook) {
          hooks.add(ancestor);
        }
      }
    }

    flagDoStatementLocation(assignStatements, hooks);

    // Generate loops around statements flagged in previous stage
    generateDoStatements(xcodeml, hooks);

    // Generate the parallel region
    Directive.generateParallelClause(xcodeml,
        _fctDef.body().firstChild(), _fctDef.body().lastChild());
  }

  private void flagDoStatementLocation(List<AssignStatement> assignStatements,
                                       Set<Xnode> hooks)
  {
    /* Iterate a second time over assign statements to flag places where to
     * insert the do statements */
    for(AssignStatement assign : assignStatements) {
      Xnode lhs = assign.getLhs();
      String lhsName = assign.getLhsName();
      boolean wrapInDoStatement = true;

      // Check if assignment is dependant of an if statement.
      if(assign.isChildOf(Xcode.F_IF_STATEMENT)) {

        // Gather all potential ancestor if statements
        List<Xnode> ifStatements = assign.matchAllAncestor(Xcode.F_IF_STATEMENT,
            Xcode.F_FUNCTION_DEFINITION);

        Xnode hookIfStmt = null;
        for(Xnode ifStmt : ifStatements) {
          if(Condition.dependsOn(
              ifStmt.matchDirectDescendant(Xcode.CONDITION),
              assign.getVarRefNames()))
          {
            // Have to put the do statement around the if as the assignment
            // is conditional as well.
            hookIfStmt = ifStmt;
          }
        }

        if(hookIfStmt != null) {
          wrapInDoStatement = false;
          boolean addIfHook = true;

          // Get rid of previously flagged hook in this if body.
          Iterator<Xnode> iter = hooks.iterator();
          while(iter.hasNext()) {
            Xnode crt = iter.next();
            if(assign.isNestedIn(crt) || hookIfStmt.isNestedIn(crt)) {
              addIfHook = false;
            }
            if(crt.isNestedIn(hookIfStmt)) {
              iter.remove();
            }
          }

          if(addIfHook) {
            hooks.add(hookIfStmt);
          }
        }
      }

      for(Xnode hook : hooks) {
        if(assign.isNestedIn(hook)) {
          wrapInDoStatement = false;
          break;
        }
      }

      if(lhs.opcode() == Xcode.F_ARRAY_REF
          && _arrayFieldsInOut.contains(lhsName) && wrapInDoStatement)
      {
        hooks.add(assign);
      } else if((lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.F_ARRAY_REF
          && _scalarFields.contains(lhsName)) &&
          (shouldBePromoted(assign) && wrapInDoStatement))
      {
        hooks.add(assign);
      }
    }
  }

  /**
   * Iterate over all assign statements to detect all indirect promotion. Apply
   * correct promotion if detected.
   *
   * @param xcodeml          Current translation unit.
   * @param assignStatements List of assignment statements
   * @throws IllegalTransformationException If promotion failed.
   */
  private void detectIndirectPromotion(XcodeProgram xcodeml,
                                       List<AssignStatement> assignStatements)
      throws IllegalTransformationException
  {
    for(AssignStatement assign : assignStatements) {
      Xnode lhs = assign.getLhs();
      if(lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.F_ARRAY_REF) {
        /* If the assignment is in the column loop and is composed with some
         * promoted variables, the field must be promoted and the var reference
         * switch to an array reference */
        promoteIfNeeded(xcodeml, assign);
      }
    }
  }

  private void promoteIfNeeded(XcodeProgram xcodeml, AssignStatement assign)
      throws IllegalTransformationException
  {
    if(shouldBePromoted(assign)) {
      PromotionInfo promotionInfo;
      Xnode lhs = assign.getLhs();
      String lhsName = assign.getLhsName();

      // Do the promotion if needed
      if(!_arrayFieldsInOut.contains(lhsName)) {
        _arrayFieldsInOut.add(lhsName);
        promotionInfo =
            new PromotionInfo(lhsName, _claw.getLayoutForData(lhsName));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(lhsName, promotionInfo);
      } else {
        promotionInfo = _promotions.get(lhsName);
      }
      // Adapt references
      if(lhs.opcode() == Xcode.VAR) {
        Field.adaptScalarRefToArrayRef(lhsName, _fctDef,
            _claw.getDefaultLayout(), xcodeml);
      } else {
        Field.adaptArrayRef(_promotions.get(lhsName), _fctDef.body(), xcodeml);
        Field.adaptAllocate(_promotions.get(lhsName), _fctDef.body(), xcodeml);
      }
      promotionInfo.setRefAdapted();
    }
  }

  /**
   * Generate new DO statement at flagged location.
   *
   * @param xcodeml Current translation unit.
   * @param hooks   Set of flagged location.
   */
  private void generateDoStatements(XcodeProgram xcodeml, Set<Xnode> hooks) {
    for(Xnode hook : hooks) {
      NestedDoStatement loops =
          new NestedDoStatement(_claw.getDefaultLayoutReversed(), xcodeml);
      hook.insertAfter(loops.getOuterStatement());
      loops.getInnerStatement().body().append(hook, true);
      hook.delete();
      Directive.generateLoopDirectives(xcodeml,
          loops.getOuterStatement(), loops.getOuterStatement(),
          Directive.NO_COLLAPSE);
    }
  }

  /**
   * Check whether the LHS variable should be promoted.
   *
   * @param assignStmt Assign statement node.
   * @return True if the LHS variable should be promoted. False otherwise.
   */
  private boolean shouldBePromoted(Xnode assignStmt) {
    Xnode rhs = assignStmt.child(Xnode.RHS);
    if(rhs == null) {
      return false;
    }
    List<Xnode> vars = XnodeUtil.findAllReferences(rhs);
    Set<String> names = XnodeUtil.getNamesFromReferences(vars);
    return Utility.hasIntersection(names, _arrayFieldsInOut);
  }

}

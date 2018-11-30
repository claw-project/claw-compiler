/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Message;
import claw.tatsu.common.Utility;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Body;
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
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.wani.language.ClawPragma;

import java.util.*;

/**
 * Single Column Abstraction (SCA) CPU target transformation. This
 * transformation has two modes:
 * - single: single statements are wrapped in do statements
 * - fusion: merge adjacent statements together to maximize vectorization.
 *
 * @author clementval
 */
public class ScaCPUvectorizeGroup extends Sca {

  private final boolean _fusion;
  private final Set<String> _temporaryFields = new HashSet<>();

  /**
   * Constructs a new SCA transformation triggered from a specific
   * pragma for a CPU target.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaCPUvectorizeGroup(ClawPragma directive) {
    super(directive);
    _fusion = false;
  }

  public ScaCPUvectorizeGroup(ClawPragma directive, boolean fusion) {
    super(directive);
    _fusion = fusion;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Apply the common transformation
    super.transform(xcodeml, translator, other);

    // Apply specific steps for CPU smart fusion
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

    detectIndirectPromotion(assignStatements);

    Set<VectorBlock> blocks = flagIfStatementWithPromotion();

    flagDoStatementLocation(assignStatements, blocks);

    List<VectorBlock> fusionBlocks =
        (_fusion) ? mergeVectorBlocks(blocks) : new ArrayList<>(blocks);

    checkMissingPromotion(fusionBlocks);
    if(_fusion) {
      Set<String> noPromotion = controlPromotion(fusionBlocks);
      _temporaryFields.removeAll(noPromotion);
    }

    for(String temporary : _temporaryFields) {
      promote(xcodeml, temporary);
    }

    // Generate loops around statements flagged in previous stage
    generateDoStatements(xcodeml, fusionBlocks);

    // Generate the parallel region
    Directive.generateParallelClause(xcodeml,
        _fctDef.body().firstChild(), _fctDef.body().lastChild());
  }

  /**
   * Check temporary variables flagged for promotion to see if this promotion
   * is still needed.
   *
   * @param blocks List of blocks.
   * @return Set of variables where promotion is not needed anymore.
   */
  private Set<String> controlPromotion(List<VectorBlock> blocks) {

    Set<String> noPromotionNeeded = new HashSet<>();
    for(String var : _temporaryFields) {
      int usedInBlock = 0;
      for(VectorBlock block : blocks) {
        if(block.getReadAndWrittentVariables().contains(var)) {
          ++usedInBlock;
          if(usedInBlock > 1) {
            break;
          }
        }
      }
      if(usedInBlock <= 1 && _arrayFieldsInOut.contains(var)) {
        noPromotionNeeded.add(var);
      }
    }

    return noPromotionNeeded;
  }

  /**
   * Check for potential missing promotions of scalar fields.
   *
   * @param blocks List of blocks.
   */
  private void checkMissingPromotion(List<VectorBlock> blocks) {

    for(String var : _scalarFields) {
      // If shared by multiple block and not promoted yet.
      if(isVarSharedByMultipleBlocks(blocks, var) && isVarNotOnlyConstant(var)
          && isVarWrittenInBlocks(blocks, var)
          && !_arrayFieldsInOut.contains(var)
          && !_inductionVariables.contains(var)
          && !_noPromotion.contains(var))
      {
        Message.debug("SCA: Promotion might be missing for: " + var);
        // TODO decide to promote?
      }

    }
  }

  /**
   * Check is the variable is not only updated by constant values.
   *
   * @param var Variable to check.
   * @return True if the variable is written with none constant value. False
   * otherwise.
   */
  private boolean isVarNotOnlyConstant(String var) {
    List<AssignStatement> assignStatements =
        Function.gatherAssignStatements(_fctDef);
    for(AssignStatement as : assignStatements) {
      if(as.getLhsName().equals(var) && !as.isContantAssignment()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Check is variable is shared by at two or more blocks.
   *
   * @param blocks List of blocks to check.
   * @param var    Variable name to check.
   * @return True if the variable is shared by at least two blocks.
   * False otherwise.
   */
  private boolean isVarSharedByMultipleBlocks(List<VectorBlock> blocks,
                                              String var)
  {
    int nbBlockSharingVariable = 0;
    // Check if there is more than one block using the variable.
    for(VectorBlock block : blocks) {
      if(block.getReadAndWrittentVariables().contains(var)) {
        ++nbBlockSharingVariable;
        if(nbBlockSharingVariable > 1) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Check if the variable is written in any blocks.
   *
   * @param blocks List of blocks to check.
   * @param var    Variable name to check.
   * @return True if the variable is written in any blocks.
   */
  private boolean isVarWrittenInBlocks(List<VectorBlock> blocks, String var) {
    for(VectorBlock block : blocks) {
      if(block.getWrittenVariables().contains(var)) {
        return true;
      }
    }
    return false;
  }

  /**
   * Merge adjacent block together to maximize vectorization and data locality.
   *
   * @param blocks Set of flagged blocks containing a single statement.
   * @return List of merged blocks.
   */
  private List<VectorBlock> mergeVectorBlocks(Set<VectorBlock> blocks) {

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
  private List<VectorBlock> sortBlockByLineOrder(Set<VectorBlock> blocks) {
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

  /**
   * Check whether condition includes a promoted variables. If so, the
   * ancestor node using the condition must be wrapped in a do statement.
   *
   * @return Set of vector block for flagged location.
   */
  private Set<VectorBlock> flagIfStatementWithPromotion() {
    Set<VectorBlock> blocks = new HashSet<>();
    /*  */
    List<Xnode> conditions = _fctDef.body().matchAll(Xcode.CONDITION);
    for(Xnode condition : conditions) {
      if(Condition.dependsOn(condition, _arrayFieldsInOut)
          && !Condition.isAllocationRelated(condition))
      {
        Xnode ancestor = condition.ancestor();
        Iterator<VectorBlock> iter = blocks.iterator();
        boolean addHook = true;
        while(iter.hasNext()) {
          if(ancestor.isNestedIn(iter.next().getStartStmt())) {
            addHook = false;
            break;
          }
        }
        if(addHook) {
          blocks.add(new VectorBlock(ancestor));
        }
      }
    }
    return blocks;
  }

  /**
   * Go through all assignments and flag all location where a do statement
   * should be inserted after variables promotion.
   *
   * @param assignStatements List of assignments.
   * @param blocks           Set of vector blocks.
   */
  private void flagDoStatementLocation(List<AssignStatement> assignStatements,
                                       Set<VectorBlock> blocks)
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

        Set<String> assignVars = assign.getVarNames();
        assignVars.retainAll(_arrayFieldsInOut);

        for(Xnode ifStmt : ifStatements) {
          if(Condition.dependsOn(
              ifStmt.matchDirectDescendant(Xcode.CONDITION), assignVars))
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
          Iterator<VectorBlock> iter = blocks.iterator();
          while(iter.hasNext()) {
            Xnode crt = iter.next().getStartStmt();
            if(assign.isNestedIn(crt) || hookIfStmt.isNestedIn(crt)) {
              addIfHook = false;
            }
            if(crt.isNestedIn(hookIfStmt)) {
              iter.remove();
            }
          }

          if(addIfHook) {
            blocks.add(new VectorBlock(hookIfStmt));
          }
        }
      }

      for(VectorBlock hook : blocks) {
        if(assign.isNestedIn(hook.getStartStmt())) {
          wrapInDoStatement = false;
          break;
        }
      }

      if(((lhs.opcode() == Xcode.F_ARRAY_REF || lhs.opcode() == Xcode.VAR)
          && _arrayFieldsInOut.contains(lhsName) && wrapInDoStatement) ||
          ((lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.F_ARRAY_REF
              && _scalarFields.contains(lhsName)) &&
              (shouldBePromoted(assign) && wrapInDoStatement)))
      {
        blocks.add(new VectorBlock(assign));
      }
    }
  }

  /**
   * Iterate over all assign statements to detect all indirect promotion. Apply
   * correct promotion if detected.
   *
   * @param assignStatements List of assignment statements
   */
  private void detectIndirectPromotion(List<AssignStatement> assignStatements) {
    /* If the assignment is in the column loop and is composed with some
     * promoted variables, the field must be promoted and the var reference
     * switch to an array reference */
    for(AssignStatement assign : assignStatements) {
      Xnode lhs = assign.getLhs();
      if((lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.F_ARRAY_REF)
          && !_noPromotion.contains(assign.getLhsName())
          && !_inductionVariables.contains(assign.getLhsName())
          && !_arrayFieldsInOut.contains(assign.getLhsName())
          && shouldBePromoted(assign))
      {
        _arrayFieldsInOut.add(assign.getLhsName());
        _temporaryFields.add(assign.getLhsName());
      }
    }
  }

  /**
   * Promote the given variable and adapt references.
   *
   * @param xcodeml Current translation unit.
   * @param lhsName Variable name to be promoted.
   * @throws IllegalTransformationException If promotion cannot be done.
   */
  private void promote(XcodeProgram xcodeml, String lhsName)
      throws IllegalTransformationException
  {
    PromotionInfo promotionInfo;
    // Do the promotion if needed
    if(!_promotions.containsKey(lhsName)) {
      promotionInfo =
          new PromotionInfo(lhsName, _claw.getLayoutForData(lhsName));
      Field.promote(promotionInfo, _fctDef, xcodeml);
      _promotions.put(lhsName, promotionInfo);
    } else {
      promotionInfo = _promotions.get(lhsName);
    }
    // Adapt references
    Xid id = _fctDef.getSymbolTable().get(lhsName);
    FbasicType bType = xcodeml.getTypeTable().getBasicType(id);
    if(!bType.isArray()) {
      Field.adaptScalarRefToArrayRef(_promotions.get(lhsName), _fctDef,
          _claw.getDefaultLayout(), xcodeml);
    } else {
      Field.adaptArrayRef(_promotions.get(lhsName), _fctDef.body(), xcodeml);
      Field.adaptAllocate(_promotions.get(lhsName), _fctDef.body(), xcodeml);
    }
    promotionInfo.setRefAdapted();
  }

  /**
   * Generate new DO statement at flagged location.
   *
   * @param xcodeml Current translation unit.
   * @param blocks  List of vectorization friendly blocks.
   */
  private void generateDoStatements(XcodeProgram xcodeml,
                                    List<VectorBlock> blocks)
      throws IllegalTransformationException
  {
    for(VectorBlock block : blocks) {
      NestedDoStatement loops =
          new NestedDoStatement(_claw.getDefaultLayoutReversed(), xcodeml);

      if(block.isSingleStatement()) {
        block.getStartStmt().insertAfter(loops.getOuterStatement());
        loops.getInnerStatement().body().append(block.getStartStmt(), true);
        block.getStartStmt().delete();
      } else {
        block.getEndStmt().insertAfter(loops.getOuterStatement());
        Body.shiftIn(block.getStartStmt(), block.getEndStmt(),
            loops.getInnerStatement().body(), true);
      }

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

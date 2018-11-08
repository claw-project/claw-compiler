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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Specific transformation for CPU target with smart fusion algorithm to group
 * statements.
 *
 * @author clementval
 */
public class ScaCPUsmartFusion extends Sca {

  /**
   * Constructs a new SCA transformation triggered from a specific
   * pragma for a CPU target.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaCPUsmartFusion(ClawPragma directive) {
    super(directive);
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
   * Apply specific step of the transformation for a CPU target with smart
   * fusion DO statement generation.
   *
   * @param xcodeml Current translation unit.
   * @throws IllegalTransformationException If any transformation fails.
   */
  private void applySpecificTransformation(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {

    // Apply transformation only when there is a body to modify
    if(_fctDef.hasEmptyBody()) {
      return;
    }
    // Variables on each "depth" of the method
    List<Set<String>> depthVars = new ArrayList<>();
    depthVars.add(new HashSet<String>());
    /* A growing list of affecting variables. Starting from the ones
     * declared by SCA and successively any variables affected by the SCA
     * declaration */
    Set<String> affectingVars = new HashSet<>(_promotions.keySet());

    /* Extract all the variables used, this doesn't include vector's
     * iterator variables */
    extractInfoSmartFusion(_fctDef.body(), new AtomicInteger(), depthVars,
        affectingVars);

    // Find the block we need to transform
    List<Set<String>> targetDepthIntersections = new ArrayList<>();
    for(int i = 0; i < depthVars.size(); i++) {
      Set<String> depthVar = depthVars.get(i);

      if(depthVar.isEmpty()) {
        targetDepthIntersections.add(null);
        continue;
      }
      // Find intersection with other blocks
      Set<String> intersection = new HashSet<>();
      for(int j = 0; j < depthVars.size(); j++) {
        // Skip itself
        if(i == j) {
          continue;
        }
        // Intersect the level set with the others
        Set<String> copy = new HashSet<>(depthVars.get(i));
        copy.retainAll(depthVars.get(j));
        intersection.addAll(copy);
      }

      // All are on the same index
      targetDepthIntersections.add(intersection);
    }

    // Gather all possible block of nodes before their modification
    List<List<Xnode>> transformations = new ArrayList<>();
    // Apply transformation at the indicated depth
    for(int targetDepth = 0; targetDepth < depthVars.size(); targetDepth++) {
      Set<String> vars = depthVars.get(targetDepth);
      if(vars.isEmpty()) {
        continue;
      }
      Set<String> intersection = targetDepthIntersections.get(targetDepth);

      // Variables still waiting for a promotion which aren't promoted yet
      Set<String> promotions = new HashSet<>(intersection);

      // Check only the LHS arrays
      Set<String> vectorType = new HashSet<>(vars);
      vectorType.retainAll(affectingVars);
      // Influenced arrays must be promoted anyway
      for(String var : vectorType) {
        Xid fieldId = _fctDef.getSymbolTable().get(var);
        FbasicType type = xcodeml.getTypeTable().getBasicType(fieldId);
        if(type != null && type.isArray()) {
          promotions.add(var);
        }
      }

      // Promote
      for(String promotion : promotions) {
        if(_promotions.containsKey(promotion)) {
          continue;
        }
        PromotionInfo promotionInfo = new PromotionInfo(promotion,
            _claw.getLayoutForData(promotion));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(promotion, promotionInfo);
        Field.adaptArrayRef(promotionInfo, _fctDef.body(), xcodeml);
        Field.adaptAllocate(_promotions.get(promotion), _fctDef.body(),
            xcodeml);
      }

      // Transform indicated level by wrapping it in a DO loop
      gatherBlocks(_fctDef.body(), new AtomicInteger(), targetDepth,
          affectingVars, transformations);
    }

    // Avoid double DO statement generation in nested groups
    cleanupBlocks(transformations);

    // After gathering we apply the transformations
    for(List<Xnode> nodeBlock : transformations) {
      wrapGroupInDoStatement(nodeBlock, xcodeml);
    }

    // If the last statement is a CONTAINS, fallback to the previous one
    Xnode lastNode = _fctDef.body().lastChild();
    while(lastNode.opcode() == Xcode.F_CONTAINS_STATEMENT) {
      lastNode = lastNode.prevSibling();
    }
    // Generate the parallel region
    Directive.generateParallelClause(xcodeml,
        _fctDef.body().firstChild(), lastNode);
  }

  /**
   * Check that flagged block to be wrapped in DO statements are not nested in
   * each other. If so, the nested block is removed from the flagged list.
   *
   * @param blocks List of block of nodes flagged to be wrapped with DO
   *               statement.
   */
  private void cleanupBlocks(List<List<Xnode>> blocks) {
    List<List<Xnode>> flaggedAsNested = new ArrayList<>();

    for(List<Xnode> block : blocks) {
      if(block.isEmpty()) {
        continue;
      }
      if(isNestedIn(blocks, block) || isContainedWithin(blocks, block)) {
        flaggedAsNested.add(block);
      }
    }

    for(List<Xnode> flagged : flaggedAsNested) {
      blocks.remove(flagged);
    }
  }

  private boolean isNestedIn(List<List<Xnode>> blocks, List<Xnode> block) {
    Xnode probe = block.get(0);
    for(List<Xnode> nest : blocks) {
      if(block != nest) {
        for(Xnode possibleAncestor : nest) {
          if(probe.isNestedIn(possibleAncestor)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Check if the given block of nodes has any nested statements inside one of
   * the other block. The check is done with line numbers of 1st and last
   * statements as those elements are not nested in the AST yet.
   *
   * @param blocks List of block of nodes.
   * @param block  The block to be checked for nesting.
   * @return True if the block is nested in another block.
   */
  private boolean isContainedWithin(List<List<Xnode>> blocks, List<Xnode> block)
  {
    for(List<Xnode> nest : blocks) {
      if(block != nest && (block.get(0).lineNo() >= nest.get(0).lineNo())
          && (block.get(block.size() - 1).lineNo()
          <= nest.get(nest.size() - 1).lineNo()))
      {
        return true;
      }
    }
    return false;
  }

  /**
   * Analysis of a routine in order to extracts the information necessary in
   * order to promote a minimal number of variables.
   * Only variables affected by affectingVars are kept in depthVars.
   *
   * @param body          The body to analyze.
   * @param depth         The depth relative to the function declaration.
   * @param depthVars     Vars used on each depth.
   * @param affectingVars Vars which are affected by SCA and consequently
   *                      affect other variables.
   */
  private void extractInfoSmartFusion(Xnode body, AtomicInteger depth,
                                      List<Set<String>> depthVars,
                                      Set<String> affectingVars)
  {
    final List<Xnode> children = body.children();
    for(Xnode node : children) {
      // Handle an assignment
      if(node.opcode() == Xcode.F_ASSIGN_STATEMENT) {
        AssignStatement as = new AssignStatement(node.element());
        Set<String> vars = XnodeUtil.findChildrenVariables(node);

        // Check if it's affected by the promotion
        Set<String> affectedVars = new HashSet<>(vars);
        affectedVars.retainAll(affectingVars);

        // If the intersection of the sets contain anything the assignment is
        // affected by a previous promotions
        depthVars.get(depth.get()).addAll(affectedVars);
        if(!affectedVars.isEmpty()) {
          affectingVars.add(as.getLhsName());
          depthVars.get(depth.get()).add(as.getLhsName());
        }
      }
      // IF statement content shouldn't increase depth counter
      else if(node.opcode() == Xcode.F_IF_STATEMENT) {
        Xnode nThen = node.firstChild().matchSibling(Xcode.THEN);
        Xnode nElse = node.firstChild().matchSibling(Xcode.ELSE);
        if(nThen != null) {
          extractInfoSmartFusion(nThen.body(), depth, depthVars,
              affectingVars);
        }
        if(nElse != null) {
          extractInfoSmartFusion(nElse.body(), depth, depthVars,
              affectingVars);
        }
      }
      // Handle node containing a body
      else if(node.opcode().hasBody()) {
        if(depthVars.size() <= depth.get() + 1) {
          depthVars.add(new HashSet<String>());
        }
        depth.incrementAndGet();
        extractInfoSmartFusion(node.body(), depth, depthVars,
            affectingVars);
      }
      // Keep going inside the new node
      else {
        extractInfoSmartFusion(node, depth, depthVars,
            affectingVars);
      }
    }
  }

  /**
   * Transform the content of the routine and add a DO loop only at the
   * indicated depth and only around the affecting variables.
   *
   * @param body          The body to transform.
   * @param currentDepth  The depth we currently are at
   * @param targetDepth   The depth we need to reach and transform.
   * @param affectingVars The variable which should be contained inside the loop
   * @param blocks        List of block of nodes.
   */
  private void gatherBlocks(Xnode body, AtomicInteger currentDepth,
                            final int targetDepth, Set<String> affectingVars,
                            List<List<Xnode>> blocks)
  {
    final List<Xnode> children = body.children();
    final List<Xnode> nodeBlocks = new ArrayList<>();
    nodeLoop:
    for(Xnode node : children) {
      // Handle an assignment
      if(node.opcode() == Xcode.F_ASSIGN_STATEMENT) {
        if(currentDepth.get() != targetDepth) continue;
        Set<String> vars = XnodeUtil.findChildrenVariables(node);
        // Statement need to be in the loop
        if(Utility.hasIntersection(vars, affectingVars)) {
          // Is the statement wasn't promoted and is not in a current loop
          // block, we can avoid to add it to a loop because it is not needed.
          AssignStatement as = new AssignStatement(node.element());
          if(!_promotions.containsKey(as.getLhsName())
              && nodeBlocks.isEmpty() && as.getVarRefNames().isEmpty())
          {
            continue;
          }
          // If the assignment is a vector, but we don't access its elements
          // we don't have to add it to a loop
          if(_promotions.containsKey(as.getLhsName()) &&
              as.getVarRefNames().isEmpty())
          {
            addNodeBlock(nodeBlocks, blocks);
            continue;
          }
          nodeBlocks.add(node);
        }
        // Particular case, unused variable inside the body
        else {
          addNodeBlock(nodeBlocks, blocks);
        }
      }
      // IF statement my have to be contained inside
      else if(node.opcode() == Xcode.F_IF_STATEMENT) {
        // Enter the if in case it contains DO statements
        if(currentDepth.get() != targetDepth) {
          Xnode nThen = node.firstChild().matchSibling(Xcode.THEN);
          Xnode nElse = node.firstChild().matchSibling(Xcode.ELSE);
          if(nThen != null) {
            addNodeBlock(nodeBlocks, blocks);
            gatherBlocks(nThen.body(), currentDepth, targetDepth, affectingVars,
                blocks);
          }
          if(nElse != null) {
            addNodeBlock(nodeBlocks, blocks);
            gatherBlocks(nElse.body(), currentDepth, targetDepth, affectingVars,
                blocks);
          }
          continue;
        }

        // We need to wrap the whole IF based on the condition
        Xnode condNode = node.firstChild().matchSibling(Xcode.CONDITION);
        if(Condition.dependsOn(condNode, affectingVars)) {
          nodeBlocks.add(node);
          continue nodeLoop;
        }

        // We need to wrap the whole IF based on the statement inside
        List<Xnode> assignStatements = node.matchAll(Xcode.F_ASSIGN_STATEMENT);
        xNodeLoop:
        for(Xnode statement : assignStatements) {
          AssignStatement as = new AssignStatement(statement.element());
          if(affectingVars.contains(as.getLhsName())) {
            // If the affected node is child of a DO, a dedicated
            // block will be created.
            Xnode pnode = statement.ancestor();
            while(pnode.hashCode() != node.hashCode()) {
              if(pnode.opcode() == Xcode.F_DO_STATEMENT) {
                break xNodeLoop;
              }
              pnode = pnode.ancestor();
            }
            // Add the whole if and continue otherwise
            nodeBlocks.add(node);
            continue nodeLoop;
          }
        }

        // If the IF statement doesn't contains any dependency we gather the
        // potential transformation and continue
        addNodeBlock(nodeBlocks, blocks);
      } else if(node.opcode().hasBody()) { // Handle node containing a body
        addNodeBlock(nodeBlocks, blocks);
        currentDepth.incrementAndGet();
        gatherBlocks(node.body(), currentDepth, targetDepth, affectingVars,
            blocks);
      } else { // Keep going inside the new node
        addNodeBlock(nodeBlocks, blocks);
        gatherBlocks(node, currentDepth, targetDepth, affectingVars, blocks);
      }
    }
    addNodeBlock(nodeBlocks, blocks);
  }

  /**
   * If a block of nodes flagged to be wrapped is not empty, add it to the
   * collection containing all the blocks of nodes found until now.
   * The content of the block of nodes is copied into a new list added to
   * `blocks`.
   * The content of the `nodeBlock` is then cleared.
   * Used by CPU smart fusion transformation.
   *
   * @param nodeBlock A list of adjacent (block) nodes to be wrapped in a
   *                  DO statement.
   * @param blocks    List containing all block of nodes.
   */
  private void addNodeBlock(List<Xnode> nodeBlock,
                            List<List<Xnode>> blocks)
  {
    if(nodeBlock.isEmpty()) {
      return;
    }
    blocks.add(new ArrayList<>(nodeBlock));
    nodeBlock.clear();
  }

  /**
   * Wrap each block of nodes given in the list with appropriate DO statement.
   *
   * @param nodeBlock Block of nodes to be wrapped in a DO statement.
   * @param xcodeml   Current translation unit.
   */
  private void wrapGroupInDoStatement(List<Xnode> nodeBlock,
                                      XcodeProgram xcodeml)
  {
    if(nodeBlock.isEmpty()) {
      return;
    }

    // Create the DO statement
    NestedDoStatement loop =
        new NestedDoStatement(_claw.getDefaultLayoutReversed(), xcodeml);

    // Insert DO statement into the AST and add statements in its body
    nodeBlock.get(0).insertBefore(loop.getOuterStatement());
    for(Xnode node : nodeBlock) {
      loop.getInnerStatement().body().append(node, false);
    }

    Directive.generateLoopDirectives(xcodeml,
        loop.getOuterStatement(), loop.getOuterStatement(),
        Directive.NO_COLLAPSE);
    nodeBlock.clear();
  }
}

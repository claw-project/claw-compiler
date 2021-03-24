/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.*;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Body;
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
import claw.wani.x2t.translator.ClawTranslator;

import java.util.*;

/**
 * Single Column Abstraction (SCA) CPU target transformation. This
 * transformation has two modes: - single: single statements are wrapped in do
 * statements - fusion: merge adjacent statements together to maximize
 * vectorization.
 *
 * @author clementval
 */
public class ScaCPUvectorizeGroup extends Sca
{

    private final boolean _applyFusion;
    private final Set<String> _temporaryFieldsToPromote = new HashSet<>();

    /**
     * Constructs a new SCA transformation triggered from a specific pragma for a
     * CPU target.
     *
     * @param directive The directive that triggered the define transformation.
     */
    public ScaCPUvectorizeGroup(ClawPragma directive)
    {
        super(directive);
        _applyFusion = false;
    }

    /**
     * Constructs a new SCA transformation triggered from a specific pragma for a
     * CPU target.
     *
     * @param directive The directive that triggered the define transformation.
     * @param fusion    If true, the fusion algorithm to the transformation.
     */
    public ScaCPUvectorizeGroup(ClawPragma directive, boolean fusion)
    {
        super(directive);
        _applyFusion = fusion;
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        if (!detectParentFunction(xcodeml))
        {
            return false;
        }

        if (!_fctType.isElemental())
        { // Only for non-elemental function/subroutine
            ClawTranslator trans = (ClawTranslator) translator;
            detectInductionVariables();
            return analyzeDimension(xcodeml) && analyzeData(xcodeml, trans);
        }
        return true;
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception
    {
        // SCA in ELEMENTAL function has no effect for CPU target
        if (_fctType.isElemental())
        {
            xcodeml.addWarning("SCA in ELEMENTAL function has no impact for CPU target", _claw.getPragma());
            removePragma();
            return;
        }

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
    private void applySpecificTransformation(XcodeProgram xcodeml) throws IllegalTransformationException
    {
        /*
         * Create a group of nested loop with the newly defined dimension and wrap every
         * assignment statement in the column loop or including data with it. This is
         * for the moment a really naive transformation idea but it is our start point.
         * Use the first over clause to do it.
         */
        List<AssignStatement> assignStatements = _fctDef.gatherAssignStatements();

        detectIndirectPromotion(assignStatements);

        Set<VectorBlock> naiveBlocks = flagDoStatementLocation(assignStatements);

        List<VectorBlock> mergedBlocks = (_applyFusion) ? VectorBlock.mergeAdjacent(naiveBlocks)
                : new ArrayList<>(naiveBlocks);

        checkMissingPromotion(mergedBlocks);
        if (_applyFusion)
        {
            removeUselessPromotion(mergedBlocks);
        }

        for (String temporary : _temporaryFieldsToPromote)
        {
            promote(xcodeml, temporary);
        }

        // Generate loops around statements flagged in previous stage
        generateDoStatements(xcodeml, mergedBlocks);

        // Generate the parallel region
        Directive.generateParallelRegion(xcodeml, _fctDef.body().firstChild(), _fctDef.body().lastChild());
    }

    /**
     * Check temporary variables flagged for promotion to see if this promotion is
     * still needed.
     *
     * @param blocks List of blocks.
     */
    private void removeUselessPromotion(List<VectorBlock> blocks)
    {
        Set<String> noPromotionNeeded = new HashSet<>();
        for (String var : _temporaryFieldsToPromote)
        {
            int usedInBlock = 0;
            for (VectorBlock block : blocks)
            {
                if (block.getReadAndWrittenVariables().contains(var))
                {
                    ++usedInBlock;
                    if (usedInBlock > 1)
                    {
                        break;
                    }
                }
            }
            if (usedInBlock <= 1 && _arrayFieldsInOut.contains(var))
            {
                noPromotionNeeded.add(var);
            }
        }
        _temporaryFieldsToPromote.removeAll(noPromotionNeeded);
    }

    /**
     * Check for potential missing promotions of scalar fields.
     *
     * @param blocks List of blocks.
     */
    private void checkMissingPromotion(List<VectorBlock> blocks)
    {

        for (String var : _scalarFields)
        {
            // If shared by multiple block and not promoted yet.
            if (isVarSharedByMultipleBlocks(blocks, var) && isVarNotOnlyConstant(var)
                    && isVarWrittenInBlocks(blocks, var) && !_arrayFieldsInOut.contains(var)
                    && !_inductionVariables.contains(var) && !_noPromotion.contains(var))
            {
                Message.debug(String.format("%s Promotion might be missing for: %s", SCA_DEBUG_PREFIX, var));
                _temporaryFieldsToPromote.add(var);

                for (AssignStatement as : _fctDef.gatherAssignStatementsByLhsName(var))
                {
                    // Check that assignments are contained in a vector block
                    if (!VectorBlock.isContainedIn(blocks, as))
                    {
                        blocks.add(new VectorBlock(as));
                    }
                }
            }
        }
    }

    /**
     * Check is the variable is not only updated by constant values.
     *
     * @param var Variable to check.
     * @return True if the variable is written with none constant value. False
     *         otherwise.
     */
    private boolean isVarNotOnlyConstant(String var)
    {
        List<AssignStatement> assignStatements = _fctDef.gatherAssignStatementsByLhsName(var);
        for (AssignStatement as : assignStatements)
        {
            if (!as.isConstantAssignement())
            {
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
     * @return True if the variable is shared by at least two blocks. False
     *         otherwise.
     */
    private boolean isVarSharedByMultipleBlocks(List<VectorBlock> blocks, String var)
    {
        int nbBlockSharingVariable = 0;
        // Check if there is more than one block using the variable.
        for (VectorBlock block : blocks)
        {
            if (block.getReadAndWrittenVariables().contains(var))
            {
                ++nbBlockSharingVariable;
                if (nbBlockSharingVariable > 1)
                {
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
    private boolean isVarWrittenInBlocks(List<VectorBlock> blocks, String var)
    {
        for (VectorBlock block : blocks)
        {
            if (block.getWrittenVariables().contains(var))
            {
                return true;
            }
        }
        return false;
    }

    /**
     * Check whether condition includes a promoted variables. If so, the ancestor
     * node using the condition must be wrapped in a do statement.
     *
     * @return Set of vector block for flagged location.
     */
    private Set<VectorBlock> flagIfStatementWithPromotion()
    {
        Set<VectorBlock> blocks = new HashSet<>();
        /*  */
        List<Xnode> conditions = _fctDef.body().matchAll(Xcode.CONDITION);
        for (Xnode condition : conditions)
        {
            if (Condition.dependsOn(condition, _arrayFieldsInOut) && !Condition.isAllocationRelated(condition))
            {
                Xnode ancestor = condition.ancestor();
                Iterator<VectorBlock> iter = blocks.iterator();
                boolean addFlaggedLocation = true;
                while (iter.hasNext())
                {
                    if (ancestor.isNestedIn(iter.next().getStartStmt()))
                    {
                        addFlaggedLocation = false;
                        break;
                    }
                }
                if (addFlaggedLocation)
                {
                    blocks.add(new VectorBlock(ancestor));
                }
            }
        }
        return blocks;
    }

    /**
     * Go through all assignments and flag all location where a do statement should
     * be inserted after variables promotion.
     *
     * @param assignStatements List of assignments.
     * @return Set of vector blocks as flag to do statement insertion location.
     */
    private Set<VectorBlock> flagDoStatementLocation(List<AssignStatement> assignStatements)
    {
        Set<VectorBlock> blocks = flagIfStatementWithPromotion();

        /*
         * Iterate a second time over assign statements to flag places where to insert
         * the do statements
         */
        for (AssignStatement assign : assignStatements)
        {
            Xnode lhs = assign.getLhs();
            String lhsName = assign.getLhsName();
            boolean wrapInDoStatement = true;

            // Check if assignment is dependant of an if statement.
            if (assign.isChildOf(Xcode.F_IF_STATEMENT))
            {

                // Gather all potential ancestor if statements
                List<Xnode> ifStatements = assign.matchAllAncestor(Xcode.F_IF_STATEMENT, Xcode.F_FUNCTION_DEFINITION);

                Xnode flaggedIfStmt = null;

                Set<String> assignVars = assign.getVarNames();
                assignVars.retainAll(_arrayFieldsInOut);

                for (Xnode ifStmt : ifStatements)
                {
                    if (Condition.dependsOn(ifStmt.matchDirectDescendant(Xcode.CONDITION), assignVars))
                    {
                        // Have to put the do statement around the if as the assignment
                        // is conditional as well.
                        flaggedIfStmt = ifStmt;
                    }
                }

                if (flaggedIfStmt != null)
                {
                    wrapInDoStatement = false;
                    boolean addFlaggedIf = true;

                    // Get rid of previously flagged location in this if body.
                    Iterator<VectorBlock> iter = blocks.iterator();
                    while (iter.hasNext())
                    {
                        Xnode crt = iter.next().getStartStmt();
                        if (assign.isNestedIn(crt) || flaggedIfStmt.isNestedIn(crt))
                        {
                            addFlaggedIf = false;
                        }
                        if (crt.isNestedIn(flaggedIfStmt))
                        {
                            iter.remove();
                        }
                    }

                    if (addFlaggedIf)
                    {
                        blocks.add(new VectorBlock(flaggedIfStmt));
                    }
                }
            }

            for (VectorBlock block : blocks)
            {
                if (assign.isNestedIn(block.getStartStmt()))
                {
                    wrapInDoStatement = false;
                    break;
                }
            }

            if (((Xnode.isOfCode(lhs, Xcode.F_ARRAY_REF) || Xnode.isOfCode(lhs, Xcode.VAR))
                    && _arrayFieldsInOut.contains(lhsName) && wrapInDoStatement)
                    || ((Xnode.isOfCode(lhs, Xcode.VAR)
                            || Xnode.isOfCode(lhs, Xcode.F_ARRAY_REF) && _scalarFields.contains(lhsName))
                            && (shouldBePromoted(assign) && wrapInDoStatement)))
            {
                blocks.add(new VectorBlock(assign));
            }
        }
        return blocks;
    }

    /**
     * Iterate over all assign statements to detect all indirect promotion. Apply
     * correct promotion if detected.
     *
     * @param assignStatements List of assignment statements
     */
    private void detectIndirectPromotion(List<AssignStatement> assignStatements)
    {
        /*
         * If the assignment is in the column loop and is composed with some promoted
         * variables, the field must be promoted and the var reference switch to an
         * array reference
         */
        for (AssignStatement assign : assignStatements)
        {
            Xnode lhs = assign.getLhs();
            if ((Xnode.isOfCode(lhs, Xcode.VAR) || Xnode.isOfCode(lhs, Xcode.F_ARRAY_REF))
                    && !_noPromotion.contains(assign.getLhsName()) && !_inductionVariables.contains(assign.getLhsName())
                    && !_arrayFieldsInOut.contains(assign.getLhsName()) && shouldBePromoted(assign))
            {
                _arrayFieldsInOut.add(assign.getLhsName());
                _temporaryFieldsToPromote.add(assign.getLhsName());
            }
        }
    }

    /**
     * Promote the given variable and adapt references.
     *
     * @param xcodeml Current translation unit.
     * @param var     Variable name to be promoted.
     * @throws IllegalTransformationException If promotion cannot be done.
     */
    private void promote(XcodeProgram xcodeml, String var) throws IllegalTransformationException
    {
        PromotionInfo promotionInfo;
        // Do the promotion if needed
        if (!_promotions.containsKey(var))
        {
            Message.debug(String.format("%s promote variable %s", SCA_DEBUG_PREFIX, var));
            promotionInfo = new PromotionInfo(var, _claw.getLayoutForData(var));
            Field.promote(promotionInfo, _fctDef, xcodeml);
            _promotions.put(var, promotionInfo);
        } else
        {
            promotionInfo = _promotions.get(var);
        }
        // Adapt references
        Xid id = _fctDef.getSymbolTable().get(var);
        FbasicType bType = xcodeml.getTypeTable().getBasicType(id);
        if (!bType.isArray())
        {
            Field.adaptScalarRefToArrayRef(_promotions.get(var), _fctDef, _claw.getDefaultLayout(), xcodeml);
        } else
        {
            Field.adaptArrayRef(_promotions.get(var), _fctDef.body(), false, xcodeml);
            Field.adaptAllocate(_promotions.get(var), _fctDef.body(), xcodeml);
        }
        promotionInfo.setRefAdapted();
    }

    /**
     * Generate new DO statement at flagged location.
     *
     * @param xcodeml Current translation unit.
     * @param blocks  List of vectorization friendly blocks.
     */
    private void generateDoStatements(XcodeProgram xcodeml, List<VectorBlock> blocks)
            throws IllegalTransformationException
    {
        for (VectorBlock block : blocks)
        {
            NestedDoStatement loops = new NestedDoStatement(_claw.getDefaultLayoutReversed(), xcodeml);

            if (block.isSingleStatement())
            {
                block.getStartStmt().insertAfter(loops.getOuterStatement());
                loops.getInnerStatement().body().append(block.getStartStmt(), true);
                block.getStartStmt().delete();
            } else
            {
                block.getEndStmt().insertAfter(loops.getOuterStatement());
                Body.shiftIn(block.getStartStmt(), block.getEndStmt(), loops.getInnerStatement().body(), true);
            }

            Directive.generateLoopDirectives(xcodeml, loops.getOuterStatement(), loops.getOuterStatement(),
                    Directive.NO_COLLAPSE);
        }
    }

    /**
     * Check whether the LHS variable should be promoted.
     *
     * @param assign Assign statement node.
     * @return True if the LHS variable should be promoted. False otherwise.
     */
    private boolean shouldBePromoted(AssignStatement assign)
    {
        return (assign.getRhs() != null)
                && XnodeUtil.findAllReferences(assign.getRhs()).stream().anyMatch(_arrayFieldsInOut::contains);
    }

}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Field;
import claw.tatsu.primitive.Loop;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.abstraction.HoistedNestedDoStatement;
import claw.tatsu.xcodeml.abstraction.ReshapeInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawPragma;
import claw.wani.language.ClawClause;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.translator.ClawTranslator;

import java.util.ArrayList;
import java.util.List;

/**
 * A LoopHoist transformation is an independent transformation over a structured
 * block. It performs loop fusion in the given block as well as do start
 * statement hoisting.
 *
 * @author clementval
 */
public class LoopHoist extends ClawBlockTransformation
{

    private final List<HoistedNestedDoStatement> _hoistedGroups;

    /**
     * Constructs a new LoopHoist triggered from a specific directive.
     *
     * @param startDirective The directive that triggered the loop hoist
     *                       transformation.
     * @param endDirective   The directive that end the structured block.
     */
    public LoopHoist(ClawPragma startDirective, ClawPragma endDirective)
    {
        super(startDirective, endDirective);
        _hoistedGroups = new ArrayList<>();
    }

    /**
     * @see Transformation#analyze(XcodeProgram, Translator)
     */
    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        if (_clawEnd == null)
        {
            xcodeml.addError("loop-hoist directive requires an end directive.", _clawStart.getPragma().lineNo());
            return false;
        }

        int pragmaDepthLevel = _clawStart.getPragma().depth();

        // Find all the group of nested loops that can be part of the hoisting
        List<HoistedNestedDoStatement> statements = XnodeUtil.findDoStatementForHoisting(_clawStart.getPragma(),
                _clawEnd.getPragma(), _clawStart.values(ClawClause.HOIST_INDUCTIONS));

        if (statements.isEmpty())
        {
            xcodeml.addError("No do statement group meets the criteria of hoisting.", _clawStart.getPragma().lineNo());
            return false;
        }

        for (HoistedNestedDoStatement hoistedNestedDoStmt : statements)
        {
            int depth = hoistedNestedDoStmt.getOuterStatement().depth();
            if (depth != pragmaDepthLevel)
            {
                Xnode tmpIf = hoistedNestedDoStmt.getOuterStatement().matchAncestor(Xcode.F_IF_STATEMENT);
                Xnode tmpSelect = hoistedNestedDoStmt.getOuterStatement().matchAncestor(Xcode.F_SELECT_CASE_STATEMENT);
                Xnode tmpDo = hoistedNestedDoStmt.getOuterStatement().matchAncestor(Xcode.F_DO_STATEMENT);

                if (tmpIf == null && tmpSelect == null && tmpDo == null)
                {
                    xcodeml.addError(
                            "A nested do stmt group is nested in an unsupported " + "statement for loop hoisting.",
                            hoistedNestedDoStmt.getOuterStatement().lineNo());
                    return false;
                }

                int ifDepth = (tmpIf != null) ? tmpIf.depth() : Xnode.UNDEF_DEPTH;
                int selectDepth = (tmpSelect != null) ? tmpSelect.depth() : Xnode.UNDEF_DEPTH;
                int doDepth = (tmpDo != null) ? tmpDo.depth() : Xnode.UNDEF_DEPTH;

                if ((pragmaDepthLevel <= ifDepth || pragmaDepthLevel <= selectDepth || pragmaDepthLevel <= doDepth)
                        && (ifDepth < depth || selectDepth < depth || doDepth < depth))
                {
                    hoistedNestedDoStmt.setExtraction();
                } else
                {
                    xcodeml.addError("Group is nested in an unsupported "
                            + "statement for loop hoisting or depth is too high " + "(Group index starts at 0).",
                            _clawStart.getPragma().lineNo());
                    return false;
                }
            }
            _hoistedGroups.add(hoistedNestedDoStmt);
        }

        HoistedNestedDoStatement master = _hoistedGroups.get(0);
        for (int i = 1; i < _hoistedGroups.size(); ++i)
        {
            HoistedNestedDoStatement next = _hoistedGroups.get(i);
            for (int j = 0; j < master.size(); ++j)
            {
                // Iteration range are identical, just merge
                if (j == 0 && (!Loop.hasSameIndexRange(master.get(j), next.get(j))
                        && Loop.hasSameIndexRangeBesidesLower(master.get(j), next.get(j))))
                {
                    // Iteration range are identical besides lower-bound, if creation
                    next.setIfStatement();
                } else if (!Loop.hasSameIndexRange(master.get(j), next.get(j)))
                {
                    // Iteration range are too different, stop analysis
                    xcodeml.addError("Iteration range of do statements group " + i
                            + " differs from group 0. Loop hoisting aborted.", _clawStart.getPragma().lineNo());
                    return false;
                }
            }
        }

        // Check reshape mandatory points
        if (_clawStart.hasClause(ClawClause.RESHAPE))
        {
            FfunctionDefinition fctDef = _clawStart.getPragma().findParentFunction();
            if (fctDef == null)
            {
                xcodeml.addError("Unable to matchSeq the function/subroutine/module "
                        + "definition including the current directive", _clawStart.getPragma().lineNo());
                return false;
            }

            for (ReshapeInfo r : _clawStart.getReshapeClauseValues())
            {
                if (!fctDef.getSymbolTable().contains(r.getArrayName())
                        || !fctDef.getDeclarationTable().contains(r.getArrayName()))
                {
                    // Check in the parent def if present
                    if (!checkUpperDefinition(fctDef, r.getArrayName()))
                    {
                        xcodeml.addError(String.format("Reshape variable %s not found in " + "the definition of %s",
                                r.getArrayName(), fctDef.getName()), _clawStart.getPragma().lineNo());
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
    private boolean checkUpperDefinition(FfunctionDefinition fctDef, String name)
    {
        FfunctionDefinition upperDef = fctDef.findParentFunction();
        return upperDef != null
                && (!(!upperDef.getSymbolTable().contains(name) || !upperDef.getDeclarationTable().contains(name))
                        || checkUpperDefinition(upperDef, name));
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation transformation)
    {
        return false; // Independent transformation
    }

    /**
     * @see Transformation#transform(XcodeProgram, Translator, Transformation)
     */
    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation transformation) throws Exception
    {
        ClawTranslator ct = (ClawTranslator) translator;

        HoistedNestedDoStatement hoisted = Loop.hoist(_hoistedGroups, _clawStart.getPragma(), _clawEnd.getPragma(),
                xcodeml);

        // Generate dynamic transformation (interchange)
        ct.generateAdditionalTransformation(_clawStart, xcodeml, hoisted.getOuterStatement());

        // Apply cleanup clause
        if (_clawStart.hasClause(ClawClause.CLEANUP))
        {
            Pragma.remove(_clawStart.getCleanupClauseValue(), _clawStart.getPragma(), _clawEnd.getPragma());
        }

        // Apply reshape clause
        if (_clawStart.hasClause(ClawClause.RESHAPE))
        {
            FfunctionDefinition fctDef = _clawStart.getPragma().findParentFunction();
            if (fctDef == null)
            {
                throw new IllegalTransformationException(
                        "Cannot apply reshape clause." + "Parent function definition not found.",
                        _clawStart.getPragma().lineNo());
            }

            for (ReshapeInfo reshapeInfo : _clawStart.getReshapeClauseValues())
            {
                Field.reshape(fctDef, reshapeInfo, xcodeml);
            }
        }

        removePragma();
    }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import java.util.List;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Loop;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

/**
 * A LoopInterchange transformation is a an independent transformation. It allow
 * to reorder nested loops up to three levels.
 *
 * @author clementval
 */
public class LoopInterchange extends ClawTransformation
{

    private NestedDoStatement _doStmts = null;

    /**
     * Constructs a new LoopInterchange triggered from a specific pragma.
     *
     * @param directive The directive that triggered the loop interchange
     *                  transformation.
     */
    public LoopInterchange(ClawPragma directive)
    {
        super(directive);
    }

    /**
     * Apply the transformation.
     *
     * @param xcodeml        The XcodeML on which the transformations are applied.
     * @param translator     The translator used to applied the transformations.
     * @param transformation Only for dependent transformation. The other
     *                       transformation part of the transformation.
     * @throws IllegalTransformationException if the transformation cannot be
     *                                        applied.
     */
    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation transformation)
            throws IllegalTransformationException
    {

        analyze(xcodeml, translator);

        Loop.reorder(xcodeml.context(), _doStmts, _claw.values(ClawClause.INTERCHANGE_INDEXES));

        // Generate directive pragmas if needed
        if (_claw.hasClause(ClawClause.ACC))
        {
            /*
             * TODO see TODO in ExpandNotation OpenACC and OpenMP loop construct are pretty
             * different ... have to look how to do that properly. See issue #22
             */
            Directive.generateAcceleratorClause(xcodeml, _doStmts.getOuterStatement(), _claw.value(ClawClause.ACC));
        }

        if (_claw.hasClause(ClawClause.PARALLEL))
        {
            Directive.generateParallelRegion(xcodeml, _doStmts.getOuterStatement(), _doStmts.getOuterStatement());
        }

        removePragma();
        transformed();
    }

    /**
     * Loop fusion analysis: - Find the different do statement that will be
     * reordered. - Check the validity of the new ordering option.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @return True if the transformation can be performed. False otherwise.
     */
    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        // Find next loop after pragma
        Xnode outerDoStatement = _claw.getPragma().matchSibling(Xcode.F_DO_STATEMENT);
        if (outerDoStatement == null)
        {
            xcodeml.addError("top level loop not found", _claw.getPragma().lineNo());
            return false;
        }

        int nestedLevel = _claw.values(ClawClause.INTERCHANGE_INDEXES) != null
                ? _claw.values(ClawClause.INTERCHANGE_INDEXES).size()
                : 2;
        _doStmts = new NestedDoStatement(outerDoStatement, nestedLevel);

        if (_claw.values(ClawClause.INTERCHANGE_INDEXES) != null)
        {
            if (_claw.values(ClawClause.INTERCHANGE_INDEXES).size() != 3)
            {
                xcodeml.addError("new-order option has not enough parameters", _claw.getPragma().lineNo());
            }

            List<String> inductions = _doStmts.getInductionVariables();
            for (String idx : _claw.values(ClawClause.INTERCHANGE_INDEXES))
            {
                if (!inductions.contains(idx.toLowerCase()))
                {
                    xcodeml.addError("invalid induction variable in new-order option. " + idx,
                            _claw.getPragma().lineNo());
                    return false;
                }
            }
        } else
        {
            if (_doStmts.size() < 2)
            {
                xcodeml.addError("Not enough nested do statements to reorder", _claw.getPragma().lineNo());
            }
        }
        return true;
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation transformation)
    {
        return false; // independent transformation
    }
}

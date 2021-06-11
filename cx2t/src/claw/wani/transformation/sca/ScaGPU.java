/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author: not specified
 */
package claw.wani.transformation.sca;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.TatsuConstant;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.directive.configuration.AcceleratorConfiguration;
import claw.tatsu.directive.configuration.AcceleratorLocalStrategy;
import claw.tatsu.directive.generator.DirectiveGenerator;
import claw.tatsu.primitive.Body;
import claw.tatsu.primitive.Field;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.abstraction.Xblock;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import claw.wani.language.ClawPragma;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawTranslator;

/**
 * Specialized version of SCA transformation for GPU target.
 *
 * Transformation for the GPU target:
 * <ul>
 * <li>Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li>Do statements over the additional dimensions is added as an outer loop
 * and wrap the entire body of the subroutine.
 * </ul>
 *
 * Generation of OpenACC directives:
 * <ul>
 * <li>acc routine seq is generated for subroutine called from the SCA
 * subroutine if they are located in the same translation unit.
 * <li>acc data region with corresponding present clause for all promoted
 * variables with the intent in, out or inout.
 * <li>acc parallel region is generated to wrap all the body of the subroutine.
 * <li>acc private clause is added to the parallel directive for all local
 * variables.
 * <li>acc loop is generated for the generated do statement.
 * <li>acc loop seq is generated for already existing do statements.
 * </ul>
 *
 * Generation of OpenMP directives on CPU:
 * <ul>
 * <li>omp parallel do is generated for each generated do statements.
 * </ul>
 *
 * Generation of OpenMP directives on GPU:
 * <ul>
 * <li>MISSING FEATURE : omp declare target is generated for subroutine called
 * from the SCA subroutine if they are located in the same translation unit.
 * <li>omp data region with corresponding present clause for all promoted
 * variables with the intent to, from or tofrom.
 * <li>omp target teams distribute region is generated to wrap all the body of
 * the subroutine.
 * <li>omp private clause is added to the target directive for all local
 * variables.
 * <li>omp collapse is generated for the generated do statement (if more that
 * 1).
 * </ul>
 *
 * @author clementval
 */
public class ScaGPU extends Sca
{

    private Xnode _specialParallelRegionStart = null;

    /**
     * Constructs a new SCA transformation triggered from a specific pragma for a
     * GPU target.
     *
     * @param directive The directive that triggered the define transformation.
     */
    public ScaGPU(ClawPragma directive)
    {
        super(directive);
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        if (!detectParentFunction(xcodeml))
        {
            return false;
        }

        ClawTranslator trans = (ClawTranslator) translator;

        if (_fctType.isElemental())
        {
            return analyzeElemental(xcodeml, trans);
        } else
        {
            return analyzeStandard(xcodeml, trans);
        }

    }

    /**
     * Perform analysis steps for SCA transformation on standard function/subroutine
     * for GPU target.
     *
     * @param xcodeml    Current translation unit.
     * @param translator Current translator.
     * @return True if the analysis succeed. False otherwise.
     */
    private boolean analyzeStandard(XcodeProgram xcodeml, ClawTranslator translator)
    {
        final Context context = xcodeml.context();
        DirectiveGenerator dirGen = context.getGenerator();

        /*
         * Check if unsupported statements are located in the future parallel region.
         */
        if (dirGen.getDirectiveLanguage() != CompilerDirective.NONE)
        {
            Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
            Xnode parallelRegionStart;
            if (_fctDef.body().child(0).is(Xcode.F_PRAGMA_STATEMENT) &&
                _fctDef.body().child(0).value().toLowerCase().startsWith(TatsuConstant.CLAW_PREFIX))
            {
                parallelRegionStart = Directive.findParallelRegionStart(context, _fctDef, null);
            } else
            {
                parallelRegionStart = _claw.getPragma().nextSibling();
                _specialParallelRegionStart = parallelRegionStart;
            }
            Xnode parallelRegionEnd = Directive.findParallelRegionEnd(context, _fctDef, contains);

            List<Xnode> unsupportedStatements = XnodeUtil.getNodes(parallelRegionStart, parallelRegionEnd,
                    dirGen.getUnsupportedStatements());

            if (!unsupportedStatements.isEmpty())
            {
                List<Xnode> falsePositive = new ArrayList<>();
                for (Xnode statement : unsupportedStatements)
                {
                    if (canTransformReturn(statement))
                    {
                        falsePositive.add(statement);
                    } else
                    {
                        if (statement != null)
                        {
                            xcodeml.addError(
                                    "Unsupported statement in parallel region: " + statement.opcode().fortran(),
                                    statement.lineNo());
                        } else
                        {
                            throw new NullPointerException("statement is null");
                        }
                    }
                }
                // Only one return statement can be transformed at the moment.
                if (falsePositive.size() > 1)
                {
                    return false;
                }
                unsupportedStatements.removeAll(falsePositive);
                if (!unsupportedStatements.isEmpty())
                {
                    return false;
                }
            }
        }

        detectInductionVariables();

        return analyzeDimension(translator.cfg(), xcodeml) && analyzeData(xcodeml, translator);
    }

    /**
     * Check whether a return statement can be transformed or will trigger an
     * unsupported statement error. Currently, only if statement located at the
     * first level of the function definition are transformable.
     *
     * @param returnStmt Node pointing to the return statement.
     * @return True if the return statement is directly nested in a if-then body.
     *         False otherwise.
     */
    private boolean canTransformReturn(Xnode returnStmt)
    {
        return returnStmt != null && returnStmt.is(Xcode.F_RETURN_STATEMENT) && returnStmt.ancestorIs(Xcode.BODY)
                && returnStmt.ancestor().ancestorIs(Xcode.THEN)
                && returnStmt.ancestor().ancestor().ancestorIs(Xcode.F_IF_STATEMENT)
                && returnStmt.ancestor().ancestor().ancestor().ancestorIs(Xcode.BODY)
                && returnStmt.ancestor().ancestor().ancestor().ancestor().ancestorIs(Xcode.F_FUNCTION_DEFINITION)
                && returnStmt.ancestor().ancestor().ancestor().ancestor().ancestor().equals(_fctDef);
    }

    /**
     * Perform analysis steps for SCA transformation on ELEMENTAL
     * function/subroutine for GPU target.
     *
     * @param xcodeml    Current translation unit.
     * @param translator Current translator.
     * @return True if the analysis succeed. False otherwise.
     */
    private boolean analyzeElemental(XcodeProgram xcodeml, ClawTranslator translator)
    {
        // Elemental needs model-data directive
        if (!_claw.isScaModelConfig() || !translator.cfg().getModelConfig().isLoaded())
        {
            xcodeml.addError("SCA applied in ELEMENTAL function/subroutine " + "requires model configuration!",
                    _claw.getPragma());
            return false;
        }
        return analyzeData(xcodeml, translator);
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception
    {
        ClawTranslator trans = (ClawTranslator) translator;
        if (_fctType.isElemental())
        {
            transformElemental(xcodeml, trans);
        } else
        {
            transformStandard(xcodeml, trans);
        }
    }

    private void transformReturnStatement(XcodeProgram xcodeml) throws IllegalTransformationException
    {
        List<Xnode> returns = _fctDef.matchAll(Xcode.F_RETURN_STATEMENT);
        if (returns.isEmpty())
        {
            return; // No return statement to be transformed
        }

        if (returns.size() > 1)
        {
            throw new IllegalTransformationException(
                    "RETURN transformation is " + "currently limited to one per subroutine/function");
        }

        Xnode returnStmt = returns.get(0);
        if (!canTransformReturn(returnStmt))
        {
            throw new IllegalTransformationException("RETURN statement cannot be " + "transformed.");
        }

        Xnode thenBody = returnStmt.ancestor();
        Xnode thenNode = thenBody.ancestor();
        Xnode ifNode = thenNode.ancestor();

        Xnode elseNode = xcodeml.createElse();
        ifNode.append(elseNode);

        returnStmt.delete();
        thenBody.append(xcodeml.createComment("CLAW: RETURN statement transformed for parallel region"));

        Body.shiftIn(ifNode.nextSibling(), _fctDef.lastChild(), elseNode.body(), true);
    }

    /**
     * Apply transformation on standard function/subroutine.
     *
     * @param xcodeml    Current translation unit.
     * @param translator Current translator.
     * @throws Exception when transformation cannot by applied.
     */
    private void transformStandard(XcodeProgram xcodeml, ClawTranslator translator) throws Exception
    {
        transformReturnStatement(xcodeml);

        // Apply the common transformation
        super.transform(xcodeml, translator, null);

        // Apply specific steps for GPU target
        applySpecificTransformation(translator.cfg(), xcodeml);

        // Finalize the common steps
        super.finalizeTransformation(xcodeml);
    }

    /**
     * Apply transformation on ELEMENTAL function/subroutine.
     *
     * @param xcodeml    Current translation unit.
     * @param translator Current translator.
     * @throws IllegalTransformationException If transformation fails.
     */
    private void transformElemental(XcodeProgram xcodeml, ClawTranslator translator) throws Exception
    {
        /*
         * SCA in ELEMENTAL function. Only flag the function and leave the actual
         * transformation until having information on the calling site from another
         * translation unit.
         */
        if (_fctType.isElemental())
        {

            _fctType.setBooleanAttribute(Xattr.WAS_ELEMENTAL, true);

            if (_fctType.isFunction() && !_fctType.hasAttribute(Xattr.RESULT_NAME))
            {
                _arrayFieldsInOut.add(_fctDef.getName());
            }

            if (translator.cfg().getBooleanParameter(Configuration.SCA_ELEMENTAL_PROMOTION_ASSUMED))
            {
                forceAssumedShapedArrayPromotion = _fctType.isSubroutine()
                        || !(_arrayFieldsInOut.contains(_fctType.getResultName())
                                || _arrayFieldsInOut.contains(_fctDef.getName()));
            }

            // SCA ELEMENTAL
            FmoduleDefinition modDef = _fctDef.findParentModule();
            if (modDef == null)
            {
                throw new IllegalTransformationException(
                        "SCA in ELEMENTAL function " + "transformation requires module encapsulation.");
            }

            transformReturnStatement(xcodeml);

            // Apply the common transformation
            super.transform(xcodeml, translator, null);

            // Remove ELEMENTAL and PURE attributes if present.
            removeAttributesWithWaring(xcodeml, _fctType, Xattr.IS_ELEMENTAL);
            removeAttributesWithWaring(xcodeml, _fctType, Xattr.IS_PURE);

            // Apply specific steps for GPU
            applySpecificTransformation(translator.cfg(), xcodeml);

            // Finalize the common steps
            super.finalizeTransformation(xcodeml);
        }
    }

    /**
     * Apply specific transformation steps for GPU target.
     *
     * @param xcodeml Current translation unit.
     * @throws IllegalTransformationException If any transformation fails.
     */
    private void applySpecificTransformation(Configuration cfg, XcodeProgram xcodeml)
            throws IllegalTransformationException
    {
        final Context context = xcodeml.context();
        AcceleratorConfiguration config = cfg.accelerator();

        if (_fctDef.hasEmptyBody())
        {
            return; // Nothing to do in this function
        }

        /*
         * Create a nested loop with the new defined dimensions and wrap it around the
         * whole subroutine's body. This is for the moment a really naive transformation
         * idea but it is our start point. Use the first over clause to create it.
         */
        NestedDoStatement loops;
        if (forceAssumedShapedArrayPromotion)
        {
            if (_promotions.isEmpty())
            {
                throw new IllegalTransformationException(
                        "Cannot assume shape of " + "array in elemental function/subroutine.",
                        _claw.getPragma().lineNo());
            }
            PromotionInfo pi = _promotions.entrySet().iterator().next().getValue();
            loops = new NestedDoStatement(_claw.getDefaultLayoutReversed(cfg), pi, xcodeml);
        } else
        {
            loops = new NestedDoStatement(_claw.getDefaultLayoutReversed(cfg), xcodeml);
        }

        /*
         * Subroutine/function can have a contains section with inner subroutines or
         * functions. The newly created (nested) do statements should stop before this
         * contains section if it exists.
         */
        Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
        if (contains != null)
        {
            Xnode parallelRegionStart;
            if (_specialParallelRegionStart == null)
            {
                parallelRegionStart = Directive.findParallelRegionStart(context, _fctDef, null);
            } else
            {
                parallelRegionStart = _specialParallelRegionStart;
            }
            Xnode parallelRegionEnd = Directive.findParallelRegionEnd(context, _fctDef, contains);

            Body.shiftIn(parallelRegionStart, parallelRegionEnd, loops.getInnerStatement().body(), true);

            contains.insertBefore(loops.getOuterStatement());
        } else
        {
            // No contains section, all the body is copied to the do statements.
            Xnode parallelRegionStart;
            if (_specialParallelRegionStart == null)
            {
                parallelRegionStart = Directive.findParallelRegionStart(context, _fctDef, null);
            } else
            {
                parallelRegionStart = _specialParallelRegionStart;
            }
            Xnode parallelRegionEnd = Directive.findParallelRegionEnd(context, _fctDef, null);

            // Define a hook from where we can insert the new do statement
            Xnode hook = parallelRegionEnd != null ? parallelRegionEnd.nextSibling() : null;
            Body.shiftIn(parallelRegionStart, parallelRegionEnd, loops.getInnerStatement().body(), true);

            // Hook is null then we append the do statement to the current fct body
            if (hook == null)
            {
                _fctDef.body().append(loops.getOuterStatement());
            } else
            {
                // Insert new do statement before the hook element
                hook.insertBefore(loops.getOuterStatement());
            }
        }

        // TODO nodep passing!
        int collapse = Directive.generateLoopSeq(xcodeml, loops.getInnerStatement().body(), CompilerDirective.CLAW.getPrefix() + " nodep");

        // Prepare variables list for present/pcreate clauses and handle
        // promotion/privatize local strategy
        List<String> presentList = _fctDef.getPresentVariables(xcodeml);
        List<String> privateList = Collections.emptyList();
        List<String> createList = Collections.emptyList();
        if (config.getLocalStrategy() == AcceleratorLocalStrategy.PRIVATE)
        {
            privateList = applyPrivateStrategy(xcodeml);
        } else if (config.getLocalStrategy() == AcceleratorLocalStrategy.PROMOTE)
        {
            createList = applyPromoteStrategy(cfg, xcodeml);
        }

        // Generate the data region
        Xblock doStmtBlock = new Xblock(loops.getOuterStatement());
        Directive.generateDataRegionClause(xcodeml, presentList, createList, doStmtBlock);

        // Generate the parallel region
        Directive.generateParallelLoopClause(xcodeml, privateList, loops.getOuterStatement(), loops.getOuterStatement(),
                null, loops.size() + collapse);

        Directive.generateRoutineDirectives(xcodeml, _fctDef);
    }

    /**
     * Apply the private local array strategy. Gather all information about local
     * array requiring a privatization.
     *
     * @param xcodeml Current translation unit.
     * @return List of private variables.
     */
    private List<String> applyPrivateStrategy(XcodeProgram xcodeml)
    {
        List<String> privateList = _fctDef.getLocalVariables(xcodeml, true);
        // Iterate over a copy to be able to remove items
        for (String identifier : new ArrayList<>(privateList))
        {
            if (_promotions.containsKey(identifier))
            {
                privateList.remove(identifier);
            }
        }
        return privateList;
    }

    /**
     * Apply the promotion local array strategy. Gather all information about local
     * variable requiring a promotion and apply it.
     *
     * @param xcodeml Current translation unit.
     * @return List of promoted variable requiring an allocation.
     * @throws IllegalTransformationException If promotion of variable fails.
     */
    private List<String> applyPromoteStrategy(Configuration cfg, XcodeProgram xcodeml)
            throws IllegalTransformationException
    {
        List<String> createList = _fctDef.getLocalVariables(xcodeml, true);
        for (String arrayIdentifier : createList)
        {
            _arrayFieldsInOut.add(arrayIdentifier);
            PromotionInfo promotionInfo = new PromotionInfo(arrayIdentifier,
                    _claw.getLayoutForData(cfg, arrayIdentifier));

            Field.promote(promotionInfo, _fctDef, xcodeml);
            _promotions.put(arrayIdentifier, promotionInfo);

            Field.adaptArrayRef(promotionInfo, _fctDef.body(), false, xcodeml);
            Field.adaptAllocate(promotionInfo, _fctDef.body(), xcodeml);
        }
        return createList;
    }

}

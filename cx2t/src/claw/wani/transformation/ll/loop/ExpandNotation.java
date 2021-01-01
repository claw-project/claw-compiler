/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Range;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.abstraction.Xblock;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.common.XstorageClass;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Xintrinsic;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.serialization.Serialization;
import claw.wani.serialization.SerializationStep;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawTranslator;

/**
 * <pre>
 * An ExpandNotation transformation is an independent transformation. It
 * transforms the Fortran array notation into single or nested do statements.
 *
 * Array notation example:
 * A(1:n) = A(1+m:n+m) + B(1:n) * C(n+1:n+n)
 *
 * DO i=1,n
 *   A(i) = A(i+m) + B(i) * C(n+i)
 * END DO
 * </pre>
 *
 * @author clementval
 */
public class ExpandNotation extends ClawBlockTransformation
{

    private final List<List<Xnode>> _groupIterationRanges;
    private final List<List<Xnode>> _groupedAssignStmts;

    /**
     * Constructs a new ExpandNotation triggered from a specific directive.
     *
     * @param begin The directive that triggered the expand notation transformation.
     * @param end   The directive that close the block transformation. Can be null.
     */
    public ExpandNotation(ClawPragma begin, ClawPragma end)
    {
        super(begin, end);
        _groupedAssignStmts = new ArrayList<>();
        _groupIterationRanges = new ArrayList<>();
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        if (_clawEnd != null)
        { // Block transformation

            // TODO Analyse dependency between assignments. cf array9 example.

            // Find assignments with vector notation
            List<Xnode> foundAssignments = XnodeUtil.getArrayAssignInBlock(_clawStart.getPragma(),
                    _clawEnd.getPragma());

            if (foundAssignments.isEmpty())
            {
                xcodeml.addError("No vector notation assignments found in the expand block.",
                        _clawStart.getPragma().lineNo());
                return false;
            }

            /*
             * Using a structure of list of list of assignments to group together the expand
             * notation that share an identical iteration range.
             */

            // 1st group always exists
            _groupedAssignStmts.add(new ArrayList<>());
            int crtGroup = 0;
            Xnode refArrayRef = foundAssignments.get(0).matchSeq(Xcode.F_ARRAY_REF);
            List<Xnode> refRanges = XnodeUtil.getIdxRangesFromArrayRef(refArrayRef);

            // First vector notation is automatically in the 1st group as 1st element
            _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(0));
            _groupIterationRanges.add(refRanges);

            for (int i = 1; i < foundAssignments.size(); ++i)
            {
                Xnode arrayRef = foundAssignments.get(i).matchSeq(Xcode.F_ARRAY_REF);
                List<Xnode> ranges = XnodeUtil.getIdxRangesFromArrayRef(arrayRef);

                // ranges are not identical so
                if (!Range.compare(refRanges, ranges))
                {
                    refRanges = ranges;
                    ++crtGroup;
                    _groupedAssignStmts.add(new ArrayList<>());
                    _groupIterationRanges.add(refRanges);
                }
                _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(i));
            }
            return true;
        } else
        { // single transformation
          // pragma must be followed by an assign statement
            Xnode stmt = _clawStart.getPragma().matchSibling(Xcode.F_ASSIGN_STATEMENT);
            if (stmt == null)
            {
                xcodeml.addError("Directive not followed by an assign statement", _clawStart.getPragma().lineNo());
                return false;
            }
            // Check if we are dealing with an vector notation
            if (!Xnode.isOfCode(stmt.child(0), Xcode.F_ARRAY_REF))
            {
                xcodeml.addError("Assign statement is not an array notation", _clawStart.getPragma().lineNo());
                return false;
            }

            List<Xnode> ranges = stmt.firstChild().children().stream().filter(x -> x.is(Xcode.INDEX_RANGE))
                    .collect(Collectors.toList());

            if (ranges.isEmpty())
            {
                xcodeml.addError("Assign statement is not an array notation", _clawStart.getPragma().lineNo());
                return false;
            }

            _groupedAssignStmts.add(Collections.singletonList(stmt));
            _groupIterationRanges.add(ranges);
            return true;
        }
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        return false; // independent transformation
    }

    /**
     * Transform an assignment using array notation to a do statement.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @param other      Only for dependent transformation. The other transformation
     *                   part of the transformation.
     * @throws Exception If the transformation cannot be applied.
     */
    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception
    {
        final ClawTranslator ct = (ClawTranslator) translator;
        final Configuration cfg = ct.cfg();

        // 1. Find the function/module declaration TODO handle module/program ?
        FfunctionDefinition fctDef = _clawStart.getPragma().findParentFunction();

        Xnode from = _clawStart.getPragma();
        Xnode to = _clawEnd != null ? _clawEnd.getPragma() : null;

        List<String> readArrays = XnodeUtil.getReadArraysInRegion(from, to);
        List<String> writtenArrays = XnodeUtil.getWrittenArraysInRegion(from, to);
        Set<String> presents = new HashSet<>(readArrays);
        presents.addAll(writtenArrays);

        Xblock doStmtsBlock = null;
        Xblock dataRegionBlock = null;

        if (xcodeml.context().isTarget(Target.GPU))
        {
            Xblock crtBlock;
            for (int i = 0; i < _groupedAssignStmts.size(); ++i)
            {
                crtBlock = generateDoStmtNotation(xcodeml, ct, fctDef, _groupIterationRanges.get(i),
                        _groupedAssignStmts.get(i), from);
                if (doStmtsBlock == null)
                {
                    doStmtsBlock = new Xblock(crtBlock.getStart());
                } else
                {
                    doStmtsBlock.setEnd(crtBlock.getEnd());
                }
            }

            if (doStmtsBlock == null)
            {
                throw new IllegalTransformationException("Problem occurred during expand transformation",
                        _clawStart.getPragma().lineNo());
            }

            Xblock parallelRegionBlock;
            Xblock updateRegionBlock = null;
            if (_clawStart.hasClause(ClawClause.PARALLEL))
            {
                List<String> privates = Collections.emptyList();

                String clauses = _clawStart.hasClause(ClawClause.ACC) ? _clawStart.value(ClawClause.ACC) : "";
                parallelRegionBlock = Directive.generateParallelLoopClause(xcodeml, privates, doStmtsBlock.getStart(),
                        doStmtsBlock.getEnd(), clauses, _groupedAssignStmts.size());

                if (_clawStart.hasClause(ClawClause.UPDATE)
                        && cfg.getBooleanParameter(Configuration.SCA_FORWARD_UPDATE_ENABLED))
                {
                    updateRegionBlock = generateUpdateClause(cfg, xcodeml, parallelRegionBlock, readArrays,
                            writtenArrays);
                }

                if (updateRegionBlock == null)
                {
                    updateRegionBlock = parallelRegionBlock;
                }
                List<String> presentLst = new ArrayList<>(presents);
                dataRegionBlock = Directive.generateDataRegionClause(xcodeml, presentLst, Collections.emptyList(),
                        updateRegionBlock);
            }
        } else
        {
            doStmtsBlock = new Xblock(_clawStart.getPragma());
            if (_clawEnd != null)
            {
                doStmtsBlock.setEnd(_clawEnd.getPragma());
            } else
            {
                doStmtsBlock.setEnd(_clawStart.getPragma().nextSibling());
            }
        }

        if (_clawStart.hasClause(ClawClause.SAVEPOINT)
                && cfg.getBooleanParameter(Configuration.SCA_SERIALIZATION_ENABLED))
        {
            if (dataRegionBlock == null)
            {
                dataRegionBlock = doStmtsBlock;
            }
            generateSavepoint(cfg, xcodeml, dataRegionBlock, readArrays, writtenArrays);
        }

        removePragma();
        transformed();
    }

    /**
     * Generate the corresponding do statements for the array notations. A do
     * statement is generated per dimension of the arrays. Iteration index range are
     * computed with array dimensions.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @param fctDef     The function definition in which the array notation is
     *                   nested.
     * @param ranges     The list of iteration ranges to be applied to the created
     *                   do statements.
     * @param statements The list of assign statements (array notation) that will be
     *                   included in the nested do statements.
     * @param doStmtGrip Grip for the code insertion. Do statements will be inserted
     *                   after the grip element.
     * @return Block surrounding the do statements generated.
     */
    private Xblock generateDoStmtNotation(XcodeProgram xcodeml, ClawTranslator translator, FfunctionDefinition fctDef,
            List<Xnode> ranges, List<Xnode> statements, Xnode doStmtGrip) throws IllegalTransformationException
    {
        String[] inductionVars = new String[ranges.size()];
        Xnode[] doStmts = new Xnode[ranges.size()];
        Xnode var = statements.get(0).matchSeq(Xcode.F_ARRAY_REF, Xcode.VAR_REF, Xcode.VAR);
        if (var == null)
        {
            var = statements.get(0).matchSeq(Xcode.F_ARRAY_REF, Xcode.VAR_REF, Xcode.F_MEMBER_REF);
        }
        // 1. Create do statements with induction variables
        for (int i = 0; i < ranges.size(); ++i)
        {
            // 1.1 Create induction variables
            if (_clawStart.hasClause(ClawClause.INDUCTION))
            { // Use user names
                inductionVars[i] = _clawStart.values(ClawClause.INDUCTION).get(i);
            } else
            { // generate new names
                inductionVars[i] = "claw_induction_" + translator.getNextTransformationCounter();
            }

            // 2.2 inject a new entry in the symbol table
            if (!fctDef.getSymbolTable().contains(inductionVars[i]))
            {
                Xid inductionVarId = xcodeml.createId(FortranType.INTEGER, XstorageClass.F_LOCAL, inductionVars[i]);
                fctDef.getSymbolTable().add(inductionVarId, false);
            }

            // 2.3 inject a new entry in the declaration table
            if (!fctDef.getDeclarationTable().contains(inductionVars[i]))
            {
                Xnode inductionVarDecl = xcodeml.createVarDecl(FortranType.INTEGER, inductionVars[i]);
                fctDef.getDeclarationTable().add(inductionVarDecl);
            }

            // 2.4 create do statements
            Xnode inductionVar = xcodeml.createVar(FortranType.INTEGER, inductionVars[i], Xscope.LOCAL);
            Xnode range;
            if (ranges.get(i).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE))
            {
                // Allocatable array
                // dimension argument of size starts at one
                range = xcodeml.createRangeForAssumedShapeArray(var, 1, i + 1);
            } else
            {
                range = ranges.get(i).cloneNode();
            }
            doStmts[i] = xcodeml.createDoStmt(inductionVar, range);
            statements.get(0).copyEnhancedInfo(doStmts[i]);
            if (i == 0)
            { // most outer loop goes after the pragma
                doStmtGrip.insertAfter(doStmts[i]);
            } else
            { // others loop go in the previous one
                doStmts[i - 1].body().append(doStmts[i]);
            }
        }

        for (Xnode stmt : statements)
        {
            // 3. Adapt array reference with induction variables
            List<Xnode> allArrayRef = stmt.matchAll(Xcode.F_ARRAY_REF);
            for (Xnode arrayRef : allArrayRef)
            {
                for (int i = 0; i < arrayRef.children().size() - 1; ++i)
                {
                    Xnode el = arrayRef.child(i + 1);
                    if (Xnode.isOfCode(el, Xcode.INDEX_RANGE) && i < doStmts.length)
                    {
                        String induction = doStmts[i].matchSeq(Xcode.VAR).value();
                        Xnode inductionVar = xcodeml.createVar(FortranType.INTEGER, induction, Xscope.LOCAL);

                        Xnode arrayIdx = xcodeml.createNode(Xcode.ARRAY_INDEX);
                        arrayIdx.append(inductionVar);

                        el.insertAfter(arrayIdx);
                        el.delete();
                    }
                }
            }

            stmt.matchAll(Xcode.FUNCTION_CALL).stream().map(FunctionCall::new)
                    .filter(x -> x.isIntrinsicCall(Xintrinsic.SUM)).forEach(FunctionCall::adaptIntrinsicSumCall);

            stmt.matchAll(Xcode.FUNCTION_CALL).stream().map(FunctionCall::new)
                    .filter(x -> x.isIntrinsicCall(Xintrinsic.SPREAD)).forEach(FunctionCall::adaptIntrinsicSpreadCall);

            // 4. Move assignment statement inside the most inner loop
            doStmts[ranges.size() - 1].body().append(stmt, true);
            stmt.delete();
        }

        // Add any additional transformation defined in the directive clauses
        translator.generateAdditionalTransformation(_clawStart, xcodeml, doStmts[0]);

        return new Xblock(doStmts[0]);
    }

    /**
     * Generate update device/host directives to manage data before and after the
     * expand block.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @param hook    Block around which directives are generated
     */
    private Xblock generateUpdateClause(Configuration cfg, XcodeProgram xcodeml, Xblock hook, List<String> readArrays,
            List<String> writtenArrays)
    {
        Xnode startNode;
        Xnode endNode;

        // Generate host to device movement
        if ((_clawStart.getUpdateClauseValue() == DataMovement.TWO_WAY
                || _clawStart.getUpdateClauseValue() == DataMovement.HOST_TO_DEVICE) && cfg.updateAtInput())
        {
            startNode = Directive.generateUpdate(xcodeml, hook.getStart(), readArrays, DataMovement.HOST_TO_DEVICE);
        } else
        {
            startNode = hook.getStart();
        }

        // Generate device to host movement
        if ((_clawStart.getUpdateClauseValue() == DataMovement.TWO_WAY
                || _clawStart.getUpdateClauseValue() == DataMovement.DEVICE_TO_HOST) && cfg.updateAtOutput())
        {
            endNode = Directive.generateUpdate(xcodeml, hook.getEnd(), writtenArrays, DataMovement.DEVICE_TO_HOST);
        } else
        {
            endNode = hook.getEnd();
        }
        return new Xblock(startNode, endNode);
    }

    /**
     * Generate serialization savepoints before and after the expand block.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @param hook    Block around which serialization are generated
     */
    private Xblock generateSavepoint(Configuration cfg, XcodeProgram xcodeml, Xblock hook, List<String> readArrays,
            List<String> writtenArrays)
    {
        final Context context = xcodeml.context();
        Serialization.insertImports(cfg, xcodeml, hook.getStart().findParentFunction());

        Xnode start = null;
        Xnode end;

        if (context.isTarget(Target.GPU))
        {
            // Read inputs
            start = Serialization.generateReadSavepoint(cfg, xcodeml, hook.getStart(), _clawStart.getMetadataMap(),
                    readArrays, _clawStart.value(ClawClause.SAVEPOINT), SerializationStep.SER_IN);
        } else if (context.isTarget(Target.CPU))
        {
            // Write inputs
            start = Serialization.generateWriteSavepoint(cfg, xcodeml, hook.getStart(), _clawStart.getMetadataMap(),
                    readArrays, _clawStart.value(ClawClause.SAVEPOINT), SerializationStep.SER_IN);
        }

        // Write outputs
        end = Serialization.generateWriteSavepoint(cfg, xcodeml, hook.getEnd(), _clawStart.getMetadataMap(),
                writtenArrays, _clawStart.value(ClawClause.SAVEPOINT), SerializationStep.SER_OUT);

        return new Xblock(start, end);
    }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Element;

import claw.shenron.transformation.DependentTransformationGroup;
import claw.shenron.transformation.IndependentTransformationGroup;
import claw.shenron.transformation.Transformation;
import claw.shenron.transformation.TransformationGroup;
import claw.shenron.translator.Translator;
import claw.tatsu.analysis.topology.DirectedGraph;
import claw.tatsu.analysis.topology.TopologicalSort;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.common.Target;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.internal.OpenAccContinuation;
import claw.wani.transformation.ll.caching.Kcaching;
import claw.wani.transformation.ll.directive.DirectivePrimitive;
import claw.wani.transformation.ll.loop.ExpandNotation;
import claw.wani.transformation.ll.loop.IfExtract;
import claw.wani.transformation.ll.loop.LoopExtraction;
import claw.wani.transformation.ll.loop.LoopFusion;
import claw.wani.transformation.ll.loop.LoopHoist;
import claw.wani.transformation.ll.loop.LoopInterchange;
import claw.wani.transformation.ll.utility.ArrayToFctCall;
import claw.wani.transformation.ll.utility.UtilityRemove;
import claw.wani.transformation.sca.ModelData;
import claw.wani.transformation.sca.ScaCPUvectorizeGroup;
import claw.wani.transformation.sca.ScaForward;
import claw.wani.transformation.sca.ScaGPU;
import claw.wani.transformation.sca.ScaRoutine;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.GroupConfiguration;

/**
 * ClawTranslator stores all transformation groups applied during the
 * translation.
 *
 * @author clementval
 */

public class ClawTranslator implements Translator
{

    // Hold all transformation groups
    private final Map<Class<?>, TransformationGroup> _tGroups;
    // Hold cross-transformation elements
    private final Map<Element, Object> _crossTransformationTable;
    private final Map<ClawDirectiveKey, Deque<ClawPragma>> _blockDirectives;
    private int _transformationCounter = 0;
    private final Configuration _cfg;
    private final Context _context;

    public Configuration cfg()
    {
        return _cfg;
    }

    public Context context()
    {
        return _context;
    }

    /**
     * ClawTranslator ctor. Creates the transformation groups needed for the CLAW
     * transformation and order the accordingly to their interpretation order.
     */
    public ClawTranslator(Configuration cfg, Context context)
    {
        _cfg = cfg;
        _context = context;
        /*
         * Use LinkedHashMap to be able to iterate through the map entries with the
         * insertion order.
         */
        _tGroups = new LinkedHashMap<>();
        for (GroupConfiguration g : cfg().getGroups())
        {
            if (g.getType() == GroupConfiguration.GroupType.DEPENDENT)
            {
                _tGroups.put(g.getTransformationClass(), new DependentTransformationGroup(g.getName()));
            } else
            {
                _tGroups.put(g.getTransformationClass(), new IndependentTransformationGroup(g.getName()));
            }
        }

        // Internal transformations not specified by default configuration or user
        _tGroups.put(OpenAccContinuation.class, new IndependentTransformationGroup("internal-open-acc-continuation"));

        _crossTransformationTable = new HashMap<>();

        _blockDirectives = new Hashtable<>();
    }

    @Override
    public void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
            throws IllegalTransformationException, IllegalDirectiveException
    {
        // Analyze the raw pragma with the CLAW language parser
        ClawPragma analyzedPragma = ClawPragma.analyze(pragma);
        if (analyzedPragma.hasErrors())
        {
            for (String err : analyzedPragma.getErrors())
            {
                xcodeml.addError(err, analyzedPragma.getPragma().lineNo());
            }
            throw new IllegalDirectiveException(analyzedPragma.getDirective().toString(),
                    "Errors detected during directive analysis.", analyzedPragma.getPragma().lineNo());
        }

        // Create transformation object based on the directive
        switch (analyzedPragma.getDirective())
        {
        case ARRAY_TO_CALL:
            addTransformation(xcodeml, new ArrayToFctCall(analyzedPragma));
            break;
        case KCACHE:
            addTransformation(xcodeml, new Kcaching(analyzedPragma));
            break;
        case LOOP_FUSION:
            addTransformation(xcodeml, new LoopFusion(analyzedPragma));
            break;
        case LOOP_INTERCHANGE:
            addTransformation(xcodeml, new LoopInterchange(analyzedPragma));
            break;
        case LOOP_EXTRACT:
            addTransformation(xcodeml, new LoopExtraction(analyzedPragma));
            break;
        case PRIMITIVE:
            addTransformation(xcodeml, new DirectivePrimitive(analyzedPragma));
            break;
        case IF_EXTRACT:
            addTransformation(xcodeml, new IfExtract(analyzedPragma));
            break;
        case SCA:
            addScaTransformation(xcodeml, analyzedPragma);
            break;
        case LOOP_HOIST:
        case EXPAND:
        case REMOVE:
        case MODEL_DATA:
            handleBlockDirective(xcodeml, analyzedPragma);
            break;
        // driver handled directives
        case IGNORE:
        case VERBATIM:
        case NO_DEP:
            break;
        default:
            throw new IllegalDirectiveException(null, "Unrecognized CLAW directive", pragma.lineNo());
        }
    }

    /**
     * Create specific SCA transformation.
     *
     * @param xcodeml        Current translation unit.
     * @param analyzedPragma Analyzed pragma object.
     * @throws IllegalTransformationException If transformation cannot be created.
     */
    private void addScaTransformation(XcodeProgram xcodeml, ClawPragma analyzedPragma)
            throws IllegalTransformationException
    {
        if (analyzedPragma.hasClause(ClawClause.FORWARD))
        {
            addTransformation(xcodeml, new ScaForward(analyzedPragma));
        } else if (analyzedPragma.hasClause(ClawClause.ROUTINE))
        {
            addTransformation(xcodeml, new ScaRoutine(analyzedPragma));
        } else
        {
            if (context().getTarget() == Target.GPU)
            {
                addTransformation(xcodeml, new ScaGPU(analyzedPragma));
            } else
            {
                if (cfg().getParameter(Configuration.CPU_STRATEGY).equalsIgnoreCase(Configuration.CPU_STRATEGY_FUSION))
                {
                    addTransformation(xcodeml, new ScaCPUvectorizeGroup(analyzedPragma, true));
                } else
                {
                    addTransformation(xcodeml, new ScaCPUvectorizeGroup(analyzedPragma));
                }
            }
        }
    }

    @Override
    public void finalizeTranslation(XcodeProgram xcodeml) throws IllegalTransformationException
    {
        // Clean up block transformation map
        for (Map.Entry<ClawDirectiveKey, Deque<ClawPragma>> entry : _blockDirectives.entrySet())
        {
            for (ClawPragma pragma : entry.getValue())
            {
                createBlockDirectiveTransformation(xcodeml, pragma, null);
            }
        }

        reorderTransformations();
    }

    /**
     * Associate correctly the start and end directive to form blocks.
     *
     * @param xcodeml        Current translation unit.
     * @param analyzedPragma Analyzed pragma object to be handle.
     */
    private void handleBlockDirective(XcodeProgram xcodeml, ClawPragma analyzedPragma)
            throws IllegalDirectiveException, IllegalTransformationException
    {
        int depth = analyzedPragma.getPragma().depth();
        ClawDirectiveKey crtRemoveKey = new ClawDirectiveKey(analyzedPragma.getDirective(), depth);
        if (analyzedPragma.isEndPragma())
        {
            ClawPragma start = getBlockDirectiveStart(crtRemoveKey);
            if (start == null)
            {
                throw new IllegalDirectiveException(analyzedPragma.getDirective().name(),
                        "Invalid CLAW directive (end with no start)", analyzedPragma.getPragma().lineNo());
            } else
            {
                createBlockDirectiveTransformation(xcodeml, start, analyzedPragma);
            }
        } else
        {
            addStartBlockDirective(analyzedPragma, crtRemoveKey);
        }
    }

    /**
     * Return the start directive object corresponding to the end directive and
     * depth.
     *
     * @param key Key representing the directive and depth values.
     * @return ClawPragma representing the start directive if any. Null otherwise.
     */
    private ClawPragma getBlockDirectiveStart(ClawDirectiveKey key)
    {
        if (_blockDirectives.containsKey(key))
        {
            ClawPragma startPragma = _blockDirectives.get(key).pop();

            if (_blockDirectives.get(key).isEmpty())
            {
                _blockDirectives.remove(key);
            }
            return startPragma;
        }
        return null;
    }

    /**
     * Add new start directive that can have an end directive.
     *
     * @param analyzedPragma ClawPragma representing the start directive.
     * @param key            Key representing the directive and depth values.
     */
    private void addStartBlockDirective(ClawPragma analyzedPragma, ClawDirectiveKey key)
    {
        if (key == null || analyzedPragma == null)
        {
            return;
        }
        if (_blockDirectives.containsKey(key))
        {
            _blockDirectives.get(key).push(analyzedPragma);
        } else
        {
            Deque<ClawPragma> stack = new ArrayDeque<>();
            stack.push(analyzedPragma);
            _blockDirectives.put(key, stack);
        }
    }

    /**
     * Create a new block transformation object according to its start directive.
     *
     * @param xcodeml Current translation unit.
     * @param begin   Begin directive which starts the block.
     * @param end     End directive which ends the block.
     */
    private void createBlockDirectiveTransformation(XcodeProgram xcodeml, ClawPragma begin, ClawPragma end)
            throws IllegalTransformationException
    {
        if (begin == null || !begin.isApplicableToCurrentTarget(context()))
        {
            return;
        }
        switch (begin.getDirective())
        {
        case REMOVE:
            addTransformation(xcodeml, new UtilityRemove(begin, end));
            break;
        case EXPAND:
            addTransformation(xcodeml, new ExpandNotation(begin, end));
            break;
        case LOOP_HOIST:
            addTransformation(xcodeml, new LoopHoist(begin, end));
            break;
        case MODEL_DATA:
            addTransformation(xcodeml, new ModelData(begin, end));
            break;
        default:
            throw new IllegalTransformationException("Unknown block transformation", begin.getPragma().lineNo());
        }
    }

    /**
     * Add transformation in the correct group.
     *
     * @param xcodeml Current translation unit.
     * @param t       Transformation to be added.
     */
    public void addTransformation(XcodeProgram xcodeml, Transformation t) throws IllegalTransformationException
    {
        if (t.getDirective() instanceof ClawPragma
                && !((ClawPragma) t.getDirective()).isApplicableToCurrentTarget(context()))
        {
            return;
        }
        if (t.analyze(xcodeml, this))
        {
            if (_tGroups.containsKey(t.getClass()))
            {
                _tGroups.get(t.getClass()).add(t);
            } else if (_tGroups.containsKey(t.getClass().getSuperclass()))
            {
                _tGroups.get(t.getClass().getSuperclass()).add(t);
            }
        } else if (t.abortOnFailedAnalysis())
        {
            throw new IllegalTransformationException(
                    "Analysis for transformation failed. " + "See errors for more information.",
                    t.getDirective().getPragma().lineNo());
        }
    }

    @Override
    public boolean isHandledPragma(Xnode pragma)
    {
        return ClawPragma.startsWithClaw(pragma);
    }

    /**
     *
     */
    private void reorderTransformations()
    {
        if (getGroups().containsKey(ScaForward.class))
        {
            TransformationGroup tg = getGroups().get(ScaForward.class);

            if (tg.count() <= 1)
            {
                return;
            }

            DirectedGraph<Transformation> dg = new DirectedGraph<>();
            Map<String, List<Transformation>> fctMap = new HashMap<>();

            for (Transformation t : tg.getTransformations())
            {
                ScaForward p = (ScaForward) t;
                dg.addNode(p);
                if (fctMap.containsKey(p.getCallingFctName()))
                {
                    List<Transformation> tList = fctMap.get(p.getCallingFctName());
                    tList.add(p);
                } else
                {
                    List<Transformation> tList = new ArrayList<>();
                    tList.add(p);
                    fctMap.put(p.getCallingFctName(), tList);
                }
            }

            for (Transformation t : tg.getTransformations())
            {
                ScaForward p = (ScaForward) t;
                if (p.getCalledFctName() != null && fctMap.containsKey(p.getCalledFctName()))
                {
                    List<Transformation> tList = fctMap.get(p.getCalledFctName());
                    for (Transformation end : tList)
                    {
                        dg.addEdge(p, end);
                    }
                }
            }

            List<Transformation> ordered = TopologicalSort.sort(TopologicalSort.reverseGraph(dg));
            tg.setTransformations(ordered);
        }

    }

    /**
     * Generate corresponding additional transformation according to optional
     * clauses given to the directive.
     *
     * @param claw    ClawPragma object that tells encapsulates all information
     *                about the current directives and its clauses.
     * @param xcodeml Current XcodeML program.
     * @param stmt    Statement on which the transformation is attached.
     * @throws IllegalTransformationException If transformation cannot be generated.
     */
    public void generateAdditionalTransformation(ClawPragma claw, XcodeProgram xcodeml, Xnode stmt)
            throws IllegalTransformationException
    {
        // Order doesn't matter
        applyFusionClause(claw, xcodeml, stmt);
        applyInterchangeClause(claw, xcodeml, stmt);
    }

    /**
     * Generate loop interchange transformation if the clause is present in the
     * directive.
     *
     * @param claw    ClawPragma object that tells encapsulates all information
     *                about the current directives and its clauses.
     * @param xcodeml Current XcodeML program.
     * @param stmt    Statement on which the transformation is attached. Must be a
     *                FdoStatement for the loop interchange transformation.
     */
    private void applyInterchangeClause(ClawPragma claw, XcodeProgram xcodeml, Xnode stmt)
            throws IllegalTransformationException
    {
        if (claw.hasClause(ClawClause.INTERCHANGE) && Xnode.isOfCode(stmt, Xcode.F_DO_STATEMENT))
        {
            Xnode p = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
            stmt.insertBefore(p);
            ClawPragma l = ClawPragma.createLoopInterchangeLanguage(claw, p);
            LoopInterchange interchange = new LoopInterchange(l);
            addTransformation(xcodeml, interchange);
            Message.debug(context(), "Loop interchange added: " + claw.values(ClawClause.INTERCHANGE_INDEXES));
        }
    }

    /**
     * Generate loop fusion transformation if the clause is present in the
     * directive.
     *
     * @param claw    ClawPragma object that tells encapsulates all information
     *                about the current directives and its clauses.
     * @param xcodeml Current XcodeML program.
     * @param stmt    Statement on which the transformation is attached. Must be a
     *                FdoStatement for the loop fusion transformation.
     */
    private void applyFusionClause(ClawPragma claw, XcodeProgram xcodeml, Xnode stmt)
            throws IllegalTransformationException
    {
        if (claw.hasClause(ClawClause.FUSION) && Xnode.isOfCode(stmt, Xcode.F_DO_STATEMENT))
        {
            ClawPragma l = ClawPragma.createLoopFusionLanguage(claw);
            addTransformation(xcodeml, new LoopFusion(stmt, l));
            Message.debug(context(), "Loop fusion added: " + claw.value(ClawClause.GROUP));
        }
    }

    /**
     * @see Translator#getGroups()
     */
    public Map<Class<?>, TransformationGroup> getGroups()
    {
        return _tGroups;
    }

    /**
     * Get the next extraction counter value.
     *
     * @return Transformation counter value.
     */
    public int getNextTransformationCounter()
    {
        int currentCounter = _transformationCounter;
        ++_transformationCounter;
        return currentCounter;
    }

    /**
     * Get a stored element from a previous transformation.
     *
     * @param key Key to use to retrieve the element.
     * @return The stored element if present. Null otherwise.
     */
    public Object hasElement(Xnode key)
    {
        if (_crossTransformationTable.containsKey(key.element()))
        {
            return _crossTransformationTable.get(key.element());
        }
        return null;
    }

    /**
     * Store a Xnode from a transformation for a possible usage in another
     * transformation. If a key is already present, the element is overwritten.
     *
     * @param key   The element acting as a key.
     * @param value The element to be stored.
     */
    public void storeElement(Xnode key, Object value)
    {
        _crossTransformationTable.remove(key.element());
        _crossTransformationTable.put(key.element(), value);
    }
}

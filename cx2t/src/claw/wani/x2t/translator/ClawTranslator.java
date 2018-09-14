/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import claw.shenron.transformation.DependentTransformationGroup;
import claw.shenron.transformation.IndependentTransformationGroup;
import claw.shenron.transformation.Transformation;
import claw.shenron.transformation.TransformationGroup;
import claw.shenron.translator.Translator;
import claw.tatsu.analysis.topology.DirectedGraph;
import claw.tatsu.analysis.topology.TopologicalSort;
import claw.tatsu.common.Message;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.internal.OpenAccContinuation;
import claw.wani.transformation.ll.caching.Kcaching;
import claw.wani.transformation.ll.directive.DirectivePrimitive;
import claw.wani.transformation.ll.loop.*;
import claw.wani.transformation.ll.utility.ArrayToFctCall;
import claw.wani.transformation.ll.utility.UtilityRemove;
import claw.wani.transformation.sca.Parallelize;
import claw.wani.transformation.sca.ParallelizeForward;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.GroupConfiguration;
import org.w3c.dom.Element;

import java.util.*;

/**
 * ClawTranslator stores all transformation groups applied during the
 * translation.
 *
 * @author clementval
 */

public class ClawTranslator implements Translator {

  // Hold all transformation groups
  private final Map<Class, TransformationGroup> _tGroups;
  // Hold cross-transformation elements
  private final Map<Element, Object> _crossTransformationTable;
  private final Map<ClawDirectiveKey, ClawPragma> _blockDirectives;
  private int _transformationCounter = 0;

  /**
   * ClawTranslator ctor. Creates the transformation groups needed for the CLAW
   * transformation and order the accordingly to their interpretation order.
   */
  public ClawTranslator() {
    /*
     * Use LinkedHashMap to be able to iterate through the map
     * entries with the insertion order.
     */
    _tGroups = new LinkedHashMap<>();
    for(GroupConfiguration g : Configuration.get().getGroups()) {
      switch(g.getType()) {
        case DEPENDENT:
          _tGroups.put(g.getTransformationClass(),
              new DependentTransformationGroup(g.getName()));
          break;
        case INDEPENDENT:
          _tGroups.put(g.getTransformationClass(),
              new IndependentTransformationGroup(g.getName()));
          break;
      }
    }

    // Internal transformations not specified by default configuration or user
    _tGroups.put(OpenAccContinuation.class,
        new IndependentTransformationGroup("internal-open-acc-continuation"));

    _crossTransformationTable = new HashMap<>();

    _blockDirectives = new Hashtable<>();
  }

  @Override
  public void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
      throws IllegalTransformationException, IllegalDirectiveException
  {
    // Analyze the raw pragma with the CLAW language parser
    ClawPragma analyzedPragma = ClawPragma.analyze(pragma);

    // Create transformation object based on the directive
    switch(analyzedPragma.getDirective()) {
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
      case LOOP_HOIST:
        HandleBlockDirective(xcodeml, analyzedPragma);
        break;
      case ARRAY_TRANSFORM:
        HandleBlockDirective(xcodeml, analyzedPragma);
        break;
      case REMOVE:
        HandleBlockDirective(xcodeml, analyzedPragma);
        break;
      case PARALLELIZE:
        if(analyzedPragma.hasForwardClause()) {
          addTransformation(xcodeml, new ParallelizeForward(analyzedPragma));
        } else {
          addTransformation(xcodeml, new Parallelize(analyzedPragma));
        }
        break;
      case PRIMITIVE:
        addTransformation(xcodeml, new DirectivePrimitive(analyzedPragma));
        break;
      case IF_EXTRACT:
        addTransformation(xcodeml, new IfExtract(analyzedPragma));
        break;
      // driver handled directives
      case IGNORE:
      case VERBATIM:
      case NO_DEP:
        break;
      default:
        throw new IllegalDirectiveException(null, "Unrecognized CLAW directive",
            pragma.lineNo());
    }
  }

  @Override
  public void finalizeTranslation(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    // Clean up block transformation map
    for(Map.Entry<ClawDirectiveKey, ClawPragma> entry :
        _blockDirectives.entrySet()) {
      createBlockDirectiveTransformation(xcodeml, entry.getValue(), null);
    }

    reorderTransformations();
  }

  /**
   * Associate correctly the start and end directive to form blocks.
   *
   * @param xcodeml        Current translation unit.
   * @param analyzedPragma Analyzed pragma object to be handle.
   */
  private void HandleBlockDirective(XcodeProgram xcodeml,
                                    ClawPragma analyzedPragma)
      throws IllegalDirectiveException, IllegalTransformationException
  {
    int depth = analyzedPragma.getPragma().depth();
    ClawDirectiveKey crtRemoveKey =
        new ClawDirectiveKey(analyzedPragma.getDirective(), depth);
    if(analyzedPragma.isEndPragma()) { // start block directive
      if(!_blockDirectives.containsKey(crtRemoveKey)) {
        throw new
            IllegalDirectiveException(analyzedPragma.getDirective().name(),
            "Invalid Claw directive (end with no start)",
            analyzedPragma.getPragma().lineNo());
      } else {
        createBlockDirectiveTransformation(xcodeml,
            _blockDirectives.get(crtRemoveKey), analyzedPragma);
        _blockDirectives.remove(crtRemoveKey);
      }
    } else { // end block directive
      if(_blockDirectives.containsKey(crtRemoveKey)) {
        createBlockDirectiveTransformation(xcodeml,
            _blockDirectives.get(crtRemoveKey), null);
      }
      _blockDirectives.remove(crtRemoveKey);
      _blockDirectives.put(crtRemoveKey, analyzedPragma);
    }
  }

  /**
   * Create a new block transformation object according to its start directive.
   *
   * @param xcodeml Current translation unit.
   * @param begin   Begin directive which starts the block.
   * @param end     End directive which ends the block.
   */
  private void createBlockDirectiveTransformation(XcodeProgram xcodeml,
                                                  ClawPragma begin,
                                                  ClawPragma end)
      throws IllegalTransformationException
  {
    if(begin == null || !begin.isApplicableToCurrentTarget()) {
      return;
    }
    switch(begin.getDirective()) {
      case REMOVE:
        addTransformation(xcodeml, new UtilityRemove(begin, end));
        break;
      case ARRAY_TRANSFORM:
        addTransformation(xcodeml, new ArrayTransform(begin, end));
        break;
      case LOOP_HOIST:
        addTransformation(xcodeml, new LoopHoist(begin, end));
        break;
    }
  }

  /**
   * Add transformation in the correct group.
   *
   * @param xcodeml Current translation unit.
   * @param t       Transformation to be added.
   */
  public void addTransformation(XcodeProgram xcodeml, Transformation t)
      throws IllegalTransformationException
  {
    if(t.getDirective() != null
        && t.getDirective() instanceof ClawPragma
        && !((ClawPragma) t.getDirective()).isApplicableToCurrentTarget())
    {
      return;
    }
    if(t.analyze(xcodeml, this)) {
      if(_tGroups.containsKey(t.getClass())) {
        _tGroups.get(t.getClass()).add(t);
      }
    } else if(t.abortOnFailedAnalysis()) {
      throw new IllegalTransformationException(
          "Analysis for transformation failed. " +
              "See errors for more information.",
          t.getDirective().getPragma().lineNo());
    }
  }

  @Override
  public boolean isHandledPragma(Xnode pragma) {
    return ClawPragma.startsWithClaw(pragma);
  }

  /**
   *
   */
  private void reorderTransformations() {
    if(getGroups().containsKey(ParallelizeForward.class)) {
      TransformationGroup tg = getGroups().get(ParallelizeForward.class);

      if(tg.count() <= 1) {
        return;
      }

      DirectedGraph<Transformation> dg = new DirectedGraph<>();
      Map<String, List<Transformation>> fctMap = new HashMap<>();

      for(Transformation t : tg.getTransformations()) {
        ParallelizeForward p = (ParallelizeForward) t;
        dg.addNode(p);
        if(fctMap.containsKey(p.getCallingFctName())) {
          List<Transformation> tList = fctMap.get(p.getCallingFctName());
          tList.add(p);
        } else {
          List<Transformation> tList = new ArrayList<>();
          tList.add(p);
          fctMap.put(p.getCallingFctName(), tList);
        }
      }

      for(Transformation t : tg.getTransformations()) {
        ParallelizeForward p = (ParallelizeForward) t;
        if(p.getCalledFctName() != null) {
          if(fctMap.containsKey(p.getCalledFctName())) {
            List<Transformation> tList = fctMap.get(p.getCalledFctName());
            for(Transformation end : tList) {
              dg.addEdge(p, end);
            }
          }
        }
      }

      List<Transformation> ordered =
          TopologicalSort.sort(TopologicalSort.reverseGraph(dg));
      tg.setTransformations(ordered);
    }
  }

  /**
   * Generate corresponding additional transformation according to optional
   * clauses given to the directive.
   *
   * @param claw    ClawPragma object that tells encapsulates all
   *                information about the current directives and its
   *                clauses.
   * @param xcodeml Current XcodeML program.
   * @param stmt    Statement on which the transformation is attached.
   * @throws IllegalTransformationException If transformation cannot be
   *                                        generated.
   */
  public void generateAdditionalTransformation(ClawPragma claw,
                                               XcodeProgram xcodeml, Xnode stmt)
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
   * @param claw    ClawPragma object that tells encapsulates all
   *                information about the current directives and its
   *                clauses.
   * @param xcodeml Current XcodeML program.
   * @param stmt    Statement on which the transformation is attached. Must
   *                be a FdoStatement for the loop interchange
   *                transformation.
   */
  private void applyInterchangeClause(ClawPragma claw, XcodeProgram xcodeml,
                                      Xnode stmt)
      throws IllegalTransformationException
  {
    if(claw.hasInterchangeClause() && stmt.opcode() == Xcode.F_DO_STATEMENT) {
      Xnode p = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
      stmt.insertBefore(p);
      ClawPragma l = ClawPragma.createLoopInterchangeLanguage(claw, p);
      LoopInterchange interchange = new LoopInterchange(l);
      addTransformation(xcodeml, interchange);
      Message.debug("Loop interchange added: " + claw.getIndexes());
    }
  }

  /**
   * Generate loop fusion transformation if the clause is present in the
   * directive.
   *
   * @param claw    ClawPragma object that tells encapsulates all
   *                information about the current directives and its
   *                clauses.
   * @param xcodeml Current XcodeML program.
   * @param stmt    Statement on which the transformation is attached. Must
   *                be a FdoStatement for the loop fusion transformation.
   */
  private void applyFusionClause(ClawPragma claw, XcodeProgram xcodeml,
                                 Xnode stmt)
      throws IllegalTransformationException
  {
    if(claw.hasFusionClause() && stmt.opcode() == Xcode.F_DO_STATEMENT) {
      ClawPragma l = ClawPragma.createLoopFusionLanguage(claw);
      addTransformation(xcodeml, new LoopFusion(stmt, l));
      Message.debug("Loop fusion added: " + claw.getGroupValue());
    }
  }

  /**
   * @see Translator#getGroups()
   */
  public Map<Class, TransformationGroup> getGroups() {
    return _tGroups;
  }

  /**
   * Get the next extraction counter value.
   *
   * @return Transformation counter value.
   */
  public int getNextTransformationCounter() {
    return _transformationCounter++;
  }

  /**
   * Get a stored element from a previous transformation.
   *
   * @param key Key to use to retrieve the element.
   * @return The stored element if present. Null otherwise.
   */
  public Object hasElement(Xnode key) {
    if(_crossTransformationTable.containsKey(key.element())) {
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
  public void storeElement(Xnode key, Object value) {
    _crossTransformationTable.remove(key.element());
    _crossTransformationTable.put(key.element(), value);
  }
}

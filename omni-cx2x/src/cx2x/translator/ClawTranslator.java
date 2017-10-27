/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

import cx2x.translator.common.topology.DirectedGraph;
import cx2x.translator.common.topology.TopologicalSort;
import cx2x.translator.config.Configuration;
import cx2x.translator.config.GroupConfiguration;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.claw.ArrayToFctCall;
import cx2x.translator.transformation.claw.Kcaching;
import cx2x.translator.transformation.claw.parallelize.Parallelize;
import cx2x.translator.transformation.claw.parallelize.ParallelizeForward;
import cx2x.translator.transformation.loop.*;
import cx2x.translator.transformation.openacc.DirectivePrimitive;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
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
  // Hold the module file cache
  private final ModuleCache _modCache;
  private final Map<ClawDirectiveKey, ClawLanguage> _blockDirectives;
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

    _modCache = new ModuleCache();

    _blockDirectives = new Hashtable<>();
  }

  @Override
  public void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
      throws IllegalTransformationException, IllegalDirectiveException
  {
    // Analyze the raw pragma with the CLAW language parser
    ClawLanguage analyzedPragma = ClawLanguage.analyze(pragma,
        Configuration.get().getAcceleratorGenerator(),
        Configuration.get().getCurrentTarget());

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
  public void finalize(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    // Clean up block transformation map
    for(Map.Entry<ClawDirectiveKey, ClawLanguage> entry :
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
                                    ClawLanguage analyzedPragma)
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
                                                  ClawLanguage begin,
                                                  ClawLanguage end)
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
        && t.getDirective() instanceof ClawLanguage
        && !((ClawLanguage) t.getDirective()).isApplicableToCurrentTarget())
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
    return ClawLanguage.startsWithClaw(pragma);
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
   * @see Translator#getModCache()
   */
  @Override
  public ModuleCache getModCache() {
    return _modCache;
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
    if(_crossTransformationTable.containsKey(key.element())) {
      _crossTransformationTable.remove(key.element());
    }
    _crossTransformationTable.put(key.element(), value);
  }
}

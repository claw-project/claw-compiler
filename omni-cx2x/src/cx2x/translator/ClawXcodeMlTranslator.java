/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

// Cx2x import

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.common.topology.DirectedGraph;
import cx2x.translator.common.topology.TopologicalSort;
import cx2x.translator.config.Configuration;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.transformation.claw.ArrayToFctCall;
import cx2x.translator.transformation.claw.Kcaching;
import cx2x.translator.transformation.claw.parallelize.Parallelize;
import cx2x.translator.transformation.claw.parallelize.ParallelizeForward;
import cx2x.translator.transformation.loop.*;
import cx2x.translator.transformation.openacc.DirectivePrimitive;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.translator.transformation.utility.XcodeMLWorkaround;
import cx2x.translator.transformer.ClawTransformer;
import cx2x.xcodeml.error.XanalysisError;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.TransformationGroup;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import xcodeml.util.XmOption;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;

// OMNI import


/**
 * ClawXcodeMlTranslator is the class driving the translation. It analyzes the
 * CLAW directives and applies the corresponding transformation to the XcodeML/F
 * intermediate representation.
 *
 * @author clementval
 */

public class ClawXcodeMlTranslator {

  private static final String ERROR_PREFIX = "claw-error: ";
  private static final String WARNING_PREFIX = "claw warning: ";
  private final Map<ClawDirectiveKey, ClawLanguage> _blockDirectives;
  private final AcceleratorGenerator _generator;
  private final Target _target;
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;
  private ClawTransformer _transformer = null;
  private XcodeProgram _program = null;

  /**
   * ClawXcodeMlTranslator ctor.
   *
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   * @param config            Configuration information object.
   * @param maxColumns        Maximum number of columns.
   */
  public ClawXcodeMlTranslator(String xcodemlInputFile,
                               String xcodemlOutputFile,
                               Configuration config,
                               int maxColumns)
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _transformer = new ClawTransformer(config, maxColumns);
    _blockDirectives = new Hashtable<>();
    _target = config.getCurrentTarget();
    _generator = AcceleratorHelper.createAcceleratorGenerator(config);
  }

  /**
   * Analysis the XcodeML code and produce a list of applicable transformation.
   */
  public void analyze() {
    _program = XcodeProgram.createFromFile(_xcodemlInputFile);
    if(_program == null) {
      abort();
    }

    // Check all pragma found in the program
    for(Xnode pragma : _program.getAllStmt(Xcode.FPRAGMASTATEMENT)) {

      // pragma does not start with the CLAW prefix
      if(!ClawLanguage.startsWithClaw(pragma)) {
        // Compile guard removal
        if(_generator != null && _generator.isCompileGuard(pragma.value())) {
          pragma.delete();
        }
        // Handle special transformation of OpenACC line continuation
        else if(pragma.value().
            toLowerCase().startsWith(ClawConstant.OPENACC_PREFIX))
        {
          OpenAccContinuation t =
              new OpenAccContinuation(new AnalyzedPragma(pragma));
          addOrAbort(t);
        }

        // Not CLAW pragma, we do nothing
        continue;
      }
      try {
        // Analyze the raw pragma with the CLAW language parser
        ClawLanguage analyzedPragma =
            ClawLanguage.analyze(pragma, _generator, _target);

        // Create transformation object based on the directive
        switch(analyzedPragma.getDirective()) {
          case ARRAY_TO_CALL:
            addOrAbort(new ArrayToFctCall(analyzedPragma));
            break;
          case KCACHE:
            addOrAbort(new Kcaching(analyzedPragma));
            break;
          case LOOP_FUSION:
            addOrAbort(new LoopFusion(analyzedPragma));
            break;
          case LOOP_INTERCHANGE:
            addOrAbort(new LoopInterchange(analyzedPragma));
            break;
          case LOOP_EXTRACT:
            addOrAbort(new LoopExtraction(analyzedPragma));
            break;
          case LOOP_HOIST:
            HandleBlockDirective(analyzedPragma);
            break;
          case ARRAY_TRANSFORM:
            HandleBlockDirective(analyzedPragma);
            break;
          case REMOVE:
            HandleBlockDirective(analyzedPragma);
            break;
          case PARALLELIZE:
            if(analyzedPragma.hasForwardClause()) {
              addOrAbort(new ParallelizeForward(analyzedPragma));
            } else {
              addOrAbort(new Parallelize(analyzedPragma));
            }
            break;
          case PRIMITIVE:
            addOrAbort(new DirectivePrimitive(analyzedPragma));
            break;
          // driver handled directives
          case IGNORE:
          case VERBATIM:
            break;
          default:
            _program.addError("Unrecognized CLAW directive",
                pragma.lineNo());
            abort();
        }
      } catch(IllegalDirectiveException ex) {
        System.err.println(ex.getMessage());
        abort();
      }
    }

    // Clean up block transformation map
    for(Map.Entry<ClawDirectiveKey, ClawLanguage> entry :
        _blockDirectives.entrySet()) {
      createBlockDirectiveTransformation(entry.getValue(), null);
    }

    // Add utility transformation
    addOrAbort(new XcodeMLWorkaround(new ClawLanguage(_program)));

    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }


  /**
   * Associate correctly the start and end directive to form blocks.
   *
   * @param analyzedPragma Analyzed pragma object to be handle.
   */
  private void HandleBlockDirective(ClawLanguage analyzedPragma) {
    int depth = analyzedPragma.getPragma().depth();
    ClawDirectiveKey crtRemoveKey =
        new ClawDirectiveKey(analyzedPragma.getDirective(), depth);
    if(analyzedPragma.isEndPragma()) { // start block directive
      if(!_blockDirectives.containsKey(crtRemoveKey)) {
        _program.addError("Invalid Claw directive (end with no start)",
            analyzedPragma.getPragma().lineNo());
        abort();
      } else {
        createBlockDirectiveTransformation(_blockDirectives.get(crtRemoveKey),
            analyzedPragma);
        _blockDirectives.remove(crtRemoveKey);
      }
    } else { // end block directive
      if(_blockDirectives.containsKey(crtRemoveKey)) {
        createBlockDirectiveTransformation(_blockDirectives.get(crtRemoveKey),
            null);
      }
      _blockDirectives.remove(crtRemoveKey);
      _blockDirectives.put(crtRemoveKey, analyzedPragma);
    }
  }

  /**
   * Create a new block transformation object according to its start directive.
   *
   * @param begin Begin directive which starts the block.
   * @param end   End directive which ends the block.
   */
  private void createBlockDirectiveTransformation(ClawLanguage begin,
                                                  ClawLanguage end)
  {
    if(!begin.isApplicableToCurrentTarget()) {
      return;
    }
    switch(begin.getDirective()) {
      case REMOVE:
        addOrAbort(new UtilityRemove(begin, end));
        break;
      case ARRAY_TRANSFORM:
        addOrAbort(new ArrayTransform(begin, end));
        break;
      case LOOP_HOIST:
        addOrAbort(new LoopHoist(begin, end));
        break;
    }
  }

  /**
   * Add a transformation in the pipeline if the analysis is succeeded.
   * Otherwise, abort the translation.
   *
   * @param t The transformation to be analyzed and added.
   */
  private void addOrAbort(Transformation t) {
    if(t.getDirective() != null
        && t.getDirective() instanceof ClawLanguage
        && !((ClawLanguage) t.getDirective()).isApplicableToCurrentTarget())
    {
      return;
    }
    if(t.analyze(_program, _transformer)) {
      _transformer.addTransformation(t);
    } else {
      abort();
    }
  }

  /**
   * Apply all the transformation in the pipeline.
   */
  public void transform() {
    try {
      if(!_canTransform) {
        _program.write(_xcodemlOutputFile, ClawConstant.INDENT_OUTPUT);
        return;
      }

      reorderTransformations();

      for(Map.Entry<Class, TransformationGroup> entry :
          _transformer.getGroups().entrySet()) {
        if(XmOption.isDebugOutput()) {
          System.out.println("Apply transformation: " +
              entry.getValue().transformationName() + " - " +
              entry.getValue().count()
          );
        }

        try {
          entry.getValue().applyTranslations(_program, _transformer);
          displayWarnings();
        } catch(IllegalTransformationException itex) {
          _program.addError(itex.getMessage(), itex.getStartLine());
          abort();
        } catch(Exception ex) {
          ex.printStackTrace();
          _program.addError("Unexpected error: " + ex.getMessage(), 0);
          if(XmOption.isDebugOutput()) {
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            _program.addError(errors.toString(), 0);
          }
          abort();
        }
      }

      // Write transformed IR to file
      _program.write(_xcodemlOutputFile, ClawConstant.INDENT_OUTPUT);
    } catch(Exception ex) {
      System.err.println("Transformation exception: " + ex.getMessage());
    }
  }

  private void reorderTransformations() {
    if(_transformer.getGroups().containsKey(ParallelizeForward.class)) {
      TransformationGroup tg =
          _transformer.getGroups().get(ParallelizeForward.class);

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
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  private void abort() {
    if(_program != null) {
      displayMessages(ERROR_PREFIX, _program.getErrors());
    }
    System.exit(1);
  }

  /**
   * Print all the warnings stored in the XcodeML object and purge them after
   * displaying.
   */
  private void displayWarnings() {
    displayMessages(WARNING_PREFIX, _program.getWarnings());
  }

  /**
   * Print all messages in the given list with the prefix.
   *
   * @param prefix   Prefix for the message.
   * @param messages List of messages to display.
   */
  private void displayMessages(String prefix, List<XanalysisError> messages) {
    for(XanalysisError message : messages) {
      if(message.getLine() == 0) {
        System.err.println(String.format("%s %s, line: undefined", prefix,
            message.getMessage()));
      } else {
        System.err.println(String.format("%s %s, line: %s", prefix,
            message.getMessage(), message.getConcatLines()));
      }
    }
    messages.clear();
  }

  /**
   * Flush all information stored in the translator.
   *
   * @param config Current configuration.
   */
  public void flush(Configuration config)
      throws IllegalTransformationException
  {
    String modPrefix = Utility.formattedModuleFilePrefix(
        config.getCurrentTarget(), config.getCurrentDirective());
    _transformer.getModCache().write(modPrefix, ClawConstant.INDENT_OUTPUT);
  }

}

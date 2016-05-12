/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

// Cx2x import
import cx2x.translator.common.Constant;
import cx2x.translator.common.GroupConfiguration;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.transformation.claw.ArrayToFctCall;
import cx2x.translator.transformation.claw.Kcaching;
import cx2x.translator.transformation.loop.*;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.xcodeml.error.*;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.transformation.*;
import cx2x.translator.transformer.*;

// OMNI import
import xcodeml.util.XmOption;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;


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
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;

  private ClawTransformer _transformer = null;
  private XcodeProgram _program = null;
  private final AcceleratorGenerator _generator;
  private final Target _target;

  private static final int INDENT_OUTPUT = 2; // Number of spaces for indent

  /**
   * ClawXcodeMlTranslator ctor.
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   * @param directive         Accelerator directlve language for code
   *                          generation.
   * @param target            Target influencing code transformation.
   * @param groups            Transformation groups configuration list.
   */
  public ClawXcodeMlTranslator(String xcodemlInputFile,
                               String xcodemlOutputFile,
                               AcceleratorDirective directive,
                               Target target,
                               List<GroupConfiguration> groups)
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _transformer = new ClawTransformer(groups);
    _blockDirectives = new Hashtable<>();
    _target = target;
    _generator = AcceleratorHelper.createAcceleratorGenerator(directive);
  }

  /**
   * Analysis the XcodeML code and produce a list of applicable transformation.
   */
  public void analyze() {
    _program = XcodeProgram.createFromFile(_xcodemlInputFile);
    if(_program == null){
      abort();
    }

    // Check all pragma found in the program
    for (Xpragma pragma :  XelementHelper.findAllPragmas(_program)){

      // pragma does not start with the CLAW prefix
      if(!ClawLanguage.startsWithClaw(pragma)){
        // Compile guard removal
        if(_generator != null && _generator.isCompileGuard(pragma.getValue())){
          pragma.delete();
        }
        // Handle special transformation of OpenACC line continuation
        else if(pragma.getValue().
            toLowerCase().startsWith(Constant.OPENACC_PREFIX))
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
        switch (analyzedPragma.getDirective()) {
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
          default:
            _program.addError("Unrecognized CLAW directive", pragma.getLineNo());
            abort();
        }
      } catch (IllegalDirectiveException ex){
        System.err.println(ex.getMessage());
        abort();
      }
    }

    // Clean up block transformation map
    for(Map.Entry<ClawDirectiveKey, ClawLanguage> entry :
        _blockDirectives.entrySet())
    {
      createBlockDirectiveTransformation(entry.getValue(), null);
    }

    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }


  /**
   * Associate correctly the start and end directive to form blocks.
   * @param analyzedPragma Analyzed pragma object to be handle.
   */
  private void HandleBlockDirective(ClawLanguage analyzedPragma){
    int depth =
        XelementHelper.getDepth(analyzedPragma.getPragma());
    ClawDirectiveKey crtRemoveKey =
        new ClawDirectiveKey(analyzedPragma.getDirective(), depth);
    if(analyzedPragma.isEndPragma()){ // start block directive
      if (!_blockDirectives.containsKey(crtRemoveKey)) {
        _program.addError("Invalid Claw directive (end with no start)",
            analyzedPragma.getPragma().getLineNo());
        abort();
      } else {
        createBlockDirectiveTransformation(_blockDirectives.get(crtRemoveKey),
            analyzedPragma);
        _blockDirectives.remove(crtRemoveKey);
      }
    } else { // end block directive
      if (_blockDirectives.containsKey(crtRemoveKey)) {
        createBlockDirectiveTransformation(_blockDirectives.get(crtRemoveKey),
            null);
      }
      _blockDirectives.remove(crtRemoveKey);
      _blockDirectives.put(crtRemoveKey, analyzedPragma);
    }
  }

  /**
   * Create a new block transformation object according to its start directive.
   * @param begin Begin directive which starts the block.
   * @param end   End directive which ends the block.
   */
  private void createBlockDirectiveTransformation(ClawLanguage begin,
                                                  ClawLanguage end)
  {
    switch (begin.getDirective()){
      case REMOVE:
        addOrAbort(new UtilityRemove(begin, end));
        break;
      case ARRAY_TRANSFORM:
        addOrAbort(new ArrayTransform(begin, end));
        break;
      case LOOP_HOIST:
        addOrAbort(new LoopHoist(begin, end));
    }
  }

  /**
   * Add a transformation in the pipeline if the analysis is succeded.
   * Otherwise, abort the translation.
   * @param t           The transformation to be analyzed and added.
   */
  private void addOrAbort(Transformation t)
  {
    if(t.analyze(_program, _transformer)){
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
      if(!_canTransform){
        if(!XelementHelper.writeXcodeML(_program,
            _xcodemlOutputFile, INDENT_OUTPUT))
        {
          abort();
        }
        return;
      }

      for (Map.Entry<Class, TransformationGroup> entry :
          _transformer.getGroups().entrySet())
      {
        if(XmOption.isDebugOutput()){
          System.out.println("Apply transfomation: " +
              entry.getValue().transformationName() + " - " +
              entry.getValue().count()
          );
        }

        try {
          entry.getValue().applyTranslations(_program, _transformer);
          displayWarnings();
        } catch (IllegalTransformationException itex) {
          _program.addError(itex.getMessage(), itex.getStartLine());
          abort();
        } catch (Exception ex){
          ex.printStackTrace();
          _program.addError("Unexpected error: " + ex.getMessage(), 0);
          if(XmOption.isDebugOutput()){
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            _program.addError(errors.toString(), 0);
          }
          abort();
        }
      }


      if(!XelementHelper.writeXcodeML(_program, _xcodemlOutputFile,
          INDENT_OUTPUT))
      {
        abort();
      }

    } catch (Exception ex) {
      System.err.println("Transformation exception: " + ex.getMessage());
    }
  }

  /**
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  private void abort(){
    if(_program != null) {
      for (XanalysisError error : _program.getErrors()) {
        if (error.getLine() == 0) {
          System.err.println(ERROR_PREFIX + error.getMessage() + ", line: " +
              "undefined");
        } else {
          System.err.println(ERROR_PREFIX + error.getMessage() + ", line: " +
              error.getLine());
        }
      }
    }
    System.exit(1);
  }

  /**
   * Print all the warnings stored in the XcodeML object and purge them after
   * displaying.
   */
  private void displayWarnings(){
    for(XanalysisError warn : _program.getWarnings()){
      if(warn.getLine() == 0){
        System.err.println(WARNING_PREFIX + warn.getMessage() + ", line: " +
            "undefined");
      } else {
        System.err.println(WARNING_PREFIX + warn.getMessage() + ", line: " +
            warn.getLine());
      }
    }
    _program.purgeWarning();
  }

}

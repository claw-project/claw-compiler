/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

// Cx2x import
import cx2x.translator.common.Constant;
import cx2x.translator.language.ClawDirective;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.transformation.loop.ArrayTransform;
import cx2x.translator.transformation.loop.LoopExtraction;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.translator.transformation.loop.LoopInterchange;
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
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;

  private ClawTransformer _transformer = null;
  private XcodeProgram _program = null;

  private static final int INDENT_OUTPUT = 2; // Number of spaces for indent

  /**
   * ClawXcodeMlTranslator ctor.
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   */
  public ClawXcodeMlTranslator(String xcodemlInputFile,
    String xcodemlOutputFile)
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _transformer = new ClawTransformer();
  }

  /**
   * Analysis the XcodeML code and produce a list of applicable transformation.
   * @throws Exception if analysis fails.
   */
  public void analyze() throws Exception {
    _program = new XcodeProgram(_xcodemlInputFile);
    if(!_program.load()){
      abort();
    }

    // TODO make it generic in the loop
    Map<ClawDirective, ClawLanguage> _blockDirectives = new Hashtable<>();


    for (Xpragma pragma :  XelementHelper.findAllPragmas(_program)){

      if(!ClawLanguage.startsWithClaw(pragma)){
        if(pragma.getValue().toLowerCase().startsWith(Constant.OPENACC_PREFIX))
        {
          OpenAccContinuation t = new OpenAccContinuation(new AnalyzedPragma(pragma));
          addOrAbort(t, _program, _transformer);
        }
        continue; // Not CLAW pragma, we do nothing
      }

      // Analyze the raw pragma with the CLAW language parser
      ClawLanguage analyzedPragma = ClawLanguage.analyze(pragma.getValue());
      analyzedPragma.attachPragma(pragma);
      // TODO code review pass Xpargma directly

      switch (analyzedPragma.getDirective()){
        case LOOP_FUSION:
          addOrAbort(new LoopFusion(analyzedPragma), _program, _transformer);
          break;
        case LOOP_INTERCHANGE:
          addOrAbort(new LoopInterchange(analyzedPragma), _program,
              _transformer);
          break;
        case LOOP_EXTRACT:
          addOrAbort(new LoopExtraction(analyzedPragma), _program,
              _transformer);
          break;
        case ARRAY_TRANSFORM:
          addOrAbort(new ArrayTransform(analyzedPragma), _program,
              _transformer);
          break;
        case REMOVE:
          // TODO code review have a notion of depth to associate pragma of block transformation
          if (_blockDirectives.containsKey(ClawDirective.REMOVE)) {
            addOrAbort(
                new UtilityRemove(
                    _blockDirectives.get(ClawDirective.REMOVE), null
                ), _program, _transformer
            );
          }
          _blockDirectives.remove(ClawDirective.REMOVE);
          _blockDirectives.put(ClawDirective.REMOVE, analyzedPragma);
          break;
        case END_REMOVE:
          if (!_blockDirectives.containsKey(ClawDirective.REMOVE)) {
            _program.addError("Invalid Claw directive (end with no start)",
                pragma.getLineNo());
            abort();
          } else {

            addOrAbort(
                new UtilityRemove(
                    _blockDirectives.get(ClawDirective.REMOVE), analyzedPragma
                ),
                _program, _transformer
            );
            _blockDirectives.remove(ClawDirective.REMOVE);
          }
          break;
        default:
          // TODO add error
          abort();
      }

    }
    if(_blockDirectives.containsKey(ClawDirective.REMOVE)){
      addOrAbort(
          new UtilityRemove(
              _blockDirectives.get(ClawDirective.REMOVE), null
          ), _program, _transformer
      );
    }


    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }

  /**
   * Add a transformation in the pipeline if the analysis is succeded.
   * Otherwise, abort the translation.
   * @param t           The transformation to be analyzed and added.
   * @param xcodeml     The XcodeML object
   * @param translator  The current translator
   */
  private void addOrAbort(Transformation t, XcodeProgram xcodeml,
                          Transformer translator)
  {
    if(t.analyze(xcodeml, translator)){
      translator.addTransformation(t);
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

      // Do the transformation here

      /*if(XmOption.isDebugOutput()){
        for (Map.Entry<Class, TransformationGroup> entry : _transformer.getGroups().entrySet()) {

        //for(TransformationGroup t : _transformer.getGroups()){
          //System.out.println("transform " + t.transformationName () + ": "
          //+ t.count());

          System.out.println("transform " + entry.getValue().transformationName () + ": "
              + entry.getValue().count());
        }
      }*/

      //for(TransformationGroup t : _transformer.getGroups()){
      for (Map.Entry<Class, TransformationGroup> entry : _transformer.getGroups().entrySet()) {

        if(XmOption.isDebugOutput()){
          System.out.println("Apply transfomation: " + entry.getValue().transformationName());
        }

        try {
          entry.getValue().applyTranslations(_program, _transformer);
        } catch (IllegalTransformationException itex) {
          _program.addError("IllegalTransformationException: " +
            itex.getMessage(), itex.getStartLine());
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
      System.err.println("Transformation exception: ");
      ex.printStackTrace();
    }
  }

  /**
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  private void abort(){
    for(XanalysisError error : _program.getErrors()){
      if(error.getLine() == 0){
        System.err.println(ERROR_PREFIX + error.getMessage() + ", line: " +
            "undefined");
      } else {
        System.err.println(ERROR_PREFIX + error.getMessage() + ", line: " +
            error.getLine());
      }
    }
    System.exit(1);
  }

}

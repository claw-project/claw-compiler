/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

// Cx2x import
import cx2x.translator.common.Constant;
import cx2x.translator.transformation.loop.LoopExtraction;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.translator.transformation.loop.LoopInterchange;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.xcodeml.error.*;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.transformation.*;
import cx2x.translator.transformer.*;
import cx2x.translator.pragma.ClawPragma;

// OMNI import
import xcodeml.util.XmOption;


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
  private XcodeProg _program = null;

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
   * @throws Exception
   */
  public void analyze() throws Exception {
    _program = new XcodeProg(_xcodemlInputFile);
    if(!_program.load()){
      abort();
    }

    UtilityRemove _remove = null;

    for (Xpragma pragma :  XelementHelper.findAllPragmas(_program)){
      if(!ClawPragma.startsWithClaw(pragma.getData())){
        if(pragma.getData().toLowerCase().startsWith(Constant.OPENACC_PREFIX))
        {
          OpenAccContinuation t = new OpenAccContinuation(pragma);
          addOrAbort(t, _program, _transformer);
        }
        continue; // Not CLAW pragma, we do nothing
      }

      if(ClawPragma.isValid(pragma.getData())){
        ClawPragma clawDirective = ClawPragma.getDirective(pragma.getData());

        try {
          if (clawDirective == ClawPragma.LOOP_FUSION) {
            LoopFusion trans = new LoopFusion(pragma);
            addOrAbort(trans, _program, _transformer);
          } else if (clawDirective == ClawPragma.LOOP_INTERCHANGE) {
            LoopInterchange trans = new LoopInterchange(pragma);
            addOrAbort(trans, _program, _transformer);
          } else if (clawDirective == ClawPragma.LOOP_EXTRACT) {
            LoopExtraction trans = new LoopExtraction(pragma);
            addOrAbort(trans, _program, _transformer);
          } else if (clawDirective == ClawPragma.UTILITIES_REMOVE) {
            if (_remove != null) {
              addOrAbort(_remove, _program, _transformer);
            }
            _remove = new UtilityRemove(pragma);
          } else if (clawDirective == ClawPragma.BASE_END) {
            if (_remove == null) {
              _program.addError("Invalid Claw directive (end with no start)",
                  pragma.getLine());
              abort();
            } else {
              _remove.setEnd(pragma);
              addOrAbort(_remove, _program, _transformer);
              _remove = null;
            }
          }
        } catch(IllegalDirectiveException ide){
          System.err.println("INVALID DIRECTIVE: " + ide.getDirective());
          System.err.println("cause: " + ide.getMessage());
          if(ide.getDirectiveLine() != 0) {
            System.err.println("line: " + ide.getDirectiveLine());
          }
          System.exit(1);
        }

      } else {
        System.err.println("INVALID PRAGMA: !$" + pragma.getData());
        System.exit(1);
      }
    }
    if(_remove != null){
      addOrAbort(_remove, _program, _transformer);
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
  private void addOrAbort(Transformation t, XcodeProg xcodeml,
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
        // TODO handle return value
        XelementHelper.writeXcodeML(_program, _xcodemlOutputFile, INDENT_OUTPUT);
        return;
      }

      // Do the transformation here

      if(XmOption.isDebugOutput()){
        for(TransformationGroup t : _transformer.getGroups()){
          System.out.println("transform " + t.transformationName () + ": "
          + t.count());
        }
      }

      for(TransformationGroup t : _transformer.getGroups()){

        if(XmOption.isDebugOutput()){
          System.out.println("Apply transfomation: " + t.transformationName());
        }

        try {
          t.applyTranslations(_program, _transformer);
        } catch (IllegalTransformationException itex) {
          _program.addError("IllegalTransformationException: " +
            itex.getMessage(), itex.getStartLine());
          abort();
        }
      }

      // TODO handle the return value
      XelementHelper.writeXcodeML(_program, _xcodemlOutputFile, INDENT_OUTPUT);

    } catch (Exception ex) {
      // TODO handle exception
      System.out.println("Transformation exception: ");
      ex.printStackTrace();
    }
  }

  /**
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  private void abort(){
    for(XanalysisError error : _program.getErrors()){
      System.err.println(ERROR_PREFIX + error.getMessage() + ", " +
          error.getLine());
    }
    System.exit(1);
  }

}

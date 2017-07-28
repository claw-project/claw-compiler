/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator;

// Cx2x import

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.config.Configuration;
import cx2x.translator.config.GroupConfiguration;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformer.ClawTransformer;
import cx2x.xcodeml.error.XanalysisError;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.TransformationGroup;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import xcodeml.util.XmOption;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
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
   */
  public ClawXcodeMlTranslator(String xcodemlInputFile,
                               String xcodemlOutputFile,
                               Configuration config)
      throws Exception
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;

    // Create transformer
    String transformerClassPath =
        config.getParameter(Configuration.TRANSFORMER);
    if(transformerClassPath == null || transformerClassPath.equals("")) {
      throw new Exception("Transformer not set in configuration");
    }

    try {
      // Check if class is there
      Class<?> transformerClass = Class.forName(transformerClassPath);
      Constructor<?> ctor =
          transformerClass.getConstructor(Configuration.class);
      _transformer = (ClawTransformer) ctor.newInstance(config);
    } catch(ClassNotFoundException e) {
      throw new Exception("Cannot create transformer");
    }
  }

  /**
   * Analysis the XcodeML code and produce a list of applicable transformation.
   */
  public void analyze() {
    _program = XcodeProgram.createFromFile(_xcodemlInputFile);
    if(_program == null) {
      abort();
    }

    // Check all pragma found in the translation unit
    for(Xnode pragma : _program.getAllStmt(Xcode.FPRAGMASTATEMENT)) {

      // Pragma can be handled by the transformer so let it do its job.
      if(_transformer.isHandledPragma(pragma)) {
        try {
          _transformer.generateTransformation(_program, pragma);
        } catch(IllegalDirectiveException e) {
          _program.addError(e.getMessage(), e.getDirectiveLine());
          abort();
        }
      } else {
        // Check if the pragma is a compile guard
        if(_transformer.getConfiguration().getAcceleratorGenerator().
            isCompileGuard(pragma.value()))
        {
          pragma.delete();
        } else {
          // Handle special transformation of OpenACC line continuation
          for(GroupConfiguration gc :
              _transformer.getConfiguration().getGroups())
          {
            if(gc.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE
                && XnodeUtil.getPragmaPrefix(pragma).equals(gc.getDirective()))
            {
              generateTransformation(gc, new ClawLanguage(pragma));
            }
          }
        }
      }
    }

    _transformer.finalize(_program);

    // Generate transformation for translation_unit trigger type
    for(GroupConfiguration gc :
        _transformer.getConfiguration().getGroups()) {
      if(gc.getTriggerType() ==
          GroupConfiguration.TriggerType.TRANSLATION_UNIT)
      {
        generateTransformation(gc, null);
      }
    }

    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }

  /**
   * Instantiate correct transformation class from group configuration.
   *
   * @param gc     Group configuration for the
   * @param pragma Pragma associated with the transformation.
   */
  private void generateTransformation(GroupConfiguration gc,
                                      ClawLanguage pragma)
  {
    try {
      Class<?> groupClass = gc.getTransformationClass();
      ClawTransformation transformation;
      if(pragma != null) {
        Constructor<?> ctor = groupClass.getConstructor(ClawLanguage.class);
        transformation = (ClawTransformation) ctor.newInstance(pragma);
      } else {
        Constructor<?> ctor = groupClass.getConstructor();
        transformation = (ClawTransformation) ctor.newInstance();
      }
      _transformer.addTransformation(_program, transformation);
    } catch(Exception ex) {
      System.err.println("Cannot generate transformation " + gc.getName());
      System.err.println(ex.getMessage());
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
    if(_program != null) {
      displayMessages(WARNING_PREFIX, _program.getWarnings());
    }
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

  /**
   * Get the current transformer associated with this translation.
   *
   * @return Get the current transformer.
   */
  public ClawTransformer getTransformer() {
    return _transformer;
  }

  /**
   * Get the XcodeProgram object representing the Fortran code translated.
   *
   * @return Current XcodeProgram object.
   */
  public XcodeProgram getProgram() {
    return _program;
  }

}

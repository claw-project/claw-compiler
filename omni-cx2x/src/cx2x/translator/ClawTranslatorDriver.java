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
import cx2x.xcodeml.error.XanalysisError;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
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
 * ClawTranslatorDriver is the class driving the translation. It creates the
 * translator and pass to it all the directives it can manage.
 * It is also in charge of decompiling the XcodeML/F after the translation took
 * place.
 *
 * @author clementval
 */

public class ClawTranslatorDriver {

  private static final String ERROR_PREFIX = "claw-error: ";
  private static final String WARNING_PREFIX = "claw warning: ";
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;
  private ClawTranslator _translator = null;
  private XcodeProgram _translationUnit = null;

  /**
   * ClawTranslatorDriver ctor.
   *
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   * @param config            Configuration information object.
   */
  public ClawTranslatorDriver(String xcodemlInputFile,
                              String xcodemlOutputFile,
                              Configuration config)
      throws Exception
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;

    // Create translator
    String translatorClassPath =
        config.getParameter(Configuration.TRANSLATOR);
    if(translatorClassPath == null || translatorClassPath.equals("")) {
      throw new Exception("Translator not set in configuration");
    }

    try {
      // Check if class is there
      Class<?> translatorClass = Class.forName(translatorClassPath);
      Constructor<?> ctor =
          translatorClass.getConstructor(Configuration.class);
      _translator = (ClawTranslator) ctor.newInstance(config);
    } catch(ClassNotFoundException e) {
      throw new Exception("Cannot create translator");
    }
  }

  /**
   * Analysis the XcodeML/F directives and categorized them in corresponding
   * transformation with the help of the translator.
   */
  public void analyze() {
    _translationUnit = XcodeProgram.createFromFile(_xcodemlInputFile);
    if(_translationUnit == null) {
      abort();
    }
    try {
      // Check all pragma found in the translation unit
      for(Xnode pragma : _translationUnit.getAllStmt(Xcode.FPRAGMASTATEMENT)) {
        // Pragma can be handled by the translator so let it do its job.
        if(_translator.isHandledPragma(pragma)) {
          _translator.generateTransformation(_translationUnit, pragma);
        } else {
          // Check if the pragma is a compile guard
          if(_translator.getConfiguration().getAcceleratorGenerator().
              isCompileGuard(pragma.value()))
          {
            pragma.delete();
          } else {
            // Handle special transformation of OpenACC line continuation
            for(GroupConfiguration gc :
                _translator.getConfiguration().getGroups()) {
              if(gc.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE
                  && XnodeUtil.getPragmaPrefix(pragma).equals(gc.getDirective()))
              {
                generateTransformation(gc, new ClawLanguage(pragma));
              }
            }
          }
        }
      }

      _translator.finalize(_translationUnit);

    } catch(IllegalDirectiveException e) {
      _translationUnit.addError(e.getMessage(), e.getDirectiveLine());
      abort();
    } catch(IllegalTransformationException e) {
      _translationUnit.addError(e.getMessage(), e.getStartLine());
      abort();
    }

    // Generate transformation for translation_unit trigger type
    for(GroupConfiguration gc :
        _translator.getConfiguration().getGroups()) {
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
      _translator.addTransformation(_translationUnit, transformation);
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
        _translationUnit.write(_xcodemlOutputFile, ClawConstant.INDENT_OUTPUT);
        return;
      }

      for(Map.Entry<Class, TransformationGroup> entry :
          _translator.getGroups().entrySet()) {
        if(XmOption.isDebugOutput()) {
          System.out.println("Apply transformation: " +
              entry.getValue().transformationName() + " - " +
              entry.getValue().count()
          );
        }

        try {
          entry.getValue().applyTranslations(_translationUnit, _translator);
          displayWarnings();
        } catch(IllegalTransformationException itex) {
          _translationUnit.addError(itex.getMessage(), itex.getStartLine());
          abort();
        } catch(Exception ex) {
          ex.printStackTrace();
          _translationUnit.addError("Unexpected error: " + ex.getMessage(), 0);
          if(XmOption.isDebugOutput()) {
            StringWriter errors = new StringWriter();
            ex.printStackTrace(new PrintWriter(errors));
            _translationUnit.addError(errors.toString(), 0);
          }
          abort();
        }
      }

      // Write transformed IR to file
      _translationUnit.write(_xcodemlOutputFile, ClawConstant.INDENT_OUTPUT);
    } catch(Exception ex) {
      System.err.println("Transformation exception: " + ex.getMessage());
    }
  }


  /**
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  private void abort() {
    if(_translationUnit != null) {
      displayMessages(ERROR_PREFIX, _translationUnit.getErrors());
    }
    System.exit(1);
  }

  /**
   * Print all the warnings stored in the XcodeML object and purge them after
   * displaying.
   */
  private void displayWarnings() {
    if(_translationUnit != null) {
      displayMessages(WARNING_PREFIX, _translationUnit.getWarnings());
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
    _translator.getModCache().write(modPrefix, ClawConstant.INDENT_OUTPUT);
  }

  /**
   * Get the current translator associated with this translation.
   *
   * @return Get the current translator.
   */
  public ClawTranslator getTranslator() {
    return _translator;
  }

  /**
   * Get the XcodeProgram object representing the Fortran code translated.
   *
   * @return Current XcodeProgram object.
   */
  public XcodeProgram getProgram() {
    return _translationUnit;
  }

}

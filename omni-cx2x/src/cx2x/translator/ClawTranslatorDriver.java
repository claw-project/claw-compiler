/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Message;
import cx2x.translator.config.Configuration;
import cx2x.translator.config.GroupConfiguration;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformation.primitive.Module;
import cx2x.translator.transformation.primitive.Pragma;
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
   */
  public ClawTranslatorDriver(String xcodemlInputFile,
                              String xcodemlOutputFile)
      throws Exception
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;

    // Create translator
    String translatorClassPath =
        Configuration.get().getParameter(Configuration.TRANSLATOR);
    if(translatorClassPath == null || translatorClassPath.equals("")) {
      throw new Exception("Translator not set in configuration");
    }

    try {
      // Check if class is there
      Class<?> translatorClass = Class.forName(translatorClassPath);
      Constructor<?> ctor = translatorClass.getConstructor();
      _translator = (ClawTranslator) ctor.newInstance();
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
          if(Configuration.get().getAcceleratorGenerator().
              isCompileGuard(pragma.value()))
          {
            pragma.delete();
          } else {
            // Handle special transformation of OpenACC line continuation
            for(GroupConfiguration gc : Configuration.get().getGroups()) {
              if(gc.getTriggerType() == GroupConfiguration.TriggerType.DIRECTIVE
                  && Pragma.getPrefix(pragma).equals(gc.getDirective()))
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
    for(GroupConfiguration gc : Configuration.get().getGroups()) {
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
        Message.debug("Apply transformation: " +
            entry.getValue().transformationName() + " - " +
            entry.getValue().count()
        );

        try {
          entry.getValue().applyTranslations(_translationUnit, _translator);
          Message.warnings(_translationUnit);
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
    Message.errors(_translationUnit);
    System.exit(1);
  }

  /**
   * Flush all information stored in the translator.
   */
  public void flush()
      throws IllegalTransformationException
  {
    _translator.getModCache().write(ClawConstant.INDENT_OUTPUT);
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
  public XcodeProgram getTranslationUnit() {
    return _translationUnit;
  }
}

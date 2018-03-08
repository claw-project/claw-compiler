/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import claw.shenron.transformation.TransformationGroup;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.ClawConstant;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.GroupConfiguration;
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
public abstract class ClawTranslatorDriverBase {

  protected String _xcodemlInputFile = null;
  protected String _xcodemlOutputFile = null;
  protected boolean _canTransform = false;
  protected ClawTranslator _translator = null;
  protected XcodeProgram _translationUnit = null;


  /**
   * Analysis the XcodeML/F directives and categorized them in corresponding
   * transformation with the help of the translator.
   */
  public abstract void analyze();

  /**
   * Instantiate correct transformation class from group configuration.
   *
   * @param gc     Group configuration for the
   * @param pragma Pragma associated with the transformation.
   */
  private void generateTransformation(GroupConfiguration gc,
                                      ClawPragma pragma)
  {
    try {
      Class<?> groupClass = gc.getTransformationClass();
      ClawTransformation transformation;
      if(pragma != null) {
        Constructor<?> ctor = groupClass.getConstructor(ClawPragma.class);
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
  public abstract void transform();

  /**
   * Print all the errors stored in the XcodeML object and abort the program.
   */
  protected void abort() {
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

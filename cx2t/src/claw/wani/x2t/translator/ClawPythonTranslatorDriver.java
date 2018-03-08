/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import claw.shenron.transformation.TransformationGroup;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.ClawConstant;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.translator.ClawTranslatorDriverBase;
import claw.wani.x2t.configuration.Configuration;
//import xcodeml.util.XmOption;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Constructor;
import java.util.Map;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * ClawPythonTranslatorDriver is the class that wraps the use of a Python
 * script in order to perform the translation.
 *
 * @author A. R. Porter
 */
public class ClawPythonTranslatorDriver extends ClawTranslatorDriverBase {

  private String _transformScript = null;

  /**
   * ClawPythonTranslatorDriver constructor.
   *
   * @param transScript The python recipe input file path.
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   */
  public ClawPythonTranslatorDriver(String transScript,
		                    String xcodemlInputFile,
			            String xcodemlOutputFile)
      throws Exception
  {
    _transformScript = transScript;
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    
    System.out.println("Creating factory object");

  }

  /**
   * Analyse the XcodeML/F directives and categorize them in corresponding
   * transformation with the help of the translator.
   */
  public void analyze() {
    _translationUnit = XcodeProgram.createFromFile(_xcodemlInputFile);
    if(_translationUnit == null) {
      abort();
    }
    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }

  /**
   * Apply the transformation script
   */
  public void transform() {
  }
}

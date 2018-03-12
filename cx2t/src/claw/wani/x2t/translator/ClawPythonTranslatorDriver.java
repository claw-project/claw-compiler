/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.wani.ClawConstant;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.translator.ClawTranslatorDriverBase;
import claw.wani.x2t.configuration.Configuration;

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
   * Apply the transformation script
   */
  public void transform() {
  }
}

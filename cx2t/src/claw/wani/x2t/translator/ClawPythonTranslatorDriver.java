/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

/**
 * ClawPythonTranslatorDriver is the class that wraps the use of a Python
 * script in order to perform the translation.
 *
 * @author A. R. Porter
 */
public class ClawPythonTranslatorDriver extends ClawTranslatorDriver {

  private String _transformScript = null;

  /**
   * ClawPythonTranslatorDriver constructor.
   *
   * @param transScript       The python recipe input file path.
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   */
  public ClawPythonTranslatorDriver(String transScript, String xcodemlInputFile,
                                    String xcodemlOutputFile)
  {
    _transformScript = transScript;
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;

    System.out.println("Creating factory object");
  }

  /**
   * Analysis the XcodeML/F directives and categorized them in corresponding
   * transformation with the help of the translator.
   */
  public void analyze()
  {
  }

  /**
   * Apply the transformation script
   */
  public void transform()
  {
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.xcodeml.backend.OmniBackendDriver;
import claw.wani.report.ClawTransformationReport;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.translator.ClawPythonTranslatorDriver;
import claw.wani.x2t.translator.ClawTranslatorDriver;
import org.apache.commons.cli.*;
import xcodeml.util.XmOption;

import java.io.File;

/**
 * ClawX2T is the entry point of any CLAW XcodeML/F translation.
 *
 * @author clementval
 */
public class ClawX2T {

  /**
   * Print an error message an abort.
   *
   * @param filename   Filename in which error occurred.
   * @param lineNumber Line number of the error, if known.
   * @param charPos    Character position of the error, if known.
   * @param msg        Error message.
   */
  private static void error(String filename, int lineNumber,
                            int charPos, String msg)
  {
    System.err.println(String.format("%s:%d:%d error: %s",
        filename, lineNumber, charPos, msg));
    System.exit(1);
  }

  /**
   * Print program usage.
   */
  private static void usage() {
    Options options = prepareOptions();
    HelpFormatter formatter = new HelpFormatter();
    formatter.printHelp("clawfc", options);
    System.exit(1);
  }

  /**
   * List all directive target available for code generation.
   */
  private static void listTarget() {
    System.out.println("- CLAW available targets -");
    for(String t : Target.availableTargets()) {
      System.out.println("  - " + t);
    }
  }

  /**
   * List all directive directive language available for code generation.
   */
  private static void listDirectiveLanguage() {
    System.out.println("- CLAW directive directive language -");
    for(String d : CompilerDirective.availableDirectiveLanguage()) {
      System.out.println("  - " + d);
    }
  }

  /**
   * Prepare the set of available options.
   *
   * @return Options object.
   */
  private static Options prepareOptions() {
    Options options = new Options();
    options.addOption("h", "help", false,
        "display program usage.");
    options.addOption("l", false,
        "suppress line directive in decompiled code.");
    options.addOption("cp", "config-path", true,
        "specify the configuration directory");
    options.addOption("c", "config", true,
        "specify the configuration for the translator.");
    options.addOption("s", "schema", true,
        "specify the XSD schema location to validate the configuration.");
    options.addOption("t", "target", true,
        "specify the target for the code transformation.");
    options.addOption("dir", "directive", true,
        "list all directive directive language available for code generation.");
    options.addOption("d", "debug", false,
        "enable output debug message.");
    options.addOption("f", true,
        "specify FORTRAN decompiled output file.");
    options.addOption("w", true,
        "number of character per line in decompiled code.");
    options.addOption("o", true,
        "specify XcodeML/F output file.");
    options.addOption("M", true,
        "specify where to search for .xmod files");
    options.addOption("tl", "target-list", false,
        "list all target available for code transformation.");
    options.addOption("dl", "directive-list", false,
        "list all available directive language to be generated.");
    options.addOption("sc", "show-config", false,
        "display the current configuration.");
    options.addOption("fp", "force-pure", false,
        "exit the translator if a PURE subroutine/function " +
            "has to be transformed.");
    options.addOption("r", "report", true,
        "generate the transformation report.");
    options.addOption("script", "python-script", true,
        "Python optimisation script to apply (requires Jython)");
    return options;
  }

  /**
   * Parse the arguments passed to the program.
   *
   * @param args Arguments passed to the program.
   * @return Parsed command line object.
   * @throws ParseException If one or several arguments are not found.
   */
  private static CommandLine processCommandArgs(String[] args)
      throws ParseException
  {
    Options options = prepareOptions();
    CommandLineParser parser = new DefaultParser();
    return parser.parse(options, args);
  }

  /**
   * Main point of entry of the program.
   *
   * @param args Arguments of the program.
   * @throws Exception if translation failed.
   */
  public static void main(String[] args) throws Exception {
    String input;
    String xcmlOutput = null;
    String targetLangOutput = null;
    String target_option = null;
    String directive_option = null;
    String configuration_file = null;
    String configuration_path = null;
    String recipeScript = null;
    int maxColumns = 0;
    //boolean forcePure = false;

    CommandLine cmd;
    try {
      cmd = processCommandArgs(args);
    } catch(ParseException pex) {
      error("internal", 0, 0, pex.getMessage());
      return;
    }

    // Help option
    if(cmd.hasOption("h")) {
      usage();
      return;
    }

    // Display target list option
    if(cmd.hasOption("tl")) {
      listTarget();
      return;
    }

    // Display directive list option
    if(cmd.hasOption("dl")) {
      listDirectiveLanguage();
      return;
    }

    // Target option
    if(cmd.hasOption("t")) {
      target_option = cmd.getOptionValue("t");
    }

    // Directive option
    if(cmd.hasOption("dir")) {
      directive_option = cmd.getOptionValue("dir");
    }

    // Suppressing line directive option
    if(cmd.hasOption("l")) {
      XmOption.setIsSuppressLineDirective(true);
    }

    // Debug option
    if(cmd.hasOption("d")) {
      XmOption.setDebugOutput(true);
    }

    // XcodeML/F output file option
    if(cmd.hasOption("o")) {
      xcmlOutput = cmd.getOptionValue("o");
    }

    // FORTRAN output file option
    if(cmd.hasOption("f")) {
      targetLangOutput = cmd.getOptionValue("f");
    }

    if(cmd.hasOption("w")) {
      maxColumns = Integer.parseInt(cmd.getOptionValue("w"));
    }

    if(cmd.hasOption("c")) {
      configuration_file = cmd.getOptionValue("c");
    }

    if(cmd.hasOption("cp")) {
      configuration_path = cmd.getOptionValue("cp");
    }

    // Check that configuration path exists
    if(configuration_path == null) {
      error("internal", 0, 0, "Configuration path missing.");
      return;
    }

    // Check that configuration file exists
    if(configuration_file != null) {
      File configFile = new File(configuration_file);
      if(!configFile.exists()) {
        error("internal", 0, 0, "Configuration file not found. "
            + configuration_path);
      }
    }

    // --show-configuration option
    if(cmd.hasOption("sc")) {
      Configuration.get().load(configuration_path, configuration_file);
      Configuration.get().displayConfig();
      return;
    }

    if(cmd.hasOption("script")) {
      recipeScript = cmd.getOptionValue("script");
    }

    // Get the input XcodeML file to transform
    if(cmd.getArgs().length == 0) {
      input = null;
    } else {
      input = cmd.getArgs()[0];
    }

    // Read the configuration file
    try {
      Configuration.get().load(configuration_path, configuration_file);
      Configuration.get().setUserDefinedTarget(target_option);
      Configuration.get().setUserDefineDirective(directive_option);
      Configuration.get().setMaxColumns(maxColumns);
      Context.init(Configuration.get().getCurrentDirective(),
          Configuration.get().getCurrentTarget(), maxColumns);

      if(Context.get().getCompilerDirective() == CompilerDirective.OPENACC) {
        OpenAcc openaccGen = (OpenAcc) Context.get().getGenerator();
        openaccGen.setExecutionMode(Configuration.get().openACC().getMode());
      }
    } catch(Exception ex) {
      error("internal", 0, 0, ex.getMessage());
      return;
    }

    // Module search path options
    if(cmd.hasOption("M")) {
      for(String value : cmd.getOptionValues("M")) {
        Context.get().getModuleCache().addSearchPath(value);
      }
    }

    // Force pure option
    if(cmd.hasOption("fp")) {
      Configuration.get().setForcePure();
    }

    ClawTranslatorDriver translatorDriver;

    // Call the translator driver to apply transformation on XcodeML/F
    if(recipeScript != null) {
      // Transformation is to be performed by Python script
      translatorDriver =
          new ClawPythonTranslatorDriver(recipeScript, input, xcmlOutput);
    } else {
      translatorDriver = new ClawTranslatorDriver(input, xcmlOutput);
    }

    translatorDriver.analyze();
    translatorDriver.transform();
    translatorDriver.flush();

    // Produce report (unless we've used the Python driver)
    if(recipeScript == null && cmd.hasOption("r")) {
      ClawTransformationReport report =
          new ClawTransformationReport(cmd.getOptionValue("r"));
      report.generate(args, translatorDriver);
    }

    // Decompile XcodeML/F to target language
    OmniBackendDriver backend;
    if(Configuration.get().getCurrentTarget() == Target.FPGA) {
      // TODO remove when supported
      error(xcmlOutput, 0, 0, "FPGA target is not supported yet");
      backend = new OmniBackendDriver(OmniBackendDriver.Lang.C);
    } else {
      backend = new OmniBackendDriver(OmniBackendDriver.Lang.FORTRAN);
    }

    if(xcmlOutput == null) { // XcodeML output not written to file. Use pipe.
      if(!backend.decompile(targetLangOutput,
          translatorDriver.getTranslationUnit(), maxColumns,
          XmOption.isSuppressLineDirective()))
      {
        error(targetLangOutput, 0, 0, "Unable to decompile XcodeML to Fortran");
        System.exit(1);
      }
    } else {
      if(!backend.decompileFromFile(targetLangOutput, xcmlOutput, maxColumns,
          XmOption.isSuppressLineDirective()))
      {
        error(xcmlOutput, 0, 0, "Unable to decompile XcodeML to Fortran");
        System.exit(1);
      }
    }
  }
}

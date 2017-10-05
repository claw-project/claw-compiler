/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x;

import cx2x.decompiler.XcmlBackend;
import cx2x.translator.ClawTranslatorDriver;
import cx2x.translator.config.Configuration;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.report.ClawTransformationReport;
import exc.xcodeml.XcodeMLtools_Fmod;
import org.apache.commons.cli.*;
import xcodeml.util.XmOption;

import java.io.File;

/**
 * Cx2x is the entry point of any CLAW XcodeML/F translation.
 *
 * @author clementval
 */
public class Cx2x {

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
   * List all accelerator target available for code generation.
   */
  private static void listTarget() {
    System.out.println("- CLAW available targets -");
    for(String t : Target.availableTargets()) {
      System.out.println("  - " + t);
    }
  }

  /**
   * List all accelerator directive language available for code generation.
   */
  private static void listDirectiveLanguage() {
    System.out.println("- CLAW accelerator directive language -");
    for(String d : AcceleratorDirective.availableDirectiveLanguage()) {
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
    options.addOption("h", "help", false, "display program usage.");
    options.addOption("l", false, "suppress line directive in decompiled code.");
    options.addOption("cp", "config-path", true, "specify the configuration directory");
    options.addOption("c", "config", true, "specify the configuration for the translator.");
    options.addOption("s", "schema", true, "specify the XSD schema location to validate the configuration.");
    options.addOption("t", "target", true, "specify the target for the code transformation.");
    options.addOption("dir", "directive", true, "list all accelerator directive language available for code generation.");
    options.addOption("d", "debug", false, "enable output debug message.");
    options.addOption("f", true, "specify FORTRAN decompiled output file.");
    options.addOption("w", true, "number of character per line in decompiled code.");
    options.addOption("o", true, "specify XcodeML/F output file.");
    options.addOption("M", true, "specify where to search for .xmod files");
    options.addOption("tl", "target-list", false, "list all target available for code transformation.");
    options.addOption("dl", "directive-list", false, "list all available directive language to be generated.");
    options.addOption("sc", "show-config", false, "display the current configuration.");
    options.addOption("fp", "force-pure", false, "exit the translator if a PURE subroutine/function has to be transformed.");
    options.addOption("r", "report", true, "generate the transformation report.");
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
    String xcmlOuput = null;
    String targetLangOutput = null;
    String target_option = null;
    String directive_option = null;
    String configuration_file = null;
    String configuration_path = null;
    int maxColumns = 0;
    boolean forcePure = false;

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
      xcmlOuput = cmd.getOptionValue("o");
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

    // --show-config option
    if(cmd.hasOption("sc")) {
      Configuration config =
          new Configuration(configuration_path, configuration_file);
      config.displayConfig();
      return;
    }

    if(cmd.getArgs().length == 0) {
      error("internal", 0, 0, "no input file");
      return;
    } else {
      input = cmd.getArgs()[0];
    }

    // Module search path options
    if(cmd.hasOption("M")) {
      for(String value : cmd.getOptionValues("M")) {
        XcodeMLtools_Fmod.addSearchPath(value);
      }
    }

    // Read the configuration file
    Configuration config;
    try {
      config = new Configuration(configuration_path, configuration_file);
      config.setUserDefinedTarget(target_option);
      config.setUserDefineDirective(directive_option);
      config.setMaxColumns(maxColumns);
    } catch(Exception ex) {
      error("internal", 0, 0, ex.getMessage());
      return;
    }

    // Force pure option
    if(cmd.hasOption("fp")) {
      config.setForcePure();
    }

    // Call the translator driver to apply transformation on XcodeML/F
    ClawTranslatorDriver translatorDriver =
        new ClawTranslatorDriver(input, xcmlOuput, config);
    translatorDriver.analyze();
    translatorDriver.transform();
    translatorDriver.flush(config);

    // Produce report
    if(cmd.hasOption("r")) {
      ClawTransformationReport report =
          new ClawTransformationReport(cmd.getOptionValue("r"));
      report.generate(config, args, translatorDriver);
    }

    // Decompile XcodeML/F to target language
    XcmlBackend decompiler;
    if(config.getCurrentTarget() == Target.FPGA) {
      // TODO remove when supported
      error(xcmlOuput, 0, 0, "FPGA target is not supported yet");
      decompiler = new XcmlBackend(XcmlBackend.Lang.C);
    } else {
      decompiler = new XcmlBackend(XcmlBackend.Lang.FORTRAN);
    }
    if(!decompiler.decompile(targetLangOutput, xcmlOuput, maxColumns,
        XmOption.isSuppressLineDirective()))
    {
      error(xcmlOuput, 0, 0, "Unable to decompile XcodeML to target language");
      System.exit(1);
    }
  }
}

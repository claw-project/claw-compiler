/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x;

import cx2x.decompiler.FortranDecompiler;
import cx2x.translator.ClawXcodeMlTranslator;
import cx2x.translator.config.Configuration;
import cx2x.translator.config.GroupConfiguration;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.target.Target;
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
   * @param s Error message.
   */
  private static void error(String s) {
    System.err.println(s);
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
   * Read the configuration file and output the information for user.
   *
   * @param configPath Path to the configuration file.
   * @param schemaPath Path to the XSD schema for configuration file validation.
   */
  private static void showConfig(String configPath, String schemaPath) {
    try {
      Configuration config = new Configuration(configPath, schemaPath);
      System.out.println("- CLAW translator configuration -\n");
      System.out.println("Default accelerator directive: " +
          config.getCurrentDirective() + "\n");
      System.out.println("Default target: " + config.getCurrentTarget() + "\n");
      System.out.println("Current transformation order:");
      int i = 0;
      for(GroupConfiguration g : config.getGroups()) {
        System.out.printf("  %1d) %-20s - type:%-15s, class:%-60s\n",
            i++, g.getName(), g.getType(), g.getTransformationClassName());
      }
    } catch(Exception e) {
      error("Could not read the configuration file.\n" + e.getMessage());
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
    options.addOption("c", "config", true, "specify an alternative configuration for the translator.");
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
    String xcodeMlOutput = null;
    String fortranOutput = null;
    String target_option = null;
    String directive_option = null;
    String configuration_path = null;
    String schema_path = null;
    int maxColumns = 0;
    boolean forcePure = false;

    CommandLine cmd;
    try {
      cmd = processCommandArgs(args);
    } catch(ParseException pex) {
      error(pex.getMessage());
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
      xcodeMlOutput = cmd.getOptionValue("o");
    }

    // FORTRAN output file option
    if(cmd.hasOption("f")) {
      fortranOutput = cmd.getOptionValue("f");
    }

    if(cmd.hasOption("w")) {
      maxColumns = Integer.parseInt(cmd.getOptionValue("w"));
    }

    if(cmd.hasOption("c")) {
      configuration_path = cmd.getOptionValue("c");
    }

    if(cmd.hasOption("s")) {
      schema_path = cmd.getOptionValue("s");
    }

    // Check that configuration file exists
    if(configuration_path == null) {
      error("Configuration file missing.");
      return;
    }
    File configFile = new File(configuration_path);
    if(!configFile.exists()) {
      error("Configuration file not found. " + configuration_path);
    }

    if(cmd.hasOption("sc")) {
      showConfig(configuration_path, schema_path);
      return;
    }

    if(cmd.getArgs().length == 0) {
      error("no input file");
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
    Configuration config = new Configuration(configuration_path, schema_path);
    config.setUserDefinedTarget(target_option);
    config.setUserDefineDirective(directive_option);

    // Force pure option
    if(cmd.hasOption("fp")) {
      config.setForcePure();
    }

    // Call the translator to apply transformation on XcodeML/F
    ClawXcodeMlTranslator translator = new ClawXcodeMlTranslator(input,
        xcodeMlOutput, config, maxColumns);
    translator.analyze();
    translator.transform();
    translator.flush(config);

    // Decompile XcodeML/F to Fortran
    FortranDecompiler decompiler = new FortranDecompiler();
    if(!decompiler.decompile(fortranOutput, xcodeMlOutput, maxColumns,
        XmOption.isSuppressLineDirective()))
    {
      error("Unable to decompile XcodeML to Fortran");
    }
  }
}

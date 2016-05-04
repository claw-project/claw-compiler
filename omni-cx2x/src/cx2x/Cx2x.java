/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x;

import cx2x.translator.ClawXcodeMlTranslator;
import cx2x.translator.common.ConfigurationHelper;
import cx2x.translator.common.GroupConfiguration;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import exc.xcodeml.XcodeMLtools_Fmod;
import xcodeml.util.XmOption;

import java.io.File;
import java.util.List;

/**
 * Cx2x is the entry point of any CLAW XcodeML/F translation.
 *
 * @author clementval
 */

public class Cx2x {
  /**
   * Print an error message an abort.
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
    final String[] lines = {
      "arguments: options",
      "           <input XcodeML file>",
      "           [-o <output reconstructed XcodeML file>]",
      "",
      "  -h, --help       display program usage.",
      "  -l               suppress line directive in decompiled code.",
      //"  -w N             set max columns to N for Fortran source.",
      //"  -gnu             decompile for GNU Fortran (default).",
      //"  -intel           decompile for Intel Fortran.",
      "  -M dir           specify where to search for .xmod files",
      //"  -decomp          output decompiled source code.",
      "  -target-list     list all accelerator target available for code generation.",
      "  -target=<target> specify the accelerator language target for code generation.",
      "  -config=<target> specify an alternative configuration for the translator.",
      "",
      " Debug Options:",
      "  -d           enable output debug message.",
      //"  -dxcode      output Xcode file as <input file>.x",
      //"  -dump        output Xcode file and decompiled file to standard output.",
    };

    for(String line : lines) {
      System.err.println(line);
    }
    System.exit(1);
  }

  /**
   * List all accelerator target available for code generation.
   */
  private static void listTarget(){
    System.out.println("Not implemented yet.");
  }

  /**
   * Read the configuration file and output the information for user.
   * @param configPath Path to the configuration file
   */
  private static void showConfig(String configPath){
    try{
      List<GroupConfiguration> groups =
          ConfigurationHelper.readGroups(configPath);
      AcceleratorDirective target =
          ConfigurationHelper.readDefaultTarget(configPath);
      System.out.println("- CLAW translator configuration -\n");
      System.out.println("Default accelerator target: " + target + "\n");
      System.out.println("Current transformation order:");
      for (int i = 0; i < groups.size(); ++i) {
        GroupConfiguration g = groups.get(i);
        System.out.printf("  %1d) %-20s - type:%-15s, class:%-60s\n",
            i, g.getName(), g.getType(), g.getTransformationClass());
      }
    } catch (Exception e) {
      error("Could not read the configuration file");
    }
  }

  /**
   * Main point of entry of the program.
   * @param args  Arguments of the program.
   * @throws Exception if translation failed.
   */
  public static void main(String[] args) throws Exception {
    String inXmlFile = null;
    String outXmlFile = null;
    //String lang = "F"; // default language is Fortran
    String target_option = null;
    String configuration_path = null;
    boolean show_configuration = false;

    for(int i = 0; i < args.length; ++i) {
      String arg = args[i];
      String narg = (i < args.length - 1) ? args[i + 1] : null;

      if(arg.equals("-h") || arg.equals("--help")) {
        usage();
      } else if(arg.equals("-l")) {
        XmOption.setIsSuppressLineDirective(true);
      } else if(arg.equals("-d")) {
        XmOption.setDebugOutput(true);
      } else if(arg.equals("-o")) {
        if(narg == null)
          error("needs argument after -o");
        outXmlFile = narg;
        ++i;
      /*} else if(arg.equals("-gnu")) {
        XmOption.setCompilerVendor(XmOption.COMP_VENDOR_GNU);
      } else if(arg.equals("-intel")) {
        XmOption.setCompilerVendor(XmOption.COMP_VENDOR_INTEL);*/
      } else if (arg.startsWith("-M")) {
          if (arg.equals("-M")) {
            if (narg == null)
              error("needs argument after -M");
            XcodeMLtools_Fmod.addSearchPath(narg);
            ++i;
          }
          else {
            XcodeMLtools_Fmod.addSearchPath(arg.substring(2));
          }
      } else if (arg.startsWith("--target-list")) {
        listTarget();
        return;
      } else if (arg.startsWith("--target=")) {
        target_option = arg.replace("--target=", "");
      } else if (arg.startsWith("--config=")) {
        configuration_path = arg.replace("--config=", "");
      } else if (arg.startsWith("--show-config")) {
        show_configuration = true;
      } else if(arg.startsWith("-")){
        error("unknown option " + arg);
      } else if(inXmlFile == null) {
        inXmlFile = arg;
      } else {
        error("too many arguments");
      }
    }

    // Check that configuration file exists
    if(configuration_path == null){
      error("Configuration file missing.");
      return;
    }
    File config = new File(configuration_path);
    if(!config.exists()){
      error("Configuration file not found. " + configuration_path);
      return;
    }

    if(show_configuration){
      showConfig(configuration_path);
      return;
    }

    // Read default target from config if not set by user
    AcceleratorDirective target;
    if(target_option == null){
      target = ConfigurationHelper.readDefaultTarget(configuration_path);
    } else {
      target = AcceleratorDirective.fromString(target_option);
    }

    // Read transformation group configuration
    List<GroupConfiguration> groups;
    try {
      groups = ConfigurationHelper.readGroups(configuration_path);
    } catch (Exception ex){
      error(ex.getMessage());
      return;
    }

    ClawXcodeMlTranslator translator =
        new ClawXcodeMlTranslator(inXmlFile, outXmlFile, target, groups);
    translator.analyze();
    translator.transform();
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x;

import cx2x.translator.ClawXcodeMlTranslator;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import exc.xcodeml.XcodeMLtools_Fmod;
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
      /*"arguments: [-xc|-xf] [-l] [-fopenmp] [-f[no]coarray] [-dxcode] [-ddecomp] [-dump]",
      "           <input XcodeML file>",
      "           [-o <output reconstructed XcodeML file>]",
      "",
      "  -xc          process XcodeML/C document.",
      "  -xf          process XcodeML/Fortran document.",
      "  -l           suppress line directive in decompiled code.",
      "  -fopenmp     enable OpenMP translation.",
      "  -fcoarry     enable coarray translation.",
      "  -fnocoarry   pass without coarray translation (default).",
      "  -fatomicio   enable transforming Fortran IO statements to atomic operations.",
      "  -w N         set max columns to N for Fortran source.",
      "  -gnu         decompile for GNU Fortran (default).",
      "  -intel       decompile for Intel Fortran.",
      "  -M dir       specify where to search for .xmod files",
      "  -max_assumed_shape=N  set max number of assumed-shape arrays of a proedure (for Fortran).",
      "  -decomp      output decompiled source code.",
      "",
      " Debug Options:",
      "  -d           enable output debug message.",
      "  -dxcode      output Xcode file as <input file>.x",
      "  -dump        output Xcode file and decompiled file to standard output.",
      "  -domp        enable output OpenMP translation debug message.",
      " Profiling Options:",
      "  -scalasca-all      : output results in scalasca format for all directives.",
      "  -scalasca          : output results in scalasca format for selected directives.",
      "  -tlog-all          : output results in tlog format for all directives.",
      "  -tlog              : output results in tlog format for selected directives.",
      "",
      "  -enable-threads  enable 'threads' clause",
      "  -enable-gpu      enable xmp-dev directive/clauses"*/

      "arguments: [-l] [-ddecomp] [-dump]",
      "           <input XcodeML file>",
      "           [-o <output reconstructed XcodeML file>]",
      "",
      "  -l               suppress line directive in decompiled code.",
      "  -w N             set max columns to N for Fortran source.",
      "  -gnu             decompile for GNU Fortran (default).",
      "  -intel           decompile for Intel Fortran.",
      "  -M dir           specify where to search for .xmod files",
      "  -decomp          output decompiled source code.",
      "  -xcodeml         perform XcodeML transformation only. No Xobject.",
      "  -target-list     list all accelerator target available for code generation.",
      "  -target <target> specify the accelerator language target for code generation.",
      "  -config <target> specify an alternative configuration for the translator.",
      "",
      " Debug Options:",
      "  -d           enable output debug message.",
      "  -dxcode      output Xcode file as <input file>.x",
      "  -dump        output Xcode file and decompiled file to standard output.",
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
   * Main point of entry of the program.
   * @param args  Arguments of the program.
   * @throws Exception if translation failed.
   */
  public static void main(String[] args) throws Exception {
    String inXmlFile = null;
    String outXmlFile = null;
    String lang = "F"; // default language is Fortran
    String target_option = null;
    String configuration_path = null;
    boolean async = false;
    boolean outputXcode = false;
    boolean outputDecomp = false;
    boolean dump = false;
    boolean xcodeml_only = false;
    int maxColumns = 0;

    for(int i = 0; i < args.length; ++i) {
      String arg = args[i];
      String narg = (i < args.length - 1) ? args[i + 1] : null;

      if(arg.equals("-h") || arg.equals("--help")) {
        usage();
      } else if(arg.equals("-l")) {
        XmOption.setIsSuppressLineDirective(true);
      } else if(arg.equals("-w")) {
        if(narg == null)
          error("needs argument after -w");
        maxColumns = Integer.parseInt(narg);
        ++i;
      } else if(arg.equals("-dxcode")) {
        outputXcode = true;
      } else if(arg.equals("-decomp")) {
        outputDecomp = true;
      } else if(arg.equals("-dump")) {
        dump = true;
        outputXcode = true;
        outputDecomp = true;
      } else if(arg.equals("-d")) {
        XmOption.setDebugOutput(true);
      } else if(arg.equals("-o")) {
        if(narg == null)
          error("needs argument after -o");
        outXmlFile = narg;
        ++i;
      } else if(arg.equals("-gnu")) {
        XmOption.setCompilerVendor(XmOption.COMP_VENDOR_GNU);
      } else if(arg.equals("-intel")) {
        XmOption.setCompilerVendor(XmOption.COMP_VENDOR_INTEL);
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
      } else if (arg.startsWith("-xcodeml")) {
        xcodeml_only = true;
      } else if (arg.startsWith("-target-list")) {
        listTarget();
        return;
      } else if (arg.startsWith("-target")) {
        if (arg.equals("-target")) {
          if (narg == null)
            error("needs argument after target");
          target_option = narg;
          ++i;
        }
      } else if (arg.startsWith("-config")) {
        if (arg.equals("-config")) {
          if (narg == null)
            error("needs argument after config");
          configuration_path = narg;
          ++i;
        }
      } else if(arg.startsWith("-")){
        error("unknown option " + arg);
      } else if(inXmlFile == null) {
        inXmlFile = arg;
      } else {
        error("too many arguments");
      }
    }


    if(xcodeml_only){

      File config = new File(configuration_path);
      if(!config.exists()){
        error("Configuration file not found. " + configuration_path);
        return;
      }

      AcceleratorDirective target =
          AcceleratorDirective.fromString(target_option);
      ClawXcodeMlTranslator xcmlTranslator =
          new ClawXcodeMlTranslator(inXmlFile, outXmlFile, target,
              configuration_path);
      xcmlTranslator.analyze();
      xcmlTranslator.transform();
      return;
    }

/*

    Reader reader = null;
    Writer xmlWriter = null;
    Writer xcodeWriter = null;
    Writer decompWriter = null;
    File dir = null;

    if(inXmlFile == null) {
      reader = new InputStreamReader(System.in);
    } else {
      reader = new BufferedReader(new FileReader(inXmlFile));
      dir = new File(inXmlFile).getParentFile();
    }

    if(outXmlFile == null) {
      xmlWriter = new OutputStreamWriter(System.out);
    } else {
      xmlWriter = new BufferedWriter(new FileWriter(outXmlFile));
    }

    if(dump || outputXcode) {
      if(dump) {
        xcodeWriter = new OutputStreamWriter(System.out);
      } else {
        xcodeWriter = new BufferedWriter(new FileWriter(inXmlFile + ".x"));
      }
    }

    XmToolFactory toolFactory = new XmToolFactory(lang);
    XmOption.setLanguage(XmLanguage.valueOf(lang));

    XobjectFile xobjFile;
    String srcPath = inXmlFile;
    XcodeMLtools tools = null;
    if (XmOption.getLanguage() == XmLanguage.F) {
      tools = new XcodeMLtools_F();
    } else {
      tools = new XcodeMLtools_C();
    }

    // read XcodeML
    xobjFile = tools.read(reader);
    if (inXmlFile != null) {
      reader.close();
    }

    String baseName = null;
    if(dump || srcPath == null || srcPath.indexOf("<") >= 0 ) {
      srcPath = null;
    } else {
      String fileName = new File(srcPath).getName();
      //      int idx = fileName.indexOf(".");
      int idx = fileName.lastIndexOf(".");
      if(idx < 0) {
        XmLog.fatal("invalid source file name : " + fileName);
      }
      baseName = fileName.substring(0, idx);
    }

    if(xobjFile == null)
      System.exit(1);

    // Output Xcode
    if(xcodeWriter != null) {
      xobjFile.Output(xcodeWriter);
      xcodeWriter.flush();
    }


    // Transformation will happen here
    CLAWtranslator claw_translator = new CLAWtranslator();
    xobjFile.iterateDef(claw_translator);




    if(!dump && outputXcode) {
      xcodeWriter.close();
    }

    // translate Xcode to XcodeML
    XmXcodeProgram xmprog = null;
    Document xcodeDoc = null;
    XmXobjectToXcodeTranslator xc2xcodeTranslator = null;

    if (lang.equals("F")) {
      xc2xcodeTranslator = new XmfXobjectToXcodeTranslator();
    } else {
      xc2xcodeTranslator = new XmcXobjectToXcodeTranslator();
    }

    xcodeDoc = xc2xcodeTranslator.write(xobjFile);

    Transformer transformer = null;
    try {
      transformer = TransformerFactory.newInstance().newTransformer();
    } catch(TransformerConfigurationException e) {
      throw new XmException(e);
    }

    transformer.setOutputProperty(OutputKeys.METHOD, "xml");

    try {
      transformer.transform(new DOMSource(xcodeDoc), new StreamResult(xmlWriter));
    } catch(TransformerException e) {
      throw new XmException(e);
    }

    if (!dump && !outputDecomp) {
      xmprog = null;
    }

    xmlWriter.flush();

    if(outXmlFile != null) {
      xmlWriter.close();
      xmlWriter = null;
    }

    // Decompile
    XmDecompilerContext context = null;
    if(lang.equals("F")) {
      context = toolFactory.createDecompilerContext();
      if(maxColumns > 0)
        context.setProperty(XmDecompilerContext.KEY_MAX_COLUMNS, "" + maxColumns);
    }

    if(outputDecomp) {
      if(dump || srcPath == null) {
        decompWriter = new OutputStreamWriter(System.out);
      }
      else { // set decompile writer
        String newFileName = baseName + "." + (XmOption.isLanguageC() ? "c" : "F90");
        String newFileName2 = baseName + "." + (XmOption.isLanguageC() ? "c" : "f90");
        File newFile = new File(dir, newFileName);
        File newFile2 = new File(dir, newFileName2);

        if(newFile.exists())
          newFile.renameTo(new File(dir, newFileName + ".i"));
        if(newFile2.exists())
          newFile2.renameTo(new File(dir, newFileName2 + ".i"));

        decompWriter = new BufferedWriter(new FileWriter(newFile));
      }

      XmDecompiler decompiler = toolFactory.createDecompiler();

      if (xcodeDoc == null) {
        javax.xml.parsers.DocumentBuilderFactory docFactory = javax.xml.parsers.DocumentBuilderFactory.newInstance();
        javax.xml.parsers.DocumentBuilder builder = docFactory.newDocumentBuilder();
        xcodeDoc = builder.parse(outXmlFile);
      }
      decompiler.decompile(context, xcodeDoc, decompWriter);
      // for collect-init
      decompWriter.write(xobjFile.getTailText());
      decompWriter.flush();

      if(!dump && outputDecomp) {
        decompWriter.close();
      }
    }
*/

  }

}

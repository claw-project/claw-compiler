package cx2x.translator.xcode;

import cx2x.translator.common.GroupConfiguration;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import exc.object.XobjectFile;
import exc.openmp.OMP;
import exc.openmp.OMPtranslate;
import exc.xcodeml.XcodeMLtools;
import exc.xcodeml.XcodeMLtools_F;
import exc.xcodeml.XmXobjectToXcodeTranslator;
import exc.xcodeml.XmfXobjectToXcodeTranslator;
import org.w3c.dom.Document;
import xcodeml.XmException;
import xcodeml.XmLanguage;
import xcodeml.util.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.*;
import java.util.List;

/**
 * Handle the conversion from XcodeML to Xcode representation and apply the
 * CLAW translation on it.
 *
 * @author clementval
 */
public class ClawXcodeTranslator {
  private final String lang = "F"; // Only support Fortran
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private final AcceleratorGenerator _generator;
  private final Target _target;


  /**
   * ClawXcodeTranslator ctor.
   * @param xcodemlInputFile  The XcodeML input file path.
   * @param xcodemlOutputFile The XcodeML output file path.
   * @param directive         Accelerator directlve language for code
   *                          generation.
   * @param target            Target influencing code transformation.
   * @param groups            Transformation groups configuration list.
   */
  public ClawXcodeTranslator(String xcodemlInputFile,
                             String xcodemlOutputFile,
                             AcceleratorDirective directive,
                             Target target,
                             List<GroupConfiguration> groups)
  {
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _target = target;
    _generator =
        AcceleratorHelper.createAcceleratorGenerator(directive, target);
  }

  /**
   * Read the XcodeML file, apply the translation and produce the XcodeML
   * output file as well as the Fotran file.
   * @throws Exception
   */
  public void translate() throws Exception {
    Reader reader = null;
    Writer xmlWriter = null;
    Writer xcodeWriter = null;
    Writer decompWriter = null;
    File dir = null;

    if(_xcodemlInputFile == null) {
      reader = new InputStreamReader(System.in);
    } else {
      reader = new BufferedReader(new FileReader(_xcodemlInputFile));
      dir = new File(_xcodemlInputFile).getParentFile();
    }

    if(_xcodemlOutputFile == null) {
      xmlWriter = new OutputStreamWriter(System.out);
    } else {
      xmlWriter = new BufferedWriter(new FileWriter(_xcodemlOutputFile));
    }

    XmToolFactory toolFactory = new XmToolFactory(lang);
    XmOption.setLanguage(XmLanguage.valueOf(lang));

    XobjectFile xobjFile;
    XcodeMLtools tools = new XcodeMLtools_F();

    // Read XcodeML to Xcode
    xobjFile = tools.read(reader);
    if (_xcodemlOutputFile != null) {
      reader.close();
    }
    if(xobjFile == null) {
      System.exit(1);
    }

    String baseName = null;
    if(_xcodemlOutputFile == null || _xcodemlOutputFile.indexOf("<") >= 0 ) {
      _xcodemlOutputFile = null;
    } else {
      String fileName = new File(_xcodemlOutputFile).getName();
      int idx = fileName.lastIndexOf(".");
      if(idx < 0) {
        XmLog.fatal("invalid source file name : " + fileName);
      }
      baseName = fileName.substring(0, idx);
    }

    // Output Xcode
    if(xcodeWriter != null) {
      xobjFile.Output(xcodeWriter);
      xcodeWriter.flush();
    }
    System.gc();

    // CLAW translation
    // TODO set up CLAW translate
    /*OMPtranslate omp_translator = new OMPtranslate(xobjFile);
    xobjFile.iterateDef(omp_translator);

    if(OMP.hasErrors())
      System.exit(1);

    omp_translator.finish();
    */
    if(xcodeWriter != null) {
      xobjFile.Output(xcodeWriter);
      xcodeWriter.flush();
    }

    // Translate Xcode to XcodeML
    Document xcodeDoc;
    XmXobjectToXcodeTranslator xc2xcodeTranslator = null;


    if (lang.equals("F")) {
      xc2xcodeTranslator = new XmfXobjectToXcodeTranslator();
    }
    xcodeDoc = xc2xcodeTranslator.write(xobjFile);

    Transformer transformer;
    try {
      transformer = TransformerFactory.newInstance().newTransformer();
    } catch(TransformerConfigurationException e) {
      throw new XmException(e);
    }

    transformer.setOutputProperty(OutputKeys.METHOD, "xml");

    try {
      transformer.transform(new DOMSource(xcodeDoc),
          new StreamResult(xmlWriter));
    } catch(TransformerException e) {
      throw new XmException(e);
    }

    xmlWriter.flush();

    if(_xcodemlOutputFile != null) {
      xmlWriter.close();
    }

    XmDecompilerContext context = null;

    context = toolFactory.createDecompilerContext();
    // TODO set as option
    int maxColumns = 80;
    if(maxColumns > 0) {
      context.setProperty(XmDecompilerContext.KEY_MAX_COLUMNS, "" +
          maxColumns);
    }


    // Decompile XcodeML to Fortran
    String newFileName = baseName + ".f90";
    String newFileName2 = baseName + ".f90";
    File newFile = new File(dir, newFileName);
    File newFile2 = new File(dir, newFileName2);

    if(newFile.exists())
      newFile.renameTo(new File(dir, newFileName + ".i"));
    if(newFile2.exists())
      newFile2.renameTo(new File(dir, newFileName2 + ".i"));

    decompWriter = new BufferedWriter(new FileWriter(newFile));
    XmDecompiler decompiler = toolFactory.createDecompiler();

    if (xcodeDoc == null) {
      DocumentBuilderFactory docFactory =
          DocumentBuilderFactory.newInstance();
      DocumentBuilder builder = docFactory.newDocumentBuilder();
      xcodeDoc = builder.parse(_xcodemlOutputFile);
    }
    decompiler.decompile(context, xcodeDoc, decompWriter);
    decompWriter.flush();
    decompWriter.close();
  }

}

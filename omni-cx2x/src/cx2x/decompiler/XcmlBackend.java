/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.decompiler;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import xcodeml.util.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;


/**
 * Wrapper class to call the Fortran decompiler of OMNI Compiler directly
 * from Java instead of calling it as a separated program.
 *
 * @author clementval
 */
public class XcmlBackend {

  private BufferedReader _reader;
  private XmToolFactory _toolFactory;

  /**
   * Constructs a new XcmlBackend object.
   *
   * @throws XmException If instantiation of the XmToolFactory fails.
   */
  public XcmlBackend(Lang lang)
      throws XmException
  {
    switch(lang) {
      case FORTRAN:
        _toolFactory = new XmToolFactory("F");
        break;
      case C:
        _toolFactory = new XmToolFactory("C");
        break;
    }
  }

  private boolean openXcodeMLFile(String inputFilepath)
  {
    if(_reader != null) {
      try {
        _reader.close();
      } catch(IOException e) {
        e.printStackTrace();
        return false;
      }
    }
    try {
      _reader = new BufferedReader(new FileReader(inputFilepath));
      return true;
    } catch(IOException e) {
      return false;
    }
  }

  /**
   * Decompile the XcodeML file into Fortran code.
   *
   * @param outputFilepath Fortran output file path.
   * @param inputFilepath  XcodeML input file path.
   * @param maxColumns     Maximum number of column for the output file.
   * @param lineDirectives If true, preprocessor line directives are added.
   * @return True if the decompilation succeeded. False otherwise.
   */
  public boolean decompile(String outputFilepath, String inputFilepath,
                           int maxColumns, boolean lineDirectives)
  {
    if(!lineDirectives) {
      XmOption.setIsSuppressLineDirective(true);
    }
    XmOption.setCoarrayNoUseStatement(true);
    XmOption.setDebugOutput(false);

    if(!openXcodeMLFile(inputFilepath)) {
      return false;
    }

    PrintWriter writer = null;
    try {
      File outputFile = new File(outputFilepath);
      outputFile.getParentFile().mkdir();
      writer = new PrintWriter(new BufferedWriter(new FileWriter(outputFile)));
    } catch(IOException e) {
      e.printStackTrace();
    }

    try {
      XmDecompiler decompiler = _toolFactory.createDecompiler();
      XmDecompilerContext context = _toolFactory.createDecompilerContext();

      if(maxColumns > 0) {
        context.setProperty(XmDecompilerContext.KEY_MAX_COLUMNS, "" + maxColumns);
      }

      try {
        DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = docFactory.newDocumentBuilder();
        Document xcodeDoc;
        xcodeDoc = builder.parse(inputFilepath);
        decompiler.decompile(context, xcodeDoc, writer);
      } catch(ParserConfigurationException | SAXException | IOException e) {
        return false;
      }

      if(writer != null) {
        writer.flush();
      } else {
        return false;
      }
      return true;
    } catch(Exception ex) {
      if(_reader != null) {
        try {
          _reader.close();
        } catch(IOException ignored) {
        }
      }

      if(writer != null) {
        writer.close();
      }
    }
    return false;
  }

  public enum Lang {
    C,
    FORTRAN
  }

}

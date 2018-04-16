/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.error.XanalysisError;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import org.w3c.dom.Document;

import java.io.BufferedInputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The XcodeProgram represents the XcodeProgram (2) element in XcodeML
 * intermediate representation.
 *
 * Elements: ( typeTable, globalSymbols, globalDeclarations )
 * - Required:
 * - typeTable (XtypeTable)
 * - globalSymbols (XsymbolTable)
 * - globalDeclarations (XglobalDeclTable)
 * Attributes:
 * - Optional: compiler-info (text), version (text), time (time),
 * language (text), source (text)
 *
 * @author clementval
 */

public class XcodeProgram extends XcodeML {

  private final List<XanalysisError> _errors;
  private final List<XanalysisError> _warnings;
  // XcodeProgram inner elements
  private XsymbolTable _globalSymbolsTable = null;
  private XglobalDeclTable _globalDeclarationsTable = null;

  /**
   * XcodeProgram base constructor.
   *
   * @param doc The XcodeML document.
   */
  private XcodeProgram(Document doc) {
    super(doc);
    _errors = new ArrayList<>();
    _warnings = new ArrayList<>();
  }

  /**
   * Create a XcodeProgram object from the standard input. Used when connected
   * through pipe.
   *
   * @return An XcodeProgram object loaded with the information from the std
   * input. Null if the std input couldn't be read.
   */
  public static XcodeProgram createFromStdInput() {
    BufferedInputStream bis = new BufferedInputStream(System.in);
    Document doc = readXmlStream(bis);
    return createFromDocument(doc);
  }

  /**
   * Create a XcodeProgram object from an XML document.
   *
   * @param doc Input DOM document.
   * @return A XcodeProgram object loaded with the information from the file.
   * null if the file couldn't be read.
   */
  public static XcodeProgram createFromDocument(Document doc) {
    if(doc == null) {
      System.err.println("Unable to read document");
      return null;
    }
    XcodeProgram program = new XcodeProgram(doc);
    program.readDocumentInformation();
    if(!program.isXcodeMLvalid()) {
      System.err.print("XcodeML file is not valid");
      return null;
    }
    return program;
  }

  /**
   * Create a XcodeProgram object from an XcodeML input file.
   *
   * @param input XcodeML input filename or path
   * @return An XcodeProgram object loaded with the information from the file.
   * Null if the file couldn't be read.
   */
  public static XcodeProgram createFromFile(String input) {
    Document doc = XnodeUtil.readXmlFile(input);
    return createFromDocument(doc);
  }

  /**
   * Read all the XcodeML document information: version, language, time, source,
   * compiler info.
   */
  private void readDocumentInformation() {
    readGlobalSymbolsTable();
    readGlobalDeclarationsTable();
  }

  /**
   * Add an error.
   *
   * @param msg    Error message.
   * @param lineno Line number that triggered the error.
   */
  public void addError(String msg, int lineno) {
    _errors.add(new XanalysisError(msg, lineno));
  }

  /**
   * Get all the errors.
   *
   * @return A list containing all the errors.
   */
  public List<XanalysisError> getErrors() {
    return _errors;
  }

  /**
   * Add a warning.
   *
   * @param msg    Warning message.
   * @param lineno Line number that triggered the warning.
   */
  public void addWarning(String msg, int lineno) {
    _warnings.add(new XanalysisError(msg, lineno));
  }

  /**
   * Add a warning.
   *
   * @param msg    Warning message.
   * @param lineno Line numbers that triggered the warning.
   */
  public void addWarning(String msg, List<Integer> lineno) {
    _warnings.add(new XanalysisError(msg, lineno));
  }

  /**
   * Get all the warnings.
   *
   * @return A list containing all the warnings.
   */
  public List<XanalysisError> getWarnings() {
    return _warnings;
  }

  /**
   * Get the symbols table of the XcodeML program.
   *
   * @return The symbols table.
   */
  public XsymbolTable getGlobalSymbolsTable() {
    return _globalSymbolsTable;
  }

  /**
   * Get the declarations table of the XcodeML program.
   *
   * @return The declarations table.
   */
  public XglobalDeclTable getGlobalDeclarationsTable() {
    return _globalDeclarationsTable;
  }

  /**
   * Check whether the XcodeML input file match the requirements.
   *
   * @return True if the XcodeML file matches the requirements.
   */
  private boolean isXcodeMLvalid() {
    if(getDocument() == null) {
      return false;
    }

    if(opcode() != Xcode.XCODE_PROGRAM) {
      return false;
    }

    if(!Xname.SUPPORTED_VERSION.equals(getAttribute(Xattr.VERSION))) {
      System.err.println("XcodeML version is not supported");
      return false;
    }

    if(!Xname.SUPPORTED_LANGUAGE.equals(getAttribute(Xattr.LANGUAGE))) {
      System.err.println("Language is not set to fortran");
      return false;
    }

    return true;
  }

  /**
   * Read the XcodeML global symbols table
   */
  private void readGlobalSymbolsTable() {
    _globalSymbolsTable = new XsymbolTable(matchSeq(Xcode.GLOBAL_SYMBOLS));
  }

  /**
   * Read the XcodeML global declarations table
   */
  private void readGlobalDeclarationsTable() {
    _globalDeclarationsTable =
        new XglobalDeclTable(matchSeq(Xcode.GLOBAL_DECLARATIONS));
  }

  /**
   * Get the XcodeML version.
   *
   * @return XcodeML version attribute value.
   */
  public String getVersion() {
    return getAttribute(Xattr.VERSION);
  }

  /**
   * Get the XcodeML language.
   *
   * @return XcodeML language attribute value.
   */
  public String getLanguage() {
    return getAttribute(Xattr.LANGUAGE);
  }

  /**
   * Get the XcodeML generation time.
   *
   * @return XcodeML time attribute value.
   */
  public String getTime() {
    return getAttribute(Xattr.TIME);
  }

  /**
   * Get the XcodeML source file information.
   *
   * @return Source file attribute value.
   */
  public String getSource() {
    return getAttribute(Xattr.SOURCE);
  }

  /**
   * Get the XcodeML source file information. Only filename without the
   * directory.
   *
   * @return Filename part of the source file attribute value.
   */
  public String getSourceFileOnly() {
    String source = getSource();
    File f = new File(source);
    return f.getName();
  }

  /**
   * Get the XcodeML compiler information.
   *
   * @return Compiler information attribute value.
   */
  public String getCompilerInfo() {
    return getAttribute(Xattr.COMPILER_INFO);
  }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.error.XanalysisError;
import cx2x.xcodeml.helper.XnodeUtil;
import org.w3c.dom.Document;

import java.util.ArrayList;
import java.util.List;

/**
 * The XcodeProgram represents the XcodeProgram (2) element in XcodeML
 * intermediate representation.
 * <p>
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
  // XcodeProgram optional attributes
  private String _version = null;
  private String _language = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;

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
   * Create a XcodeProgram object from an XcodeML input file.
   *
   * @param input XcodeML input filename or path
   * @return A XcodeProgram object loaded with the information from the file.
   * Null if the file couldn't be read.
   */
  public static XcodeProgram createFromFile(String input) {
    Document doc = XnodeUtil.readXmlFile(input);
    if(doc == null) {
      System.err.println("Input file does not exists: " + input + "\n");
      return null;
    }
    XcodeProgram program = new XcodeProgram(doc);
    program.readDocumentInformation();
    if(!program.isXcodeMLvalid()) {
      System.err.print("XcodeML file is not valid\n");
      return null;
    }
    return program;
  }

  /**
   * Read all the XcodeML document information: version, language, time, source,
   * compiler info.
   */
  private void readDocumentInformation() {
    _version = getAttribute(Xattr.VERSION);
    _language = getAttribute(Xattr.LANGUAGE);
    _time = getAttribute(Xattr.TIME);
    _source = getAttribute(Xattr.SOURCE);
    _compilerInfo = getAttribute(Xattr.COMPILER_INFO);

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

    if(opcode() != Xcode.XCODEPROGRAM) {
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
    _globalSymbolsTable =
        new XsymbolTable(matchSeq(Xcode.GLOBALSYMBOLS).getElement());
  }

  /**
   * Read the XcodeML global declarations table
   */
  private void readGlobalDeclarationsTable() {
    _globalDeclarationsTable =
        new XglobalDeclTable(matchSeq(Xcode.GLOBALDECLARATIONS).getElement());
  }

  /**
   * Get the XcodeML version.
   *
   * @return XcodeML version attribute value.
   */
  public String getVersion() {
    return _version;
  }

  /**
   * Get the XcodeML language.
   *
   * @return XcodeML language attribute value.
   */
  public String getLanguage() {
    return _language;
  }

  /**
   * Get the XcodeML generation time.
   *
   * @return XcodeML time attribute value.
   */
  public String getTime() {
    return _time;
  }

  /**
   * Get the XcodeML source file information.
   *
   * @return Source file attribute value.
   */
  public String getSource() {
    return _source;
  }

  /**
   * Get the XcodeML compiler information.
   *
   * @return Compiler information attribute value.
   */
  public String getCompilerInfo() {
    return _compilerInfo;
  }

}

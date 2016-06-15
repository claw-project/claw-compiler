/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Document;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import cx2x.xcodeml.error.*;

/**
 * The XcodeProgram represents the XcodeProgram (2) element in XcodeML
 * intermediate representation.
 *
 * Elements: ( typeTable, globalSymbols, globalDeclarations )
 * - Required:
 *   - typeTable (XtypeTable)
 *   - globalSymbols (XsymbolTable)
 *   - globalDeclarations (XglobalDeclTable)
 * Attributes:
 * - Optional: compiler-info (text), version (text), time (time),
 *             language (text), source (text)
 *
 * @author clementval
 */

public class XcodeProgram extends Xnode {
  private Document _xcodemlDoc = null;

  // XcodeProgram inner elements
  private XtypeTable _typeTable = null;
  private XsymbolTable _globalSymbolsTable = null;
  private XglobalDeclTable _globalDeclarationsTable = null;

  // XcodeProgram optional attributes
  private String _version = null;
  private String _language = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;

  private final List<XanalysisError> _errors;
  private final List<XanalysisError> _warnings;

  /**
   * XcodeProgram base constructor.
   * @param doc The XcodeML document.
   */
  private XcodeProgram(Document doc){
    super(doc.getDocumentElement());
    _xcodemlDoc = doc;
    _errors = new ArrayList<>();
    _warnings = new ArrayList<>();
  }

  /**
   * Read all the XcodeML document information: version, language, time, source,
   * compiler info.
   */
  private void readDocumentInformation(){
    _version = getAttribute(Xattr.VERSION);
    _language = getAttribute(Xattr.LANGUAGE);
    _time = getAttribute(Xattr.TIME);
    _source = getAttribute(Xattr.SOURCE);
    _compilerInfo = getAttribute(Xattr.COMPILER_INFO);

    readTypeTable();
    readGlobalSymbolsTable();
    readGlobalDeclarationsTable();
  }

  /**
   * Add an error.
   * @param msg     Error message.
   * @param lineno  Line number that triggered the error.
   */
  public void addError(String msg, int lineno){
    _errors.add(new XanalysisError(msg, lineno));
  }

  /**
   * Get all the errors.
   * @return A list containing all the errors.
   */
  public List<XanalysisError> getErrors(){
    return _errors;
  }

  /**
   * Add a warning.
   * @param msg     Warning message.
   * @param lineno  Line number that triggered the warning.
   */
  public void addWarning(String msg, int lineno){
    _warnings.add(new XanalysisError(msg, lineno));
  }

  /**
   * Get all the warnings.
   * @return A list containing all the warnings.
   */
  public List<XanalysisError> getWarnings(){
    return _warnings;
  }

  /**
   * Purge all current warnings.
   */
  public void purgeWarning(){
    _warnings.clear();
  }

  /**
   * @return The XML Document representing the XcodeML program.
   */
  public Document getDocument(){
    return _xcodemlDoc;
  }

  /**
   * @return The root element of the XcodeML program.
   */
  public Element getBaseElement(){
    return (_xcodemlDoc != null) ? _xcodemlDoc.getDocumentElement() : null;
  }

  /**
   * Get the type table of the XcodeML program..
   * @return The types table.
   */
  public XtypeTable getTypeTable(){
    return _typeTable;
  }

  /**
   * Get the symbols table of the XcodeML program.
   * @return The symbols table.
   */
  public XsymbolTable getGlobalSymbolsTable() {
    return _globalSymbolsTable;
  }

  /**
   * Get the delcarations table of the XcodeML program.
   * @return The declarations table.
   */
  public XglobalDeclTable getGlobalDeclarationsTable(){
    return _globalDeclarationsTable;
  }

  /**
   * Check whether the XcodeML input file match the requirements.
   * @return True if the XcodeML file matches the requirements.
   * @throws Exception
   */
  private boolean isXcodeMLvalid() throws Exception {
    if(_xcodemlDoc == null){
      return false;
    }

    if(Opcode() != Xcode.XCODEPROGRAM){
      return false;
    }

    if(!Xname.SUPPORTED_VERSION.equals(getAttribute(Xattr.VERSION))) {
      System.err.println("XcodeML version is not supported");
      return false;
    }

    if(!Xname.SUPPORTED_LANGUAGE.equals(getAttribute(Xattr.LANGUAGE))){
      System.err.println("Language is not set to fortran");
      return false;
    }

    return true;
  }

  /**
   * Read the XcodeML type table
   */
  private void readTypeTable() {
    _typeTable = new XtypeTable(find(Xcode.TYPETABLE).getElement());
  }

  /**
   * Read the XcodeML global symbols table
   */
  private void readGlobalSymbolsTable() {
    _globalSymbolsTable =
        new XsymbolTable(find(Xcode.GLOBALSYMBOLS).getElement());
  }

  /**
   * Read the XcodeML global declarations table
   */
  private void readGlobalDeclarationsTable(){
    _globalDeclarationsTable =
        new XglobalDeclTable(find(Xcode.GLOBALDECLARATIONS).getElement());
  }

  /**
   * Get the XcodeML version.
   * @return XcodeML version attribute value.
   */
  public String getVersion(){
    return _version;
  }

  /**
   * Get the XcodeML language.
   * @return XcodeML language attribute value.
   */
  public String getLanguage(){
    return _language;
  }

  /**
   * Get the XcodeML generation time.
   * @return XcodeML time attribute value.
   */
  public String getTime(){
    return _time;
  }

  /**
   * Get the XcodeML source file information.
   * @return Source file attribute value.
   */
  public String getSource(){
    return _source;
  }

  /**
   * Get the XcodeML compiler information.
   * @return Compiler information attribute value.
   */
  public String getCompilerInfo(){
    return _compilerInfo;
  }


  /**
   * Create a XcodeProgram object from an XcodeML input file.
   * @param input XcodeML input filename or path
   * @return A XcodeProgram object loaded with the information from the file.
   * Null if the file couldn't be read.
   */
  public static XcodeProgram createFromFile(String input){
    try {
      File fXmlFile = new File(input);
      if(!fXmlFile.exists()){
        System.err.println("Input file does not exists: " + input + "\n");
        return null;
      }
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();

      XcodeProgram program = new XcodeProgram(doc);
      program.readDocumentInformation();
      if (!program.isXcodeMLvalid()){
        System.err.print("XcodeML file is not valid\n");
        return null;
      }
      return program;
    } catch(Exception ex){
      System.err.print("Unable to read file " + input + "\n");
    }
    return null;
  }

}

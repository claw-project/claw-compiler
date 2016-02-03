/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Document;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

import cx2x.xcodeml.error.*;
import cx2x.xcodeml.helper.*;

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

public class XcodeProgram extends XbaseElement {
  private Document _xcodemlDoc = null;
  private String _xcodemlInputFile = null;

  // XcodeProgram inner elements
  private XtypeTable _typeTable = null;
  private XglobalSymbolTable _globalSymbolsTable = null;
  private XglobalDeclTable _globalDeclarationsTable = null;

  // XcodeProgram optional attributes
  private String _version = null;
  private String _language = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;


  private boolean _isLoaded = false;

  private List<XanalysisError> _errors;

  /**
   * XcodeProgram base constructor.
   * @param inputFile The XcodeML input file path.
   */
  public XcodeProgram(String inputFile){
    super(null);
    _xcodemlInputFile = inputFile;
    _errors = new ArrayList<>();
  }

  /**
   * Constructs a new XcodeProgram object from a single root element.
   * @param doc The XcodeML document.
   */
  public XcodeProgram(Document doc){
    super(doc.getDocumentElement());
    _xcodemlDoc = doc;
  }

  /**
   * Read all the XcodeML document information: version, language, time, source,
   * compiler info.
   */
  private void readDocumentInformation(){
    _version = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_VERSION);
    _language = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_LANGUAGE);
    _time = XelementHelper.getAttributeValue(this, XelementName.ATTR_TIME);
    _source = XelementHelper.getAttributeValue(this, XelementName.ATTR_SOURCE);
    _compilerInfo = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_COMPILER_INFO);
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
   * Open the XcodeML input file and read its information.
   * @return True if the XcodeML was loaded successfully. False otherwise.
   */
  public boolean load(){
    try {
      File fXmlFile = new File(_xcodemlInputFile);
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();
      _xcodemlDoc = doc;
      _isLoaded = true;
      baseElement = _xcodemlDoc.getDocumentElement();
      readDocumentInformation();

      if (!isXcodeMLvalid()){
        addError("XcodeML document is not valid", 0);
        return false;
      }
    } catch(Exception ex){
      _xcodemlDoc = null;
      _isLoaded = false;
      return false;
    }

    // Read information from the type table
    readTypeTable();
    readGlobalSymbolsTable();
    readGlobalDeclarationsTable();
    return true;
  }

  /**
   * Check whether the XcodeML is loaded.
   * @return True if the XcodeML is loaded. False otherwise.
   */
  public boolean isLoaded(){
    return _isLoaded;
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

    Element root = _xcodemlDoc.getDocumentElement();
    if(!root.getNodeName().equals(XelementName.X_CODE_PROGRAM)){
      return false;
    }

    if(!XelementHelper.validateStringAttribute(_xcodemlDoc,
      XelementName.SUPPORTED_VERSION, "/" + XelementName.X_CODE_PROGRAM + "/@"
      + XelementName.ATTR_VERSION))
    {
      System.err.println("XcodeML version is not supported");
      return false;
    }

    if(!XelementHelper.validateStringAttribute(_xcodemlDoc,
      XelementName.SUPPORTED_LANGUAGE, "/" + XelementName.X_CODE_PROGRAM + "/@"
      +  XelementName.ATTR_LANGUAGE))
    {
      System.err.println("Language is not set to fortran");
      return false;
    }

    return true;
  }

  /**
   * Read the XcodeML type table
   */
  private void readTypeTable() {
    _typeTable = XelementHelper.findTypeTable(this, true);
  }

  /**
   * Read the XcodeML global symbols table
   */
  private void readGlobalSymbolsTable() {
    _globalSymbolsTable = XelementHelper.findGlobalSymbols(this, true);
  }

  /**
   * Read the XcodeML global declarations table
   */
  private void readGlobalDeclarationsTable(){
    _globalDeclarationsTable = XelementHelper
        .findGlobalDeclarations(this, true);
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

}

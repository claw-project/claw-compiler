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

/**
 * The XcodeProg represents the XcodeProgram (2) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - typeTable (XtypeTable)
 *   - globalSymbols (XsymbolTable)
 *   - globalDeclarations TODO
 * Attributes:
 * - Optional: compiler-info (text), version (text), time (time),
 *             language (text), source (text)
 *
 * @author clementval
 */

public class XcodeProg extends XbaseElement {
  private Document _xcodemlDoc = null;
  private String _xcodemlInputFile = null;

  // XcodeProg inner elements
  private XtypeTable _typeTable = null;
  private XglobalSymbolTable _globalSymbolsTable = null;

  // XcodeProg optional attributes
  private String _version = null;
  private String _language = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;


  private boolean _isLoaded = false;

  private List<XanalysisError> _errors;

  public XcodeProg(String inputFile){
    super(null);
    _xcodemlInputFile = inputFile;
    _errors = new ArrayList<>();
  }

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

  public void addError(String msg, int lineno){
    _errors.add(new XanalysisError(msg, lineno));
  }

  public List<XanalysisError> getErrors(){
    return _errors;
  }

  public Document getDocument(){
    return _xcodemlDoc;
  }

  public Element getBaseElement(){
    return (_xcodemlDoc != null) ? _xcodemlDoc.getDocumentElement() : null;
  }

  public XtypeTable getTypeTable(){
    return _typeTable;
  }

  public XsymbolTable getGlobalSymbolsTable() {
    return _globalSymbolsTable;
  }

  // Read the XcodeML file and load its object representation
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
    return true;
  }

  public boolean isLoaded(){
    return _isLoaded;
  }

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

  private void readTypeTable() {
    _typeTable = XelementHelper.findTypeTable(this, true);
  }

  private void readGlobalSymbolsTable() {
    _globalSymbolsTable = XelementHelper.findGlobalSymbols(this, true);
  }

}

package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Document;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.util.Hashtable;

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
 */

public class XcodeProg {
  private Document _xcodemlDoc = null;
  private String _xcodemlInputFile = null;

  // Xcode inner elements
  private XtypeTable _typeTable = null;
  private XsymbolTable _globalSymbolsTable = null;

  // Xcode optional attributes
  private String _version = null;
  private String _lanaguage = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;


  private boolean _isLoaded = false;

  public XcodeProg(String inputFile){
    _xcodemlInputFile = inputFile;
  }

  private void readDocumentInformation(){
    _version = XelementHelper.getAttributeValue(
      _xcodemlDoc.getDocumentElement(), XelementName.ATTR_VERSION);
    _lanaguage = XelementHelper.getAttributeValue(
      _xcodemlDoc.getDocumentElement(), XelementName.ATTR_LANGUAGE);
    _time = XelementHelper.getAttributeValue(
      _xcodemlDoc.getDocumentElement(), XelementName.ATTR_TIME);
    _source = XelementHelper.getAttributeValue(
      _xcodemlDoc.getDocumentElement(), XelementName.ATTR_SOURCE);
    _compilerInfo = XelementHelper.getAttributeValue(
      _xcodemlDoc.getDocumentElement(), XelementName.ATTR_COMPILER_INFO);
  }

  public Document getDocument(){
    return _xcodemlDoc;
  }

  public XtypeTable getTypeTable(){
    return _typeTable;
  }

  public XsymbolTable getGlobalSymbolsTable() {
    return _globalSymbolsTable;
  }

  // Read the XcodeML file and load its object representation
  public void load(){
    try {
      File fXmlFile = new File(_xcodemlInputFile);
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();
      _xcodemlDoc = doc;
      readDocumentInformation();
      _isLoaded = true;
    } catch(Exception ex){
      _xcodemlDoc = null;
    }
  }

  public boolean isLoaded(){
    return _isLoaded;
  }

  public boolean isXcodeMLvalid() throws Exception {
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

  public void readTypeTable() {
    Element typeTableElement = XelementHelper.findTypeTable(_xcodemlDoc);
    _typeTable = new XtypeTable(typeTableElement);
  }

  public void readGlobalSymbolsTable() {
    Element globalSymbolsElement = XelementHelper.findGlobalSymbols(_xcodemlDoc);
    _globalSymbolsTable = new XsymbolTable(globalSymbolsElement);
  }

}

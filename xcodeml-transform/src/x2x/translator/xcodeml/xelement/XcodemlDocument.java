package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Document;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.File;
import java.util.Hashtable;

public class XcodemlDocument{
  private Document _xcodemlDoc = null;
  private String _xcodemlInputFile = null;
  private Element _typeTableElement = null;
  private XtypeTable _typeTable = null;
  private String _version = null;
  private String _lanaguage = null;
  private String _time = null;
  private String _source = null;
  private String _compilerInfo = null;
  private boolean _isLoaded = false;

  public XcodemlDocument(String inputFile){
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

  public boolean isLoaded(){
    return _isLoaded;
  }

  public void readXcodeML(){
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

  public void readTypeTable(){
    _typeTableElement = XelementHelper.findTypeTable(_xcodemlDoc);
    _typeTable = new XtypeTable(_typeTableElement);
  }

}

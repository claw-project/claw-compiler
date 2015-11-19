package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Document;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.*;
import java.io.File;
import java.util.Hashtable;

public class XcodemlDocument{
  private Document _xcodemlDoc = null;
  private String _xcodemlInputFile = null;
  private Element _typeTableElement = null;
  private XtypeTable _typeTable = null;

  public XcodemlDocument(String inputFile){
    _xcodemlInputFile = inputFile;
  }

  public Document getDocument(){
    return _xcodemlDoc;
  }

  public XtypeTable getTypeTable(){
    return _typeTable;
  }

  public void readXcodeML(){
    try {
      File fXmlFile = new File(_xcodemlInputFile);
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();
      _xcodemlDoc = doc;
    } catch(Exception ex){
      _xcodemlDoc = null;
    }
  }

  public boolean isXcodeMLvalid() throws Exception {
    if(_xcodemlDoc == null){
      return false;
    }

    Element root = _xcodemlDoc.getDocumentElement();
    if(!root.getNodeName().equals("XcodeProgram")){ // TODO const or enum
      return false;
    }

    if(!validateStringAttribute("1.0", "/XcodeProgram/@version")){
      System.err.println("Language is not set to fortran");
      return false;
    }

    if(!validateStringAttribute("Fortran", "/XcodeProgram/@language")){
      System.err.println("Language is not set to fortran");
      return false;
    }

    return true;
  }

  public void readTypeTable(){
    _typeTableElement = XelementHelper.findTypeTable(_xcodemlDoc);
    _typeTable = new XtypeTable(_typeTableElement);
  }

  private boolean validateStringAttribute(String attrValue, String xpathQuery) throws Exception {
    XPathFactory xPathfactory = XPathFactory.newInstance();
    XPath xpath = xPathfactory.newXPath();
    XPathExpression getVersion = xpath.compile(xpathQuery);
    String outputValue = (String) getVersion.evaluate(_xcodemlDoc, XPathConstants.STRING);
    if(outputValue.equals(attrValue)){
      return true;
    }
    return false;
  }
}

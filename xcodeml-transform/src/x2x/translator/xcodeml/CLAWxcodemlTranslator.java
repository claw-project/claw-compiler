package x2x.translator.xcodeml;


import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import java.io.File;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.*;

import java.util.*;


import exc.xcodeml.*;
import exc.object.Xcode;

import x2x.translator.pragma.CLAWpragma;

public class CLAWxcodemlTranslator {
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;
  private XcodeMLNameTable_F _xcodemlNameTable = null;
  private Document _xcodemlDoc = null;
  private ArrayList<CLAWloopFusion> _loopFusion = null;

  public CLAWxcodemlTranslator(String xcodemlInputFile, String xcodemlOutputFile){
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _xcodemlNameTable = new XcodeMLNameTable_F();
    _loopFusion = new ArrayList<CLAWloopFusion>();
  }

  private void readXcodeML(){
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

  private void ouputXcodeML() throws Exception {
    Transformer transformer = TransformerFactory.newInstance().newTransformer();
    transformer.setOutputProperty(OutputKeys.INDENT, "yes");
    DOMSource source = new DOMSource(_xcodemlDoc);
    if(_xcodemlOutputFile == null){
      // Output to console
      StreamResult console = new StreamResult(System.out);
    } else {
      // Output to file
      StreamResult console = new StreamResult(new File(_xcodemlOutputFile));
      transformer.transform(source, console);
    }
  }

  private boolean isXcodeMLvalid() throws Exception {
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

  public void analyze() throws Exception {
    readXcodeML();

    if(!isXcodeMLvalid()){
      System.err.println("XcodeML document is not valid");
      return;
    }

    NodeList nList = _xcodemlDoc.getElementsByTagName(_xcodemlNameTable.getName(Xcode.PRAGMA_LINE));
    for (int i = 0; i < nList.getLength(); i++) {
      Node pragmaNode = nList.item(i);
      if (pragmaNode.getNodeType() == Node.ELEMENT_NODE) {
        Element pragmaElement = (Element) pragmaNode;
        String fullPragmaText = pragmaElement.getTextContent();

        if(CLAWpragma.isValid(fullPragmaText)){

          CLAWpragma clawDirective = CLAWpragma.getDirective(fullPragmaText);
          if(clawDirective == CLAWpragma.LOOP_FUSION){


            // TODO find attached loop and raise error in case there is not
            Node pragmaSibling = pragmaNode.getNextSibling();
            while(pragmaSibling.getNodeType() != Node.ELEMENT_NODE){
              pragmaSibling = pragmaSibling.getNextSibling();
            }


            if (pragmaSibling.getNodeType() == Node.ELEMENT_NODE) {
              Element elementSibling = (Element) pragmaSibling;
              if(elementSibling.getTagName().equals("FdoStatement")){
                _loopFusion.add(new CLAWloopFusion(pragmaElement, elementSibling));
              }
            }

            //System.out.println("LOOP FUSION detected");
            //pragmaElement.getParentNode().removeChild(pragmaElement);

          }

        } else {
          System.out.println("INVALID PRAGMA: " + fullPragmaText);
        }



      }
    }

    // Analysis done, the transformation can be performed.
    _canTransform = true;
  }

  public void transform() {
    try {
      if(!_canTransform){
        ouputXcodeML();
        return;
      }

      // Do the transformation here

      // Apply loop-fusion transformation
      for(int i = 0; i < _loopFusion.size(); ++i){
        CLAWloopFusion base = _loopFusion.get(i);
        for(int j = i+1; j < _loopFusion.size(); ++j){
          CLAWloopFusion candidate = _loopFusion.get(j);
          if(candidate.isMerged()){
            continue;
          }
          if(base.canMergeWith(candidate)){
            base.merge(candidate);
          }
        }
      }


      ouputXcodeML();
    } catch (Exception ex) {
      // TODO handle exception
    }
  }

}

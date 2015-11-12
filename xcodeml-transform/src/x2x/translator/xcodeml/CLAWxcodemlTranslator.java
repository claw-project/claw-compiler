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
import xcodeml.util.XmOption;
import x2x.translator.pragma.CLAWpragma;

public class CLAWxcodemlTranslator {
  private String _xcodemlInputFile = null;
  private String _xcodemlOutputFile = null;
  private boolean _canTransform = false;
  private XcodeMLNameTable_F _xcodemlNameTable = null;
  private Document _xcodemlDoc = null;
  private ArrayList<CLAWloopFusion> _loopFusion = null;
  private ArrayList<CLAWloopInterchange> _loopInterchange = null;
  private ArrayList<CLAWextract> _loopExtract = null;

  public CLAWxcodemlTranslator(String xcodemlInputFile, String xcodemlOutputFile){
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _xcodemlNameTable = new XcodeMLNameTable_F();
    _loopFusion = new ArrayList<CLAWloopFusion>();
    _loopInterchange = new ArrayList<CLAWloopInterchange>();
    _loopExtract = new ArrayList<CLAWextract>();
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

        if(!CLAWpragma.startsWithClaw(fullPragmaText)){
          continue; // Not CLAW pragma, we do nothing
        }

        if(CLAWpragma.isValid(fullPragmaText)){
          CLAWpragma clawDirective = CLAWpragma.getDirective(fullPragmaText);

          // loop-fusion directives
          if(clawDirective == CLAWpragma.LOOP_FUSION){

            // TODO find attached loop and raise error in case there is not
            Node pragmaSibling = pragmaNode.getNextSibling();
            while(pragmaSibling.getNodeType() != Node.ELEMENT_NODE){
              pragmaSibling = pragmaSibling.getNextSibling();
            }

            Element elementSibling = (Element) pragmaSibling;
            if(elementSibling.getTagName().equals("FdoStatement")){
              _loopFusion.add(new CLAWloopFusion(pragmaElement, elementSibling));
            }


          // loop-interchange directives
          } else if(clawDirective == CLAWpragma.LOOP_INTERCHANGE){
            Element loop = findNextLoop(pragmaNode);
            if(loop == null){
              System.err.println("loop-interchange pragma is not followed by a loop");
            } else {
                _loopInterchange.add(new CLAWloopInterchange(pragmaElement, loop));
            }


          // loop-extract directives
          } else if(clawDirective == CLAWpragma.LOOP_EXTRACT){
            Element exprStmt = findNextExprStatement(pragmaNode);
            if(exprStmt == null){
              System.err.println("loop-extract pragma is not followed by a expression statment");
            } else {
              CLAWextract extraction = new CLAWextract(pragmaElement, exprStmt, _xcodemlDoc);
              if(extraction.analyze()){
                _loopExtract.add(extraction);
              }
            }
          }

        } else {
          System.out.println("INVALID PRAGMA: " + fullPragmaText);
          System.exit(1);
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

      if(XmOption.isDebugOutput()){
        System.out.println("transform loop-fusion: "+ _loopFusion.size());
        System.out.println("transform loop-interchange: "+ _loopInterchange.size());
        System.out.println("transform loop-extract: "+ _loopExtract.size());
      }

      // Apply loop-extract transformation
      for(int i = 0; i < _loopExtract.size(); ++i){
        CLAWextract extraction = _loopExtract.get(i);
        extraction.transform();
      }


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

      // Apply loop-interchange transformation
      for(int i = 0; i < _loopInterchange.size(); ++i){
        CLAWloopInterchange  loop = _loopInterchange.get(i);
        loop.transform();
      }


      ouputXcodeML();
    } catch (Exception ex) {
      // TODO handle exception
    }
  }



  private Element findNextLoop(Node from){
    Node nextNode = from.getNextSibling();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("FdoStatement")){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  private Element findNextExprStatement(Node from){
    Node nextNode = from.getNextSibling();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("exprStatement")){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

}

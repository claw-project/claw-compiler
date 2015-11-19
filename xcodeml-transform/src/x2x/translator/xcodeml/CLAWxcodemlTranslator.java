package x2x.translator.xcodeml;

import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.translation.*;

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

  private ArrayList<LoopFusion> _loopFusion = null;
  private ArrayList<LoopInterchange> _loopInterchange = null;
  private ArrayList<LoopExtraction> _loopExtract = null;
  private XcodemlDocument _program = null;

  public CLAWxcodemlTranslator(String xcodemlInputFile, String xcodemlOutputFile){
    _xcodemlInputFile = xcodemlInputFile;
    _xcodemlOutputFile = xcodemlOutputFile;
    _xcodemlNameTable = new XcodeMLNameTable_F();
    _loopFusion = new ArrayList<LoopFusion>();
    _loopInterchange = new ArrayList<LoopInterchange>();
    _loopExtract = new ArrayList<LoopExtraction>();
  }



  private void ouputXcodeML() {

    try {
      cleanEmptyTextNodes(_program.getDocument());
      Transformer transformer = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount",
                Integer.toString(2));
      DOMSource source = new DOMSource(_program.getDocument());
      if(_xcodemlOutputFile == null){
        // Output to console
        StreamResult console = new StreamResult(System.out);
      } else {
        // Output to file
        StreamResult console = new StreamResult(new File(_xcodemlOutputFile));
        transformer.transform(source, console);
      }
    } catch (TransformerConfigurationException ex){
      System.out.println("Cannot output file: " + ex.getMessage());
    } catch (TransformerException ex){
      System.out.println("Cannot output file: " + ex.getMessage());
    }

  }



  public void analyze() throws Exception {
    _program = new XcodemlDocument(_xcodemlInputFile);
    _program.readXcodeML();


    if(!_program.isXcodeMLvalid()){
      System.err.println("XcodeML document is not valid");
      return;
    }

    // Read information from the type table
    _program.readTypeTable();

    NodeList nList = _program.getDocument()
      .getElementsByTagName(_xcodemlNameTable.getName(Xcode.PRAGMA_LINE));

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
              _loopFusion.add(new LoopFusion(pragmaElement, elementSibling));
            }


          // loop-interchange directives
          } else if(clawDirective == CLAWpragma.LOOP_INTERCHANGE){
            Element loop = XelementHelper.findNextLoop(pragmaNode);
            if(loop == null){
              System.err.println("loop-interchange pragma is not followed by a loop");
            } else {
                _loopInterchange.add(new LoopInterchange(pragmaElement, loop));
            }


          // loop-extract directives
          } else if(clawDirective == CLAWpragma.LOOP_EXTRACT){
            Element exprStmt = XelementHelper.findNextExprStatement(pragmaNode);
            if(exprStmt == null){
              System.err.println("loop-extract pragma is not followed by a expression statment");
            } else {
              LoopExtraction extraction = new LoopExtraction(pragmaElement, exprStmt, _program);
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
        LoopExtraction extraction = _loopExtract.get(i);
        extraction.transform(_program);
      }


      // Apply loop-fusion transformation
      for(int i = 0; i < _loopFusion.size(); ++i){
        LoopFusion base = _loopFusion.get(i);
        for(int j = i+1; j < _loopFusion.size(); ++j){
          LoopFusion candidate = _loopFusion.get(j);
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
        LoopInterchange  loop = _loopInterchange.get(i);
        loop.transform();
      }


      ouputXcodeML();
    } catch (Exception ex) {
      // TODO handle exception
      System.out.println("Transformation exception: ");
      ex.printStackTrace();
    }
  }




  /**
   * Removes text nodes that only contains whitespace. The conditions for
   * removing text nodes, besides only containing whitespace, are: If the
   * parent node has at least one child of any of the following types, all
   * whitespace-only text-node children will be removed: - ELEMENT child -
   * CDATA child - COMMENT child
   *
   * The purpose of this is to make the format() method (that use a
   * Transformer for formatting) more consistent regarding indenting and line
   * breaks.
   */
  private static void cleanEmptyTextNodes(Node parentNode) {
    boolean removeEmptyTextNodes = false;
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      removeEmptyTextNodes |= checkNodeTypes(childNode);
      childNode = childNode.getNextSibling();
    }

    if (removeEmptyTextNodes) {
      removeEmptyTextNodes(parentNode);
    }
  }

  private static void removeEmptyTextNodes(Node parentNode) {
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      // grab the "nextSibling" before the child node is removed
      Node nextChild = childNode.getNextSibling();

      short nodeType = childNode.getNodeType();
      if (nodeType == Node.TEXT_NODE) {
        boolean containsOnlyWhitespace = childNode.getNodeValue()
          .trim().isEmpty();
        if (containsOnlyWhitespace) {
          parentNode.removeChild(childNode);
        }
      }
      childNode = nextChild;
    }
  }

  private static boolean checkNodeTypes(Node childNode) {
    short nodeType = childNode.getNodeType();

    if (nodeType == Node.ELEMENT_NODE) {
      cleanEmptyTextNodes(childNode); // recurse into subtree
    }

    if (nodeType == Node.ELEMENT_NODE
        || nodeType == Node.CDATA_SECTION_NODE
        || nodeType == Node.COMMENT_NODE) {
      return true;
    } else {
      return false;
    }
  }

}

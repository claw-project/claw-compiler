package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Document;

public class CLAWextract {

  protected Element _pragmaElement = null;
  protected Element _exprStmtElement = null;
  protected Element _fncCallStmt = null;
  protected Document _xcodemlDoc = null;

  private CLAWfctCall _fctCall = null;

  protected CLAWindexRange _extractedLoopRange = null;

  public CLAWextract(Element pragma, Element exprStmt, Document xcodemlDoc){
    _pragmaElement = pragma;
    _exprStmtElement = exprStmt;
    _xcodemlDoc = xcodemlDoc;
  }

  private void extractMapping(){


  }

  private void extractRange(){

  }

  private void analyzeMapping(){

  }

  public boolean analyze(){
    // Find function CALL
    Element fctCallElement = findFctCall();
    if(fctCallElement == null){
      System.err.println("No function call detected after loop-extract");
      System.exit(1);
    }

    _fctCall = new CLAWfctCall(fctCallElement);
    System.out.println(_fctCall.getFctName());

    // Find function declaration
    Element fctDefinitionElement = CLAWelementHelper.findFunctionDefinition(
      _xcodemlDoc, _fctCall);

    if(fctDefinitionElement == null){
      System.err.println("Could not locate the function definition for: "
        + _fctCall.getFctName());
    }


    // Find loop in function

    // Compare range

    // Duplicate function without the loop



    return true;
  }

  public void transform(){
    // Do smth here

  }

  private Element findFctCall(){
    if(_exprStmtElement == null){
      return null;
    }

    NodeList nodeList = _exprStmtElement.getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("functionCall")){
          return element;
        }
      }
    }
    return null;
  }

}

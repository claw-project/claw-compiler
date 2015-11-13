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
  protected XcodemlDocument _xcodeml = null;

  private CLAWfctCall _fctCall = null;
  private CLAWloop _extractedLoop = null;

  public CLAWextract(Element pragma, Element exprStmt, XcodemlDocument xcodemlDoc){
    _pragmaElement = pragma;
    _exprStmtElement = exprStmt;
    _xcodeml = xcodemlDoc;
  }

  private void extractMapping(){

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
    CLAWfctDef fctDef = CLAWelementHelper.findFunctionDefinition(
      _xcodeml.getDocument(), _fctCall);

    if(fctDef == null){
      System.err.println("Could not locate the function definition for: "
        + _fctCall.getFctName());
      System.exit(1);
    }

    // Find loop in function
    _extractedLoop = CLAWelementHelper.findLoop(fctDef);
    if(_extractedLoop == null){
      System.err.println("Could not locate inner loop in subroutine "
        + fctDef.getFctName());
      System.exit(1);
    }

    System.out.println("loopRange: "
      + _extractedLoop.getIterationRange().toString());

    return true;
  }

  public void transform(XcodemlDocument xcodeml){
    // Duplicate function definition

    // Remove loop from body


    // Wrap function call with loop
    wrapCallWithLoop(xcodeml, _extractedLoop.getIterationRange());

    // Adapt function call parameters
  }

  private void wrapCallWithLoop(XcodemlDocument xcodeml,
    CLAWloopIterationRange iterationRange)
  {
    Document document = xcodeml.getDocument();

    // Create the loop before the call
    Element loop = document.createElement(XelementName.DO_STMT);
    _pragmaElement.getParentNode().insertBefore(loop, _pragmaElement.getNextSibling());

    loop.appendChild(iterationRange.getInductionVar().clone());
    loop.appendChild(iterationRange.getIndexRange().clone());

    Element body = document.createElement(XelementName.BODY);

    loop.appendChild(body);

    // Move the call into the loop body
    body.appendChild(_fctCall.getFctElement().getParentNode());

    // TODO create needed variable
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

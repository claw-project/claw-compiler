package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWmapping;
import x2x.translator.xcodeml.xelement.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.*;

import xcodeml.util.XmOption;


public class LoopExtraction implements Transformation<LoopExtraction> {

  protected Element _pragmaElement = null;
  protected Element _exprStmtElement = null;
  protected Element _fncCallStmt = null;

  private ArrayList<CLAWmapping> _mappings = null;
  private XfctCall _fctCall = null;
  private XfctDef _fctDef = null; // Fct holding the fct call
  private XfctDef _extractedFctDef = null;
  private XdoStatement _extractedLoop = null;

  private XfctDef _copiedFctDef = null;

  public LoopExtraction(Element pragma, Element exprStmt) {
    _pragmaElement = pragma;
    _exprStmtElement = exprStmt;
    _mappings = new ArrayList<CLAWmapping>();
    extractMappingInformation();
  }

  private void extractRangeInformation(){

  }

  private void extractMappingInformation(){
    List<String> allMappings = new ArrayList<String>();
    Matcher m = Pattern.compile("map\\(([^:]*:.)\\)")
     .matcher(_pragmaElement.getTextContent());
    while (m.find()) {
      allMappings.add(m.group(1));
    }

    for(String mappingClause : allMappings){
      System.out.println("MAPPING " + mappingClause);
      CLAWmapping mapping = new CLAWmapping(mappingClause);
      _mappings.add(mapping);
    }
  }

  private boolean checkMappingInformation(){


    return true; //TODO
  }

  public boolean analyze(XcodeProg xcodeml){
    // Find function CALL
    Element fctCallElement = findFctCall();
    if(fctCallElement == null){
      System.err.println("No function call detected after loop-extract");
      System.exit(1);
    }

    _fctCall = new XfctCall(fctCallElement);

    _fctDef = XelementHelper.findParentFctDef(_fctCall.getBaseElement());
    if(_fctDef == null){
      System.err.println("No function around the fct call");
      System.exit(1);
    }

    // Find function declaration
    _extractedFctDef = XelementHelper.findFunctionDefinition(
      xcodeml.getDocument(), _fctCall);

    if(_extractedFctDef == null){
      System.err.println("Could not locate the function definition for: "
        + _fctCall.getFctName());
      System.exit(1);
    }

    // Find loop in function
    _extractedLoop = XelementHelper.findLoop(_extractedFctDef);
    if(_extractedLoop == null){
      System.err.println("Could not locate inner loop in subroutine "
        + _extractedFctDef.getFctName());
      System.exit(1);
    }

    if(!checkMappingInformation()){
      System.err.println("Mapping information are not usable"
        + _extractedFctDef.getFctName());
      System.exit(1);
    }

    return true;
  }

  public void transform(XcodeProg xcodeml, LoopExtraction other){

    /*
     * DUPLICATE THE FUNCTION
     */

    // Duplicate function definition
    Node cloned = _extractedFctDef.clone();
    XfctDef clonedFctDef = new XfctDef((Element)cloned);
    String newFctTypeHash = xcodeml.getTypeTable().generateFctTypeHash();
    // TODO new name should be generated and unique
    String newFctName = clonedFctDef.getFctName() + "_extracted";
    clonedFctDef.updateName(newFctName);
    clonedFctDef.updateType(newFctTypeHash);
    // Update the symbol table in the fct definition
    Xid fctId = clonedFctDef.getSymbolTable().
      get(_extractedFctDef.getFctName());
    fctId.setType(newFctTypeHash);
    fctId.setName(newFctName);

    // Get the fctType in typeTable
    XfctType fctType = (XfctType)xcodeml
      .getTypeTable().get(_extractedFctDef.getFctType());
    XfctType newFctType = fctType.cloneObject();
    newFctType.setType(newFctTypeHash);
    xcodeml.getTypeTable().add(newFctType);

    // Get the id from the global symbols table
    Xid globalFctId = xcodeml.getGlobalSymbolsTable()
      .get(_extractedFctDef.getFctName());
    Xid newFctId = globalFctId.cloneObject();
    newFctId.setType(newFctTypeHash);
    newFctId.setName(newFctName);
    xcodeml.getGlobalSymbolsTable().add(newFctId);



    // Find the loop that will be extracted
    XdoStatement loopInClonedFct = XelementHelper.findLoop(clonedFctDef);



    if(XmOption.isDebugOutput()){
      System.out.println("loop-extract transformation");
      System.out.println("  created subroutine: " + clonedFctDef.getFctName());
    }

    XelementHelper.insertAfter(_extractedFctDef.getBaseElement(), cloned);

    /*
     * DEMOTE ARRAY REFERENCES IN THE BODY OF THE FUNCTION
     */



    /*
     * REMOVE BODY FROM THE LOOP AND DELETE THE LOOP
     */

    // 1. append body into fct body after loop
    XelementHelper.extractBody(loopInClonedFct);
    // 2. delete loop
    XelementHelper.delete(loopInClonedFct.getBaseElement());


    /*
     * ADAPT FUNCTION CALL
     */

    // Wrap function call with loop
    wrapCallWithLoop(xcodeml, _extractedLoop.getIterationRange());

    if(XmOption.isDebugOutput()){
      System.out.println("  call wrapped with loop: " + _fctCall.getFctName()
       + " --> " + clonedFctDef.getFctName());
    }

    // Change called fct name
    _fctCall.setName(newFctName);
    _fctCall.setType(newFctTypeHash);

    // Adapt function call parameters
    XargumentsTable args = _fctCall.getArgumentsTable();
    for(CLAWmapping mapping : _mappings){
      System.out.println("Apply mapping (" + mapping.getMappedDimensions() + ") ");

      for(String var : mapping.getMappedVariables()){

        System.out.println("  Var: " + var);
        Xvar argument = args.findArgument(var);
        if(argument != null){
          System.out.println("  arg found: " + argument.getType());
          XbasicType type = (XbasicType)xcodeml.getTypeTable().get(argument.getType());

          System.out.println("  ref: " + type.getRef());
          System.out.println("  dimensions: " + type.getDimensions());

          // Demotion cannot be applied as type dimension is smaller
          if(type.getDimensions() < mapping.getMappedDimensions()){
            // TODO problem !!!! demotion to big
          }

          //
          for(String mappingVar : mapping.getMappingVariables()){

          }


        }
      }
    }

    /* TODO
     * Demotion possibility
     * 1. Var --> ArrayRef
     * 2. ArrayRef (n arrayIndex) --> ArrayRef (n+m arrayIndex)
     *
     * Var --> ArrayRef transformation
     * 1. Check that the variable used as array index exists in the current
     *    scope (XdeclTable). If so, get its type value. Create a Var element
     *    for the arrayIndex. Create the arrayIndex element with Var as child.
     *
     * 2. Get the reference type of the base variable.
     *    2.1 Create the varRef element with the type of base variable
     *    2.2 insert clone of base variable in varRef
     * 3. Create arrayRef element with varRef + arrayIndex
     */





  }

  private void wrapCallWithLoop(XcodeProg xcodeml,
    XloopIterationRange iterationRange)
  {
    // TODO have single method to create a loop from iterationRange
    Document document = xcodeml.getDocument();

    // Create the loop before the call
    Element loop = document.createElement(XelementName.DO_STMT);
    _pragmaElement.getParentNode().insertBefore(loop, _pragmaElement.getNextSibling());

    loop.appendChild(iterationRange.getInductionVar().clone());
    loop.appendChild(iterationRange.getIndexRange().clone());

    Element body = document.createElement(XelementName.BODY);

    loop.appendChild(body);

    // Move the call into the loop body
    body.appendChild(_fctCall.getBaseElement().getParentNode());


    insertDeclaration(iterationRange.getInductionVar().getValue());
    if(iterationRange.getIndexRange().getLowerBound().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getLowerBound().getValue());
    }
    if(iterationRange.getIndexRange().getUpperBound().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getUpperBound().getValue());
    }
    if(iterationRange.getIndexRange().getStep().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getStep().getValue());
    }
  }

  private void insertDeclaration(String id){
    Xid inductionVarId = _fctDef.getSymbolTable().get(id);
    if(inductionVarId == null){
      Xid copyId = _extractedFctDef.getSymbolTable().get(id);
      _fctDef.getSymbolTable().add(copyId);
    }

    XvarDecl inductionVarDecl = _fctDef.getDeclarationTable().get(id);
    if(inductionVarDecl == null){
      XvarDecl copyDecl = _extractedFctDef.getDeclarationTable().get(id);
      _fctDef.getDeclarationTable().add(copyDecl);
    }
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

  public boolean isTransformed() {
    return true; // TODO
  }

  public boolean canBeTransformedWith(LoopExtraction other){
    return true; // Always true as independent transformation
  }

}

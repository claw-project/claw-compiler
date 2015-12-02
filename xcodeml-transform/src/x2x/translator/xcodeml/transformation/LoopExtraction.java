package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWmapping;
import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformer.Transformer;

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

  private Xpragma _pragma = null;
  private XexprStatement _exprStmt = null;

  private ArrayList<CLAWmapping> _mappings = null;
  private XfctCall _fctCall = null;
  private XfctDef _fctDef = null; // Fct holding the fct call
  private XfctDef _extractedFctDef = null;
  private XdoStatement _extractedLoop = null;
  private XfctDef _copiedFctDef = null;

  // Fusion and fusion option
  private boolean _hasFusion = false;
  private String _fusionGroupLabel = "";


  public LoopExtraction(Xpragma pragma) {
    _pragma = pragma;
    _mappings = new ArrayList<CLAWmapping>();
    extractMappingInformation();
    extractFusionInformation();
  }

  private void extractRangeInformation(){

  }

  private void extractMappingInformation(){
    List<String> allMappings = new ArrayList<String>();
    Matcher m = Pattern.compile("map\\(([^:]*:.)\\)")
     .matcher(_pragma.getData());
    while (m.find()) {
      allMappings.add(m.group(1));
    }

    for(String mappingClause : allMappings){
      System.out.println("MAPPING " + mappingClause);
      CLAWmapping mapping = new CLAWmapping(mappingClause);
      _mappings.add(mapping);
    }
  }

  private void extractFusionInformation(){
    if(_pragma.getData().contains(" fusion ")){
      _hasFusion = true;
    }

    // TODO centralize in the pragma class
    Matcher m = Pattern.compile("fusion\\s+group\\((.*)\\)")
     .matcher(_pragma.getData());
    while(m.find()){
      _fusionGroupLabel = m.group(1);

    }
  }

  private boolean checkMappingInformation(){
    // TODO Mapped variable should appear on one time
    // TODO Mapped variable should be declared in the fct definition
    // declarations or in the global declarations table

    // TODO Merge mapping if they have the exact same mapping vars to reduce the
    // number of iteration for the demotions

    return true; // TODO
  }

  public boolean analyze(XcodeProg xcodeml, Transformer transformer){
    Element exprElement = XelementHelper
      .findNextExprStatement(_pragma.getBaseElement());

    if(exprElement == null){
      // TODO give the reason and stops analysis
      return false;
    }
    _exprStmt = new XexprStatement(exprElement);

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

  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopExtraction other)
  {

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

    // If the fct is define in the global symbol table, duplicate it
    if(globalFctId != null){
      Xid newFctId = globalFctId.cloneObject();
      newFctId.setType(newFctTypeHash);
      newFctId.setName(newFctName);
      xcodeml.getGlobalSymbolsTable().add(newFctId);
    }




    // Find the loop that will be extracted
    XdoStatement loopInClonedFct = XelementHelper.findLoop(clonedFctDef);



    if(XmOption.isDebugOutput()){
      System.out.println("loop-extract transformation");
      System.out.println("  created subroutine: " + clonedFctDef.getFctName());
    }

    XelementHelper.insertAfter(_extractedFctDef.getBaseElement(), cloned);

    /*
     *
     */



    /*
     * REMOVE BODY FROM THE LOOP AND DELETE THE LOOP
     */

    // 1. append body into fct body after loop
    XelementHelper.extractBody(loopInClonedFct);
    // 2. delete loop
    loopInClonedFct.delete();


    /*
     * ADAPT FUNCTION CALL AND DEMOTE ARRAY REFERENCES IN THE BODY
     * OF THE FUNCTION
     */

    // Wrap function call with loop
    XdoStatement extractedLoop = wrapCallWithLoop(xcodeml,
      _extractedLoop.getIterationRange());

    if(XmOption.isDebugOutput()){
      System.out.println("  call wrapped with loop: " + _fctCall.getFctName()
       + " --> " + clonedFctDef.getFctName());
    }

    // Change called fct name
    _fctCall.setName(newFctName);
    _fctCall.setType(newFctTypeHash);

    // Adapt function call parameters and function declaration
    XargumentsTable args = _fctCall.getArgumentsTable();
    XdeclTable fctDeclarations = clonedFctDef.getDeclarationTable();
    XsymbolTable fctSymbols = clonedFctDef.getSymbolTable();

    for(CLAWmapping mapping : _mappings){
      System.out.println("Apply mapping (" + mapping.getMappedDimensions() + ") ");

      for(String var : mapping.getMappedVariables()){

        System.out.println("  Var: " + var);
        XbaseElement argument = args.findArgument(var); // TODO return a dedictaed type
        if(argument != null){

          /* Case 1: Var --> ArrayRef
           * Var --> ArrayRef transformation
           * 1. Check that the variable used as array index exists in the
           *    current scope (XdeclTable). If so, get its type value. Create a
           *    Var element for the arrayIndex. Create the arrayIndex element
           *    with Var as child.
           *
           * 2. Get the reference type of the base variable.
           *    2.1 Create the varRef element with the type of base variable
           *    2.2 insert clone of base variable in varRef
           * 3. Create arrayRef element with varRef + arrayIndex
           */
          if(argument instanceof Xvar){
            Xvar varArg = (Xvar)argument;
            System.out.println("  arg found: " + varArg.getType());
            XbasicType type =
              (XbasicType)xcodeml.getTypeTable().get(varArg.getType());

            System.out.println("  ref: " + type.getRef());
            System.out.println("  dimensions: " + type.getDimensions());

            // Demotion cannot be applied as type dimension is smaller
            if(type.getDimensions() < mapping.getMappedDimensions()){
              // TODO problem !!!! demotion to big
            }

            XarrayRef newArg = XarrayRef.createEmpty(xcodeml, type.getRef());
            XvarRef varRef = XvarRef.createEmpty(xcodeml, varArg.getType());
            varRef.append(varArg, true);
            newArg.append(varRef);

            XarrayIndex arrayIndex = XarrayIndex.createEmpty(xcodeml);


            //  create arrayIndex

            for(String mappingVar : mapping.getMappingVariables()){
              // Find the mapping var in the local table (fct scope)
              XvarDecl mappingVarDecl =
                _fctDef.getDeclarationTable().get(mappingVar);

              // Add to arrayIndex
              Xvar newMappingVar = Xvar.createEmpty(xcodeml, Xscope.LOCAL.toString());
              newMappingVar.setType(mappingVarDecl.getName().getType());
              newMappingVar.setValue(mappingVarDecl.getName().getValue());
              arrayIndex.append(newMappingVar);
            }
            newArg.append(arrayIndex);
            args.replace(varArg, newArg);
          }
          // Case 2: ArrayRef (n arrayIndex) --> ArrayRef (n+m arrayIndex)
          else if (argument instanceof XarrayRef){
            // TODO
          }


          // Change variable declaration in extracted fct

          XvarDecl varDecl = fctDeclarations.get(var);
          Xid id = fctSymbols.get(var);
          XbasicType varDeclType =
            (XbasicType)xcodeml.getTypeTable().get(varDecl.getName().getType());

          // Case 1: variable is demoted to scalar then take the ref type
          if(varDeclType.getDimensions() == mapping.getMappedDimensions()){
            XvarDecl newVarDecl = XvarDecl.createEmpty(xcodeml, var, varDeclType.getRef());
            fctDeclarations.replace(newVarDecl);
            id.setType(varDeclType.getRef());
          }

          // Case 2: variable is not totally demoted then create new type
          // TODO


          // Adapt array reference in extracted fct body element
          ArrayList<XarrayRef> arrayReferences =
            XelementHelper.getAllArrayReferences(clonedFctDef.getBody());
          for(String mappingVar : mapping.getMappingVariables()){
            for(XarrayRef ref : arrayReferences){
              for(XbaseElement e : ref.getInnerElements()){
                if(e instanceof XarrayIndex){
                  XarrayIndex arrayIndex = (XarrayIndex)e;

                  if(arrayIndex.getExprModel() != null && arrayIndex.getExprModel().isVar()){
                    if(arrayIndex.getExprModel().getVar().getValue().equals(mappingVar)){
                      XelementHelper.insertAfter(ref.getBaseElement(), ref.getVarRef().clone());
                      ref.delete();
                    }
                  }
                }
              }
            } // Loop over arrayReferences
          } // Loop over mapping variables

          // End of array references adaptation block TODO refactor the code to
          // be more readable and segment it in smaller methods


        } // If arg null TODO invert if to reduce nesting
      } // Loop mapped variables
    } // Loop over mapping clauses



    // Transformation is done. Add additional transfomation here
    if(_hasFusion){

      LoopFusion fusion = new LoopFusion(extractedLoop, _fusionGroupLabel);
      transformer.addTransformation(fusion);

      if(XmOption.isDebugOutput()){
        System.out.println("Loop fusion added: " + _fusionGroupLabel);
      }

    }

  }

  private XdoStatement wrapCallWithLoop(XcodeProg xcodeml,
    XloopIterationRange iterationRange)
  {
    // TODO have single method to create a loop from iterationRange
    Document document = xcodeml.getDocument();

    // Create the loop before the call TODO priority move
    Element loop = document.createElement(XelementName.DO_STMT);
    _pragma.getBaseElement().getParentNode().insertBefore(loop,
      _pragma.getBaseElement().getNextSibling());

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

    return new XdoStatement(loop);
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
    if(_exprStmt == null){
      return null;
    }

    NodeList nodeList = _exprStmt.getBaseElement().getChildNodes();
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

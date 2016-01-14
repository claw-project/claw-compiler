package cx2x.translator.transformation;

import cx2x.translator.pragma.CLAWmapping;
import cx2x.translator.exception.*;
import cx2x.xcodeml.xelement.*;
import cx2x.translator.transformer.Transformer;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.*;

import xcodeml.util.XmOption;


public class LoopExtraction extends Transformation<LoopExtraction> {

  private XexprStatement _exprStmt = null;

  private ArrayList<CLAWmapping> _mappings = null;
  private XfctCall _fctCall = null;
  private XfctDef _fctDef = null; // Fct holding the fct call
  private XfctDef _fctDefToExtract = null;
  private XdoStatement _extractedLoop = null;
  private XfctDef _copiedFctDef = null;

  // Fusion and fusion option
  private boolean _hasFusion = false;
  private String _fusionGroupLabel = "";


  public LoopExtraction(Xpragma pragma) {
    super(pragma);
    _mappings = new ArrayList<CLAWmapping>();
    extractMappingInformation();
    extractFusionInformation();
  }

  private void extractRangeInformation(){

  }

  private void extractMappingInformation(){
    List<String> allMappings = new ArrayList<String>();
    // TODO move regex somewhere centralized
    Matcher m = Pattern.compile("map\\(([^:]*:[^)]*)\\)")
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
    _exprStmt = XelementHelper.findNextExprStatement(_pragma);
    if(_exprStmt == null){
      xcodeml.addError("No function call detected after loop-extract",
        _pragma.getLine());
      return false;
    }

    // Find function CALL
    _fctCall = XelementHelper.findFctCall(_exprStmt);
    if(_fctCall == null){
      xcodeml.addError("No function call detected after loop-extract",
        _pragma.getLine());
      return false;
    }

    _fctDef = XelementHelper.findParentFctDef(_fctCall);
    if(_fctDef == null){
      xcodeml.addError("No function around the fct call",
        _pragma.getLine());
      return false;
    }

    // Find function declaration
    _fctDefToExtract = XelementHelper.findFunctionDefinition(xcodeml, _fctCall);

    if(_fctDefToExtract == null){
      System.err.println("Could not locate the function definition for: "
        + _fctCall.getFctName());
      System.exit(1);
    }

    // Find loop in function
    // TODO find any loops ? or subroutine must have only one loop ? ...
    _extractedLoop = XelementHelper.findLoop(_fctDefToExtract, true);
    if(_extractedLoop == null){
      System.err.println("Could not locate inner loop in subroutine "
        + _fctDefToExtract.getFctName());
      System.exit(1);
    }

    if(!checkMappingInformation()){
      System.err.println("Mapping information are not usable"
        + _fctDefToExtract.getFctName());
      System.exit(1);
    }

    return true;
  }

  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopExtraction other) throws IllegalTransformationException
  {

    /*
     * DUPLICATE THE FUNCTION
     */

    // Duplicate function definition
    XfctDef clonedFctDef = _fctDefToExtract.cloneObject();
    String newFctTypeHash = xcodeml.getTypeTable().generateFctTypeHash();
    // TODO new name should be generated and unique
    String newFctName = clonedFctDef.getFctName() + "_extracted";
    clonedFctDef.updateName(newFctName);
    clonedFctDef.updateType(newFctTypeHash);
    // Update the symbol table in the fct definition
    Xid fctId = clonedFctDef.getSymbolTable().
      get(_fctDefToExtract.getFctName());
    fctId.setType(newFctTypeHash);
    fctId.setName(newFctName);

    // Get the fctType in typeTable
    XfctType fctType = (XfctType)xcodeml
      .getTypeTable().get(_fctDefToExtract.getFctType());
    XfctType newFctType = fctType.cloneObject();
    newFctType.setType(newFctTypeHash);
    xcodeml.getTypeTable().add(newFctType);

    // Get the id from the global symbols table
    Xid globalFctId = xcodeml.getGlobalSymbolsTable()
      .get(_fctDefToExtract.getFctName());

    // If the fct is define in the global symbol table, duplicate it
    if(globalFctId != null){
      Xid newFctId = globalFctId.cloneObject();
      newFctId.setType(newFctTypeHash);
      newFctId.setName(newFctName);
      xcodeml.getGlobalSymbolsTable().add(newFctId);
    }

    // Insert the duplicated function declaration
    XelementHelper.insertAfter(_fctDefToExtract, clonedFctDef);

    // Find the loop that will be extracted
    // TODO find any loops ?
    XdoStatement loopInClonedFct = XelementHelper.findLoop(clonedFctDef, true);


    if(XmOption.isDebugOutput()){
      System.out.println("loop-extract transformation: " + _pragma.getData());
      System.out.println("  created subroutine: " + clonedFctDef.getFctName());
    }



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

    if(XmOption.isDebugOutput()){
      System.out.println("  Start to apply mapping: " + _mappings.size());
    }

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

            //  create arrayIndex
            for(String mappingVar : mapping.getMappingVariables()){
              XarrayIndex arrayIndex = XarrayIndex.createEmpty(xcodeml);
              // Find the mapping var in the local table (fct scope)
              XvarDecl mappingVarDecl =
                _fctDef.getDeclarationTable().get(mappingVar);

              // Add to arrayIndex
              Xvar newMappingVar = Xvar.createEmpty(xcodeml, Xscope.LOCAL.toString());
              newMappingVar.setType(mappingVarDecl.getName().getType());
              newMappingVar.setValue(mappingVarDecl.getName().getValue());
              arrayIndex.append(newMappingVar);
              newArg.append(arrayIndex);
            }

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
          List<XarrayRef> arrayReferences =
            XelementHelper.getAllArrayReferences(clonedFctDef.getBody());
          for(String mappingVar : mapping.getMappingVariables()){
            for(XarrayRef ref : arrayReferences){
              boolean changeRef = true;
              for(XbaseElement e : ref.getInnerElements()){
                if(e instanceof XarrayIndex){
                  XarrayIndex arrayIndex = (XarrayIndex)e;
                  if(arrayIndex.getExprModel() != null && arrayIndex.getExprModel().isVar()){
                    if(!arrayIndex.getExprModel().getVar().getValue().equals(mappingVar)){
                      changeRef = false;
                    }
                  }
                }
              }
              if(changeRef){
                // TODO Var ref should be extracted only if the reference can be
                // totally demoted
                XelementHelper.insertBefore(ref, ref.getVarRef().cloneObject());
                ref.delete();
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

      LoopFusion fusion = new LoopFusion(extractedLoop, _fusionGroupLabel,
        _pragma.getLine());
      transformer.addTransformation(fusion);

      if(XmOption.isDebugOutput()){
        System.out.println("Loop fusion added: " + _fusionGroupLabel);
      }

    }
    this.transformed();
  }

  private XdoStatement wrapCallWithLoop(XcodeProg xcodeml,
    XloopIterationRange iterationRange)
  {
    // Create a new empty loop
    XdoStatement loop = XdoStatement.createEmpty(xcodeml, iterationRange);

    // Insert the new empty loop just after the pragma
    XelementHelper.insertAfter(_pragma, loop);

    // Move the call into the loop body
    XelementHelper.insertFctCallIntoLoop(loop, _fctCall);

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

    return loop;
  }

  private void insertDeclaration(String id){
    Xid inductionVarId = _fctDef.getSymbolTable().get(id);
    if(inductionVarId == null){
      Xid copyId = _fctDefToExtract.getSymbolTable().get(id);
      _fctDef.getSymbolTable().add(copyId);
    }

    XvarDecl inductionVarDecl = _fctDef.getDeclarationTable().get(id);
    if(inductionVarDecl == null){
      XvarDecl copyDecl = _fctDefToExtract.getDeclarationTable().get(id);
      _fctDef.getDeclarationTable().add(copyDecl);
    }
  }


  public boolean canBeTransformedWith(LoopExtraction other){
    return true; // Always true as independent transformation
  }

}

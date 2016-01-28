/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

// Cx2x import
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.exception.*;
import cx2x.translator.pragma.*;

// OMNI import
import xcodeml.util.XmOption;

// Java import
import java.util.*;

/**
 * A LoopExtraction transformation is an independent transformation. The
 * transformation consists of locating a loop in a function call and extract it.
 * This loop is then wrapped around the function call and the parameters are
 * demoted accordingly to the mapping options.
 *
 * @author clementval
 */

public class LoopExtraction extends Transformation<LoopExtraction> {

  private List<ClawMapping> _mappings = null;
  private Map<String, ClawMapping> _mappingMap = null;
  private XfctCall _fctCall = null;
  private XfctDef _fctDef = null; // Fct holding the fct call
  private XfctDef _fctDefToExtract = null;
  private XdoStatement _extractedLoop = null;

  // Fusion and fusion option
  private boolean _hasFusion = false;
  private String _fusionGroupLabel = "";

  // Acc options
  private boolean _hasParallelOption = false;
  private String _accAdditionalOption = null;

  /**
   * Constructs a new LoopExtraction triggered from a specific pragma.
   * @param pragma  The pragma that triggered the loop extraction
   *                transformation.
   */
  public LoopExtraction(Xpragma pragma) throws IllegalDirectiveException {
    super(pragma);
    _mappings = new ArrayList<>();
    _mappingMap = new Hashtable<>();
    extractRangeInformation();
    extractFusionInformation();

    _hasParallelOption = ClawPragma.hasParallelOption(pragma.getData());
    _accAdditionalOption = ClawPragma.getAccOptionValue(pragma.getData());

    try {
      extractMappingInformation();
    } catch (IllegalDirectiveException ide){
      ide.setDirectiveLine(_pragma.getLine());
      throw ide;
    }
  }

  private void extractRangeInformation(){
    // TODO
  }

  /**
   * Extract all mapping information from the pragma data. Each
   * map(<mapped>:<mapping>) produces a ClawMapping object.
   */
  private void extractMappingInformation() throws IllegalDirectiveException {
    _mappings = ClawPragma.extractMappingInformation(_pragma.getData());
    for(ClawMapping m : _mappings){
      for(String mappedVar : m.getMappedVariables()){
        if(_mappingMap.containsKey(mappedVar)){
          throw new IllegalDirectiveException(_pragma.getData(), mappedVar +
              " appears more than once in the mapping");
        } else {
          _mappingMap.put(mappedVar, m);
        }
      }
    }
  }

  /**
   * Extract optional fusion information. The extract loop might later be merged
   * with a fusion group. This method extract whether there is a fusion to
   * perform and its optional fusion group option.
   */
  private void extractFusionInformation(){
    _hasFusion = ClawPragma.hasFusionOption(_pragma.getData());
    _fusionGroupLabel = ClawPragma.getExtractFusionOption(_pragma.getData());
  }

  /**
   * Check whether the provided mapping information are correct or not. A
   * mapped variable should only appear once. Mapped variable must be parameters
   * in the function definition.
   * Mapping using the same mapping variables are merged together.
   * @return True if all the conditions are respected. False otherwise.
   */
  private boolean checkMappingInformation(XcodeProg xcodeml){
    for(Map.Entry<String, ClawMapping> map : _mappingMap.entrySet()){
      if(_fctCall.getArgumentsTable().findArgument(map.getKey()) == null){
        xcodeml.addError("Mapped variable " + map.getKey() +
            " not found in function call arguments", _pragma.getLine());
        return false;
      }
    }

    // TODO Merge mapping if they have the exact same mapping vars to reduce the
    // number of iteration for the demotions

    return true; // TODO
  }

  /**
   *
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if the transformation analysis succeeded. False otherwise.
   */
  public boolean analyze(XcodeProg xcodeml, Transformer transformer){
    XexprStatement _exprStmt = XelementHelper.findNextExprStatement(_pragma);
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

    if(!checkMappingInformation(xcodeml)){
      return false;
    }

    return true;
  }

  /**
   * Apply the transformation. A loop extraction is applied in the following
   * steps:
   *  1) Duplicate the function targeted by the transformation
   *  2) Extract the loop body in the duplicated function and remove the loop.
   *  3) Adapt function call and demote array references in the duplicated
   *     function body.
   *  4) Optional: Add a LoopFusion transformation to the transformaions' queue.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @param other       Only for dependent transformation. The other
   *                    transformation part of the transformation.
   * @throws IllegalTransformationException
   */
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
    String newFctName = clonedFctDef.getFctName() + "_extracted" +
        transformer.getNextTransformationCounter();
    clonedFctDef.updateName(newFctName);
    clonedFctDef.updateType(newFctTypeHash);
    // Update the symbol table in the fct definition
    Xid fctId = clonedFctDef.getSymbolTable()
        .get(_fctDefToExtract.getFctName());
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
    _fctCall.updateName(newFctName);
    _fctCall.updateType(newFctTypeHash);


    // Adapt function call parameters and function declaration
    XargumentsTable args = _fctCall.getArgumentsTable();
    XdeclTable fctDeclarations = clonedFctDef.getDeclarationTable();
    XsymbolTable fctSymbols = clonedFctDef.getSymbolTable();

    if(XmOption.isDebugOutput()){
      System.out.println("  Start to apply mapping: " + _mappings.size());
    }

    for(ClawMapping mapping : _mappings){
      System.out.println("Apply mapping (" + mapping.getMappedDimensions() + ") ");

      for(String var : mapping.getMappedVariables()){

        System.out.println("  Var: " + var);
        XbaseElement argument = args.findArgument(var); // TODO return a dedictaed type
        if(argument == null) {
          continue;
        }

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
            throw new IllegalTransformationException(
                "mapping dimensions too big. Mapping " + mapping.toString() +
                    " is wrong ...", _pragma.getLine());
          }

          XarrayRef newArg = XarrayRef.createEmpty(xcodeml, type.getRef());
          XvarRef varRef = XvarRef.createEmpty(xcodeml, varArg.getType());
          varRef.append(varArg, true);
          newArg.append(varRef);

          //  create arrayIndex
          for(ClawMappingVar mappingVar : mapping.getMappingVariables()){
            XarrayIndex arrayIndex = XarrayIndex.createEmpty(xcodeml);
            // Find the mapping var in the local table (fct scope)
            XvarDecl mappingVarDecl =
                _fctDef.getDeclarationTable().get(mappingVar.getArgMapping());

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



      } // Loop mapped variables
    } // Loop over mapping clauses


    // Adapt array reference in function body
    List<XarrayRef> arrayReferences =
        XelementHelper.getAllArrayReferences(clonedFctDef.getBody());
    for(XarrayRef ref : arrayReferences){
      if(!ref.getVarRef().isVar()){
        continue;
      }
      String mappedVar = ref.getVarRef().getVar().getValue();
      if(_mappingMap.containsKey(mappedVar)){
        ClawMapping mapping = _mappingMap.get(mappedVar);

        boolean changeRef = true;

        int mappingIndex = 0;
        for(XbaseElement e : ref.getInnerElements()){
          if(e instanceof XarrayIndex){
            XarrayIndex arrayIndex = (XarrayIndex)e;
            if(arrayIndex.getExprModel() != null && arrayIndex.getExprModel().isVar()){
              String varName = arrayIndex.getExprModel().getVar().getValue();
              if(varName.equals(mapping.getMappingVariables().get(mappingIndex).getFctMapping())){
                ++mappingIndex;
              } else {
                changeRef = false;
              }
            }
          }
        }
        if(changeRef){
          // TODO Var ref should be extracted only if the reference can be
          // totally demoted
          XelementHelper.insertBefore(ref, ref.getVarRef().getVar().cloneObject());
          ref.delete();
        }
      }
    }

    // Wrap with parallel section if option is set
    if(_hasParallelOption){
      Xpragma parallelStart = Xpragma.createEmpty(xcodeml);
      parallelStart.setData("acc parallel");

      Xpragma parallelEnd = Xpragma.createEmpty(xcodeml);
      parallelEnd.setData("acc end parallel");

      XelementHelper.insertAfter(_pragma, parallelStart);
      XelementHelper.insertAfter(extractedLoop, parallelEnd);

      if(_accAdditionalOption != null){
        insertAccOption(parallelStart, xcodeml);
      }
    } else if (_accAdditionalOption != null){
      insertAccOption(_pragma, xcodeml);
    }



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

  /**
   * Create a new pragma statement and insert it after the insert point
   * @param insertPoint Statement just before the insertion
   * @param xcodeml     The XcodeML representation.
   */
  private void insertAccOption(Xpragma insertPoint, XcodeProg xcodeml){
    Xpragma accAdditionalOption = Xpragma.createEmpty(xcodeml);
    accAdditionalOption.setData("acc " + _accAdditionalOption); // TODO move acc
    XelementHelper.insertAfter(insertPoint, accAdditionalOption);
  }

  /**
   * Wrap a function call with a do statement.
   * @param xcodeml        The XcodeML representation.
   * @param iterationRange Iteration range to be applied to the do statement.
   * @return The created do statement.
   */
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
    if(iterationRange.getIndexRange().getLowerBound().getExprModel().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getLowerBound().getValue());
    }
    if(iterationRange.getIndexRange().getUpperBound().getExprModel().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getUpperBound().getValue());
    }
    if(iterationRange.getIndexRange().getStep().getExprModel().isVar()){
      insertDeclaration(iterationRange.getIndexRange().getStep().getValue());
    }

    return loop;
  }

  /**
   * Insert new declaration in the function definition.
   * @param id The id used for insertion.
   */
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

  /**
   * @see Transformation#canBeTransformedWith(Object)
   * @return Always false as independent transformation are applied one by one.
   */
  public boolean canBeTransformedWith(LoopExtraction other) {
    return false; // Always false as independent transformation
  }
}

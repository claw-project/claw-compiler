/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.common.NestedDoStatement;
import cx2x.translator.language.ClawDimension;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

import java.util.*;

/**
 * The parallelize transformation transforms the code contained in a
 * subtourine/function by adding necessary dimensions and parallelism to the
 * defined data.
 *
 * @author clementval
 */
public class Parallelize extends Transformation {

  private final ClawLanguage _claw;
  private Map<String, ClawDimension> _dimensions;
  private int _overDimensions;
  private XfunctionDefinition _fctDef;

  /**
   * Constructs a new Parallelize transfomration triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
    _overDimensions = 0;
    _claw = directive; // Keep information about the claw directive here
    _dimensions = new HashMap<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // Check for the parent fct/subroutine definition
    _fctDef = XelementHelper.findParentFctDef(_claw.getPragma());
    if(_fctDef == null){
      xcodeml.addError("Parent function/subroutine cannot be found. " +
          "Parallelize directive must be defined in a function/subroutine.",
          _claw.getPragma().getLineNo());
      return false;
    }

    return analyseDimension(xcodeml) && analyseData(xcodeml) &&
        analyseOver(xcodeml);
  }


  /**
   * Analyse the defined dimension.
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseDimension(XcodeProgram xcodeml){
    if(!_claw.hasDimensionClause()){
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma().getLineNo());
      return false;
    }

    for(ClawDimension d : _claw.getDimesionValues()){
      if(_dimensions.containsKey(d.getIdentifier())){
        xcodeml.addError(
            String.format("Dimension with identifier %s already specified.",
                d.getIdentifier()), _claw.getPragma().getLineNo()
        );
        return false;
      }
      _dimensions.put(d.getIdentifier(), d);
    }
    return true;
  }

  /**
   * Analyse the information defined in the data clause.
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseData(XcodeProgram xcodeml){
    if(!_claw.hasDataClause()){
      return true;
    }
    for(String d : _claw.getDataClauseValues()){
      if(!_fctDef.getSymbolTable().contains(d)){
        xcodeml.addError(
            String.format("Data %s is not defined in the current block.", d),
            _claw.getPragma().getLineNo()
        );
        return false;
      }
      if(!_fctDef.getDeclarationTable().contains(d)){
        xcodeml.addError(
            String.format("Data %s is not declared in the current block.", d),
            _claw.getPragma().getLineNo()
        );
        return false;
      }
    }
    return true;
  }

  /**
   * Analyse the information defined in the over clause.
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseOver(XcodeProgram xcodeml){
    if(!_claw.hasOverClause()){
      return true;
    }
    if(!_claw.getOverClauseValues().contains(":")){
      xcodeml.addError("The column dimension has not been specified in the " +
              "over clause. Use : to specify it.",
          _claw.getPragma().getLineNo());
      return false;
    }

    // Check if over dimensions are defined dimensions
    for(String o : _claw.getOverClauseValues()){
      if(!o.equals(ClawDimension.BASE_DIM)){
        ++_overDimensions;
        if(!_dimensions.containsKey(o)){
          xcodeml.addError(
              String.format(
                  "Dimension %s is not defined. Cannot be used in over " +
                      "clause", o), _claw.getPragma().getLineNo()
          );
          return false;
        }
      }
    }
    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other)
      throws Exception
  {
    // Insert the declarations of variables to iterate over the new dimensions
    insertVariableToIterateOverDimension(xcodeml);

    // Promot all array fields with new dimensions.
    promoteFields(xcodeml);

    // Adapt array references.
    adapteArrayReferences(xcodeml);

    // Delete the pragma
    _claw.getPragma().delete();

    // Apply specific target transformation
    if(_claw.getTarget() == Target.GPU){
      transformForGPU(xcodeml);
    } else {
      transformForCPU(xcodeml);
    }
  }

  /**
   * Apply GPU based transformation.
   * @param xcodeml     Current XcodeML program unit.
   * @throws Exception
   */
  private void transformForGPU(XcodeProgram xcodeml)
      throws Exception
  {
    /* Create a nested loop with the new defined dimensions and wrap it around
     * the whole subroutine's body. This is for the moment a really naive
     * transformation idead but it is our start point. */
    NestedDoStatement loops =
        new NestedDoStatement(getOrderedDimensionsFromDefinition(), xcodeml);
    XelementHelper.copyBody(_fctDef.getBody(), loops.getInnerStatement());
    _fctDef.getBody().delete();
    Xbody newBody = XelementHelper.createEmpty(Xbody.class, xcodeml);
    newBody.appendToChildren(loops.getOuterStatement(), false);
    _fctDef.appendToChildren(newBody, false);
    AcceleratorHelper.generateParallelLoopClause(_claw, xcodeml,
        loops.getOuterStatement(), loops.getOuterStatement(),
        loops.getGroupSize());
  }

  /**
   * Apply CPU based transformations.
   * @param xcodeml     Current XcodeML program unit
   * @throws Exception
   */
  private void transformForCPU(XcodeProgram xcodeml)
      throws Exception
  {
    /* Create a group of nested loop with the newly defined dimension and wrap
     * every assignement statement in the column loop or including data with it.
     * This is for the moment a really naive transformation idea but it is our
     * start point. */

    List<ClawDimension> order = getOrderedDimensionsFromDefinition();
    List<XassignStatement> assignStatements =
        XelementHelper.getAllAssignments(_fctDef.getBody());

    for(XassignStatement assign : assignStatements){
      if(!assign.getLValueModel().isArrayRef()){
        continue;
      }

      XarrayRef ref = assign.getLValueModel().getArrayRef();
      if(_claw.getDataClauseValues().
          contains(ref.getVarRef().getVar().getValue()))
      {
        NestedDoStatement loops = new NestedDoStatement(order, xcodeml);
        XelementHelper.insertAfter(assign, loops.getOuterStatement());
        loops.getInnerStatement().getBody().appendToChildren(assign, true);
        assign.delete();
      }
    }
  }


  /**
   * Promote all fields declared in the data clause with the additional
   * dimensions.
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @throws IllegalTransformationException if elements cannot be created or
   * elements cannot be found.
   */
  private void promoteFields(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    for(String data : _claw.getDataClauseValues()){
      Xid id = _fctDef.getSymbolTable().get(data);
      XvarDecl decl = _fctDef.getDeclarationTable().get(data);
      XbasicType bType = (XbasicType) xcodeml.getTypeTable().get(id.getType());
      if(bType == null){
        throw new IllegalTransformationException("Cannot find type for " + data,
            _claw.getPragma().getLineNo());
      }
      XbasicType newType = bType.cloneObject();
      newType.setType(xcodeml.getTypeTable().generateArrayTypeHash());
      for(int i = 0; i < _overDimensions; ++i){
        XindexRange index = XindexRange.createEmptyAssumedShaped(xcodeml);
        newType.addDimension(index, 0);
      }
      id.setType(newType.getType());
      decl.getName().setType(newType.getType());
      xcodeml.getTypeTable().add(newType);
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @throws IllegalTransformationException if elements cannot be created or
   * elements cannot be found.
   */
  private void adapteArrayReferences(XcodeProgram xcodeml) throws
      IllegalTransformationException
  {
    List<XarrayIndex> beforeCrt = new ArrayList<>();
    List<XarrayIndex> afterCrt = new ArrayList<>();
    List<XarrayIndex> crt = beforeCrt;
    if(_claw.hasOverClause()) {

      /* If the over clause is specified, the indexes respect the definition of
       * the over clause. Indexes before the "colon" symbol will be inserted
       * before the current indexes and the remaining indexes will be inserted
       * after the current indexes.  */

      for (String dim : _claw.getOverClauseValues()) {
        if (dim.equals(ClawDimension.BASE_DIM)) {
          crt = afterCrt;
        } else {
          ClawDimension d = _dimensions.get(dim);
          crt.add(d.generateArrayIndex(xcodeml));
        }
      }

    } else {

      /* If no over clause, the indexes are inserted from the defined dimensions
       * from left to right. Everything is inserted on the left of current
       * indexes */

      for(ClawDimension dim : _claw.getDimesionValues()){
        crt.add(dim.generateArrayIndex(xcodeml));
      }
    }

    Collections.reverse(beforeCrt); // Because of insertion order

    for(String data : _claw.getDataClauseValues()){
      List<XarrayRef> refs =
          XelementHelper.getAllArrayReferences(_fctDef.getBody(), data);
      for(XarrayRef ref : refs){

        for(XarrayIndex ai : beforeCrt){
          XelementHelper.insertAfter(ref.getVarRef(), ai.cloneObject());
        }
        for(XarrayIndex ai : afterCrt){
          XelementHelper.insertAfter(
              ref.getInnerElements().get(ref.getInnerElements().size()-1),
              ai.cloneObject());
        }
      }
    }
  }

  /**
   * Insert the declaration of the different variables needed to iterate over
   * the additional dimensions.
   * @param xcodeml Current XcodeML program unit in which element are created.
   * @throws IllegalTransformationException if elements cannot be created or
   * elements cannot be found.
   */
  private void insertVariableToIterateOverDimension(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    // Find function type
    XfunctionType fctType =
        (XfunctionType) xcodeml.getTypeTable().get(_fctDef.getName().getType());

    // Create type and declaration for iterations over the new dimensions
    XbasicType intTypeIntentIn = XbasicType.create(
        xcodeml.getTypeTable().generateIntegerTypeHash(),
        XelementName.TYPE_F_INT, Xintent.IN, xcodeml);
    xcodeml.getTypeTable().add(intTypeIntentIn);

    // For each dimension defined in the directive
    for(ClawDimension dimension : _claw.getDimesionValues()){
      if(dimension.lowerBoundIsVar()){
        createIdAndDecl(dimension.getLowerBoundId(), intTypeIntentIn.getType(),
            XelementName.SCLASS_F_PARAM, xcodeml);
        Xname paramName = Xname.create(dimension.getLowerBoundId(),
            intTypeIntentIn.getType(), xcodeml);
        fctType.getParams().add(paramName);
      }
      if(dimension.upperBoundIsVar()){
        createIdAndDecl(dimension.getUpperBoundId(), intTypeIntentIn.getType(),
            XelementName.SCLASS_F_PARAM, xcodeml);
        Xname paramName = Xname.create(dimension.getUpperBoundId(),
            intTypeIntentIn.getType(), xcodeml);
        fctType.getParams().add(paramName);
      }
      // Create induction variable declaration
      createIdAndDecl(dimension.getIdentifier(), XelementName.TYPE_F_INT,
          XelementName.SCLASS_F_LOCAL, xcodeml);
    }
  }

  /**
   * Create the id and varDecl elements and add them to the symbol/declaration
   * table.
   * @param name    Name of the variable.
   * @param type    Type of the variable.
   * @param sclass  Scope class of the variable (from XelementName).
   * @param xcodeml Current XcodeML program unit in which the elements will be
   *                created.
   * @throws IllegalTransformationException if the elements cannot be created.
   */
  private void createIdAndDecl(String name, String type, String sclass,
                               XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    Xid id = Xid.create(type, sclass, name, xcodeml);
    _fctDef.getSymbolTable().add(id);
    XvarDecl decl = XvarDecl.create(type, name, xcodeml);
    _fctDef.getDeclarationTable().add(decl);
  }

  /**
   * Get the list of dimensions in order from the parallelize over definition.
   * @return Ordered list of dimension object.
   */
  private List<ClawDimension> getOrderedDimensionsFromDefinition(){
    // TODO activate when over clause become optional
    /*if(_claw.hasOverClause()){
      List<ClawDimension> dimensions = new ArrayList<>();
      for(String o : _claw.getOverClauseValues()) {
        if (o.equals(ClawDimension.BASE_DIM)) {
          continue;
        }
        dimensions.add(_dimensions.get(o));
      }
      return dimensions;
    } else {*/
      return _claw.getDimensionValuesReversed();
    //}
  }


  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // This is an independent transformation
  }
}

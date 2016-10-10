/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.common.NestedDoStatement;
import cx2x.translator.language.ClawDimension;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;
import exc.openacc.ACC;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * The parallelize transformation transforms the code contained in a
 * subroutine/function by adding necessary dimensions and parallelism to the
 * defined data.
 *
 * @author clementval
 */
public class Parallelize extends Transformation {

  private final ClawLanguage _claw;
  private final Map<String, ClawDimension> _dimensions;
  private final Map<String, PromotionInfo> _promotions;
  private List<String> _arrayFieldsInOut;
  private final List<String> _scalarFields;
  private int _overDimensions;
  private XfunctionDefinition _fctDef;
  private XfunctionType _fctType;
  private List<List<Xnode>> _beforeCrt, _inMiddle, _afterCrt;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
    _overDimensions = 0;
    _claw = directive; // Keep information about the claw directive here
    _dimensions = new HashMap<>();
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new ArrayList<>();
    _scalarFields = new ArrayList<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // Check for the parent fct/subroutine definition
    _fctDef = XnodeUtil.findParentFunction(_claw.getPragma());
    if(_fctDef == null){
      xcodeml.addError("Parent function/subroutine cannot be found. " +
          "Parallelize directive must be defined in a function/subroutine.",
          _claw.getPragma().getLineNo());
      return false;
    }
    _fctType = (XfunctionType) xcodeml.getTypeTable().
        get(_fctDef.getName().getAttribute(Xattr.TYPE));
    if(_fctType == null){
      xcodeml.addError("Function/subroutine signature cannot be found. ",
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

    for(ClawDimension d : _claw.getDimensionValues()){
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
    /* If there is no data/over clause specified, an automatic deduction for
     * array promotion is performed.
     */
    if(!_claw.hasOverDataClause()){
      for(Xdecl decl : _fctDef.getDeclarationTable().getAll()){
        if(decl.isBuiltInType()){
          if(XmOption.isDebugOutput()){
            System.out.println("parallelize promotion: Scalar "
                + decl.find(Xcode.NAME).getValue()
                + " is candidate for promotion.");
          }
          _scalarFields.add(decl.find(Xcode.NAME).getValue());
        }

        Xtype type = xcodeml.getTypeTable().
            get(decl.find(Xcode.NAME).getAttribute(Xattr.TYPE));
        if(type instanceof XbasicType){
          XbasicType bType = (XbasicType)type;
          if(((bType.getIntent() == Xintent.IN
              || bType.getIntent() == Xintent.OUT
              || bType.getIntent() == Xintent.INOUT)
              || bType.isPointer()) && bType.isArray())
          {
            if(XmOption.isDebugOutput()){
              System.out.println("parallelize promotion: Array " +
                  decl.find(Xcode.NAME).getValue() + " will be promoted.");
            }
            _arrayFieldsInOut.add(decl.find(Xcode.NAME).getValue());
          }
        }
      }
      return true;
    }

    /* If the data clause if defined at least once, manual promotion is the
     * rule. The array idenitfiers defined in the data clauses will be used as
     * the list of array to be promoted.
     * In the analysis, we control that all defined arrays in the data clauses
     * are actual delcared variables.
     */
    for(List<String> data : _claw.getOverDataClauseValues()) {
      for (String d : data) {
        if (!_fctDef.getSymbolTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not defined in the current block.", d),
              _claw.getPragma().getLineNo()
          );
          return false;
        }
        if (!_fctDef.getDeclarationTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not declared in the current block.", d),
              _claw.getPragma().getLineNo()
          );
          return false;
        }
      }
      _arrayFieldsInOut.addAll(data);
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
      _overDimensions += _claw.getDimensionValues().size();
      return true;
    }
    for (List<String> over : _claw.getOverClauseValues()) {
      if(!over.contains(ClawDimension.BASE_DIM)){
        xcodeml.addError("The column dimension has not been specified in the " +
            "over clause. Use : to specify it.",
            _claw.getPragma().getLineNo());
        return false;
      }
      int baseDimNb = TransformationHelper.baseDimensionNb(over);
      if(baseDimNb > 2){
        xcodeml.addError("Too many base dimensions specified in over clause. " +
            "Maximum two base dimensions can be specified.",
            _claw.getPragma().getLineNo());
        return false;
      } else if(baseDimNb == 2){
        if(!over.get(0).equals(ClawDimension.BASE_DIM)
            || !over.get(over.size()-1).equals(ClawDimension.BASE_DIM))
        {
          xcodeml.addError("Base dimensions structure not supported in over" +
              "clause.", _claw.getPragma().getLineNo());
          return false;
        }
      }
    }

    // Check if over dimensions are defined dimensions
    _overDimensions = _claw.getDimensionValues().size();
    for(List<String> overLst: _claw.getOverClauseValues()){
      int usedDimension = 0;
      for(String o : overLst){
        if(!o.equals(ClawDimension.BASE_DIM)){
          if(!_dimensions.containsKey(o)){
            xcodeml.addError(
                String.format(
                    "Dimension %s is not defined. Cannot be used in over " +
                        "clause", o), _claw.getPragma().getLineNo()
            );
            return false;
          }
          ++usedDimension;
        }
      }
      if(usedDimension != _overDimensions){
        xcodeml.addError("Over clause doesn't use one or more defined " +
            "dimensions", _claw.getPragma().getLineNo());
        return false;
      }
    }

    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other)
      throws Exception
  {

    // Prepare the array index that will be added to the array references.
    prepareArrayIndexes(xcodeml);

    // Insert the declarations of variables to iterate over the new dimensions.
    insertVariableToIterateOverDimension(xcodeml);

    // Promote all array fields with new dimensions.
    promoteFields(xcodeml);

    // Adapt array references.
    if(_claw.hasOverDataClause()){
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i){
        TransformationHelper.adaptArrayReferences(
            _claw.getOverDataClauseValues().get(i), i, _fctDef.getBody(),
            _promotions, _beforeCrt, _inMiddle, _afterCrt, xcodeml);
      }
    } else {
      TransformationHelper.adaptArrayReferences(_arrayFieldsInOut, 0,
          _fctDef.getBody(), _promotions, _beforeCrt, _inMiddle, _afterCrt,
          xcodeml);
    }

    // Delete the pragma
    _claw.getPragma().delete();

    // Apply specific target transformation
    if(_claw.getTarget() == Target.GPU){
      transformForGPU(xcodeml);
    } else {
      transformForCPU(xcodeml);
    }

    if(!_fctType.getBooleanAttribute(Xattr.IS_PRIVATE)){
      XmoduleDefinition modDef = XnodeUtil.findParentModule(_fctDef);
      if(modDef != null){
        TransformationHelper.updateModuleSignature(xcodeml, _fctDef, _fctType,
            modDef, _claw, transformer, false);
      }
    }
  }

  /**
   * Apply GPU based transformation.
   * @param xcodeml Current XcodeML program unit.
   */
  private void transformForGPU(XcodeProgram xcodeml)
  {
    /* Create a nested loop with the new defined dimensions and wrap it around
     * the whole subroutine's body. This is for the moment a really naive
     * transformation idea but it is our start point.
     * Use the first over clause to create it. */
    NestedDoStatement loops =
        new NestedDoStatement(getOrderedDimensionsFromDefinition(0), xcodeml);

    /* Subroutine/function can have a contains section with inner subtourines or
     * functions. The newly created (nested) do statements should stop before
     * this contains section if it exists. */
    Xnode contains = _fctDef.getBody().find(Xcode.FCONTAINSSTATEMENT);
    if(contains != null){
      XnodeUtil.shiftStatementsInBody(_fctDef.getBody().getChild(0),
          contains, loops.getInnerStatement().getBody());
      XnodeUtil.insertBefore(contains, loops.getOuterStatement());
    } else {
      // No contains section, all the body is copied to the do statements.
      XnodeUtil.copyBody(_fctDef.getBody(), loops.getInnerStatement());
      _fctDef.getBody().delete();
      Xnode newBody = new Xnode(Xcode.BODY, xcodeml);
      newBody.appendToChildren(loops.getOuterStatement(), false);
      _fctDef.appendToChildren(newBody, false);
    }

    // Generate the data region
    List<String> presents =
        AcceleratorHelper.getPresentVariabes(xcodeml, _fctDef);
    AcceleratorHelper.generateDataRegionClause(_claw, xcodeml, presents,
        loops.getOuterStatement(), loops.getOuterStatement());

    // Generate the parallel region
    List<String> privates =
        AcceleratorHelper.getLocalVariables(xcodeml, _fctDef);
    AcceleratorHelper.generateParallelLoopClause(_claw, xcodeml, privates,
        loops.getOuterStatement(), loops.getOuterStatement(),
        loops.getGroupSize());
  }

  /**
   * Apply CPU based transformations.
   * @param xcodeml Current XcodeML program unit.
   * @throws IllegalTransformationException If promotion of arrays fails.
   */
  private void transformForCPU(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    /* Create a group of nested loop with the newly defined dimension and wrap
     * every assignment statement in the column loop or including data with it.
     * This is for the moment a really naive transformation idea but it is our
     * start point.
     * Use the first over clause to do it. */
    List<ClawDimension> order = getOrderedDimensionsFromDefinition(0);
    List<Xnode> assignStatements =
        XnodeUtil.findAll(Xcode.FASSIGNSTATEMENT, _fctDef.getBody());

    for(Xnode assign : assignStatements){
      if(assign.getChild(0).opcode() == Xcode.FARRAYREF){
        Xnode ref = assign.getChild(0);

        if(_arrayFieldsInOut.contains(ref.find(Xcode.VARREF, Xcode.VAR).
            getValue()))
        {
          NestedDoStatement loops = new NestedDoStatement(order, xcodeml);
          XnodeUtil.insertAfter(assign, loops.getOuterStatement());
          loops.getInnerStatement().getBody().appendToChildren(assign, true);
          assign.delete();
        }
      } else if(assign.getChild(0).opcode() == Xcode.VAR
          && _scalarFields.contains(assign.getChild(0).getValue()))
      {
        /* If the assignment is in the column loop and is composed with some
         * variables, the field must be promoted and the var reference switch
         * to an array reference */
        Xnode lhs = assign.getChild(0);
        List<Xnode> vars = XnodeUtil.findAllReferences(assign);
        if(vars.size() > 1){
          if(!_arrayFieldsInOut.contains(lhs.getValue())){
            _arrayFieldsInOut.add(lhs.getValue());

            PromotionInfo promotionInfo =
                TransformationHelper.promoteField(lhs.getValue(), false, false,
                    0, _overDimensions, _fctDef, _fctType,
                    _claw.getDimensionValues(), _claw, xcodeml, null);
            _promotions.put(lhs.getValue(), promotionInfo);
          }
          adaptScalarRefToArrayReferences(xcodeml,
              Collections.singletonList(lhs.getValue()), 0); // TODO should be fine
          NestedDoStatement loops = new NestedDoStatement(order, xcodeml);
          XnodeUtil.insertAfter(assign, loops.getOuterStatement());
          loops.getInnerStatement().getBody().appendToChildren(assign, true);
          assign.delete();
        }
      }
    }
  }

  /**
   * Prepare the arrayIndex elements that will be inserted before and after the
   * current indexes in the array references.
   * @param xcodeml Current XcodeML program unit in which new elements are
   *                created.
   */
  private void prepareArrayIndexes(XcodeProgram xcodeml) {
    _beforeCrt = new ArrayList<>();
    _afterCrt = new ArrayList<>();
    _inMiddle = new ArrayList<>();

    if(_claw.hasOverClause()) {
      /* If the over clause is specified, the indexes respect the definition of
       * the over clause. Indexes before the "colon" symbol will be inserted
       * before the current indexes and the remaining indexes will be inserted
       * after the current indexes.  */

      for(List<String> over : _claw.getOverClauseValues()){
        List<Xnode> beforeCrt = new ArrayList<>();
        List<Xnode> afterCrt = new ArrayList<>();
        List<Xnode> inMiddle = new ArrayList<>();
        List<Xnode> crt = beforeCrt;

        if(TransformationHelper.baseDimensionNb(over) == 2){ // In middle insertion
          for (String dim : over) {
            if (!dim.equals(ClawDimension.BASE_DIM)) {
              ClawDimension d = _dimensions.get(dim);
              inMiddle.add(d.generateArrayIndex(xcodeml));
            }
          }
        } else {
          for (String dim : over) {
            if (dim.equals(ClawDimension.BASE_DIM)) {
              crt = afterCrt;
            } else {
              ClawDimension d = _dimensions.get(dim);
              crt.add(d.generateArrayIndex(xcodeml));
            }
          }
        }

        Collections.reverse(beforeCrt);
        _beforeCrt.add(beforeCrt);
        _afterCrt.add(afterCrt);
        _inMiddle.add(inMiddle);
      }
    } else {
      /* If no over clause, the indexes are inserted in order from the first
       * defined dimensions from left to right. Everything is inserted on the
       * left of current indexes. */
      List<Xnode> crt = new ArrayList<>();
      List<Xnode> empty = Collections.emptyList();
      for(ClawDimension dim : _claw.getDimensionValues()){
        crt.add(dim.generateArrayIndex(xcodeml));
      }
      Collections.reverse(crt);
      _beforeCrt.add(crt);
      _afterCrt.add(empty);
      _inMiddle.add(empty);
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
    if(_claw.hasOverDataClause()){
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i){
        for (String fieldId : _claw.getOverDataClauseValues().get(i)) {
          PromotionInfo promotionInfo =
              TransformationHelper.promoteField(fieldId, true, true, i,
                  _overDimensions, _fctDef, _fctType,
                  _claw.getDimensionValues(), _claw, xcodeml, null);
          _promotions.put(fieldId, promotionInfo);
        }
      }
    } else {
      // Promote all arrays in a similar manner
      for (String fieldId : _arrayFieldsInOut) {
        PromotionInfo promotionInfo =
            TransformationHelper.promoteField(fieldId, true, true, 0,
                _overDimensions, _fctDef, _fctType, _claw.getDimensionValues(),
                _claw, xcodeml, null);
        _promotions.put(fieldId, promotionInfo);
      }
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @param ids     List of array identifiers that must be adapted.
   * @param index   Index designing the correct over clause to be used.
   */
  private void adaptScalarRefToArrayReferences(XcodeProgram xcodeml,
                                               List<String> ids,
                                               int index)
  {
    for(String id : ids){
      List<Xnode> vars = XnodeUtil.findAllReferences(_fctDef.getBody(), id);

      Xid sId = _fctDef.getSymbolTable().get(id);
      XbasicType type = (XbasicType) xcodeml.getTypeTable().get(sId.getType());

      for(Xnode var : vars){
        Xnode ref =
            XnodeUtil.createArrayRef(xcodeml, type, var.cloneObject());
        for(Xnode ai : _beforeCrt.get(index)){
          XnodeUtil.insertAfter(ref.find(Xcode.VARREF), ai.cloneObject());
        }
        for(Xnode ai : _afterCrt.get(index)){
          ref.appendToChildren(ai, true);
        }

        XnodeUtil.insertAfter(var, ref);
        var.delete();
      }
    }
  }

  /**
   * Insert the declaration of the different variables needed to iterate over
   * the additional dimensions.
   * @param xcodeml     Current XcodeML program unit in which element are
   *                    created.
   */
  private void insertVariableToIterateOverDimension(XcodeProgram xcodeml) {
    // Create type and declaration for iterations over the new dimensions
    XbasicType intTypeIntentIn = XnodeUtil.createBasicType(xcodeml,
        xcodeml.getTypeTable().generateIntegerTypeHash(),
        Xname.TYPE_F_INT, Xintent.IN);
    xcodeml.getTypeTable().add(intTypeIntentIn);

    // For each dimension defined in the directive
    for(ClawDimension dimension : _claw.getDimensionValues()){
      // Create the parameter for the lower bound
      if(dimension.lowerBoundIsVar()){
        XnodeUtil.createIdAndDecl(dimension.getLowerBoundId(),
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, _fctDef, xcodeml);

        // Add parameter to the local type table
        XnodeUtil.createAndAddParam(xcodeml,
            dimension.getLowerBoundId(), intTypeIntentIn.getType(), _fctType);
      }

      // Create parameter for the upper bound
      if(dimension.upperBoundIsVar()){
        XnodeUtil.createIdAndDecl(dimension.getUpperBoundId(),
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, _fctDef, xcodeml);

        // Add parameter to the local type table
        XnodeUtil.createAndAddParam(xcodeml,
            dimension.getUpperBoundId(), intTypeIntentIn.getType(), _fctType);
      }
      // Create induction variable declaration
      XnodeUtil.createIdAndDecl(dimension.getIdentifier(), Xname.TYPE_F_INT,
          Xname.SCLASS_F_LOCAL, _fctDef, xcodeml);
    }
  }

  /**
   * Get the list of dimensions in order from the parallelize over definition.
   * @param overIndex Which over clause to use.
   * @return Ordered list of dimension object.
   */
  private List<ClawDimension> getOrderedDimensionsFromDefinition(int overIndex){
    if(_claw.hasOverClause()){
      List<ClawDimension> dimensions = new ArrayList<>();
      for(String o : _claw.getOverClauseValues().get(overIndex)) {
        if (o.equals(ClawDimension.BASE_DIM)) {
          continue;
        }
        dimensions.add(_dimensions.get(o));
      }
      return dimensions;
    } else {
      return _claw.getDimensionValuesReversed();
    }
  }


  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // This is an independent transformation
  }
}

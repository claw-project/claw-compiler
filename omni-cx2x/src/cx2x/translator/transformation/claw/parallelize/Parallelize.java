/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.common.NestedDoStatement;
import cx2x.translator.common.Utility;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawDimension;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformer.ClawTransformer;
import cx2x.translator.xnode.ClawAttr;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;
import xcodeml.util.XmOption;

import java.util.*;

/**
 * The parallelize transformation transforms the code contained in a
 * subroutine/function by adding necessary dimensions and parallelism to the
 * defined data.
 * <p>
 * Transformation for the GPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Do statements over the additional dimensions is added as an outter
 * loop and wrap the entire body of the subroutine.
 * </ul>
 * <p>
 * Transformation for the CPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Propagated promotion is applied to all scalars or arrays used in an
 * assign statement at the lhs and where a promoted variable is used on the
 * rhs.
 * <li> Do statements over the additional dimensions are added as an inner
 * loop wrapping each assign statements including promoted variables.
 * </ul>
 * <p>
 * Generation of OpenACC directives:<ul>
 * <li> acc routine seq is generated for subroutine called from the parallelized
 * subroutine if they are located in the same translation unit.
 * <li> acc data region with corresponding present clause for all promoted
 * variables with the intent in, out or inout.
 * <li> acc parallel region is generated to wrap all the body of the subroutine.
 * <li> acc private clause is added to the parallel directive for all local
 * variables.
 * <li> acc loop is generated for the generated do statement.
 * <li> acc loop seq is generated for already existing do statements.
 * </ul>
 * <p>
 * Generation of OpenMP directives: <ul>
 * <li> omp parallel do is generated for each generated do statements.
 * </ul>
 *
 * @author clementval
 */
public class Parallelize extends ClawTransformation {

  private static final int DEFAULT_OVER = 0;

  private final Map<String, ClawDimension> _dimensions;
  private final Map<String, PromotionInfo> _promotions;
  private final List<String> _arrayFieldsInOut;
  private final List<String> _scalarFields;
  private int _overDimensions;
  private XfunctionDefinition _fctDef;
  private XfunctionType _fctType;
  private List<List<Xnode>> _beforeCrt, _inMiddle, _afterCrt;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
    _overDimensions = 0;
    _dimensions = new HashMap<>();
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new ArrayList<>();
    _scalarFields = new ArrayList<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // Check for the parent fct/subroutine definition
    _fctDef = XnodeUtil.findParentFunction(_claw.getPragma());
    if(_fctDef == null) {
      xcodeml.addError("Parent function/subroutine cannot be found. " +
              "Parallelize directive must be defined in a function/subroutine.",
          _claw.getPragma().lineNo());
      return false;
    }
    _fctType = (XfunctionType) xcodeml.getTypeTable().
        get(_fctDef.getName().getAttribute(Xattr.TYPE));
    if(_fctType == null) {
      xcodeml.addError("Function/subroutine signature cannot be found. ",
          _claw.getPragma().lineNo());
      return false;
    }

    return analyseDimension(xcodeml) && analyseData(xcodeml) &&
        analyseOver(xcodeml);
  }


  /**
   * Analyse the defined dimension.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseDimension(XcodeProgram xcodeml) {
    if(!_claw.hasDimensionClause()) {
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma().lineNo());
      return false;
    }

    for(ClawDimension d : _claw.getDimensionValues()) {
      if(_dimensions.containsKey(d.getIdentifier())) {
        xcodeml.addError(
            String.format("Dimension with identifier %s already specified.",
                d.getIdentifier()), _claw.getPragma().lineNo()
        );
        return false;
      }
      _dimensions.put(d.getIdentifier(), d);
    }
    return true;
  }

  /**
   * Analyse the information defined in the data clause.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseData(XcodeProgram xcodeml) {
    /* If there is no data/over clause specified, an automatic deduction for
     * array promotion is performed. */
    if(!_claw.hasOverDataClause()) {
      for(Xdecl decl : _fctDef.getDeclarationTable().getAll()) {
        if(decl.isBuiltInType()) {
          if(XmOption.isDebugOutput()) {
            System.out.println("parallelize promotion: Scalar "
                + decl.matchSeq(Xcode.NAME).value()
                + " is candidate for promotion.");
          }
          _scalarFields.add(decl.matchSeq(Xcode.NAME).value());
        }

        if(decl.opcode() != Xcode.VARDECL) {
          continue;
        }

        Xtype type = xcodeml.getTypeTable().
            get(decl.matchSeq(Xcode.NAME).getAttribute(Xattr.TYPE));
        if(type instanceof XbasicType) {
          XbasicType bType = (XbasicType) type;
          if(((bType.getIntent() == Xintent.IN
              || bType.getIntent() == Xintent.OUT
              || bType.getIntent() == Xintent.INOUT)
              || bType.isPointer()) && bType.isArray())
          {
            if(XmOption.isDebugOutput()) {
              System.out.println("parallelize promotion: Array " +
                  decl.matchSeq(Xcode.NAME).value() + " will be promoted.");
            }
            _arrayFieldsInOut.add(decl.matchSeq(Xcode.NAME).value());
          } else if(bType.isArray()) {
            if(XmOption.isDebugOutput()) {
              System.out.println("parallelize promotion: Array "
                  + decl.matchSeq(Xcode.NAME).value()
                  + " is candidate for promotion.");
            }
            _scalarFields.add(decl.matchSeq(Xcode.NAME).value());
          }
        }
      }
      return true;
    }

    /* If the data clause if defined at least once, manual promotion is the
     * rule. The array identifiers defined in the data clauses will be used as
     * the list of array to be promoted.
     * In the analysis, we control that all defined arrays in the data clauses
     * are actual declared variables. */
    for(List<String> data : _claw.getOverDataClauseValues()) {
      for(String d : data) {
        if(!_fctDef.getSymbolTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not defined in the current block.", d),
              _claw.getPragma().lineNo()
          );
          return false;
        }
        if(!_fctDef.getDeclarationTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not declared in the current block.", d),
              _claw.getPragma().lineNo()
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
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyseOver(XcodeProgram xcodeml) {
    if(!_claw.hasOverClause()) {
      _overDimensions += _claw.getDimensionValues().size();
      return true;
    }
    for(List<String> over : _claw.getOverClauseValues()) {
      if(!over.contains(ClawDimension.BASE_DIM)) {
        xcodeml.addError("The column dimension has not been specified in the " +
                "over clause. Use : to specify it.",
            _claw.getPragma().lineNo());
        return false;
      }
      int baseDimNb = TransformationHelper.baseDimensionNb(over);
      if(baseDimNb > 2) {
        xcodeml.addError("Too many base dimensions specified in over clause. " +
                "Maximum two base dimensions can be specified.",
            _claw.getPragma().lineNo());
        return false;
      } else if(baseDimNb == 2) {
        if(!over.get(0).equals(ClawDimension.BASE_DIM)
            || !over.get(over.size() - 1).equals(ClawDimension.BASE_DIM))
        {
          xcodeml.addError("Base dimensions structure not supported in over" +
              "clause.", _claw.getPragma().lineNo());
          return false;
        }
      }
    }

    // Check if over dimensions are defined dimensions
    _overDimensions = _claw.getDimensionValues().size();
    for(List<String> overLst : _claw.getOverClauseValues()) {
      int usedDimension = 0;
      for(String o : overLst) {
        if(!o.equals(ClawDimension.BASE_DIM)) {
          if(!_dimensions.containsKey(o)) {
            xcodeml.addError(
                String.format(
                    "Dimension %s is not defined. Cannot be used in over " +
                        "clause", o), _claw.getPragma().lineNo()
            );
            return false;
          }
          ++usedDimension;
        }
      }
      if(usedDimension != _overDimensions) {
        xcodeml.addError("Over clause doesn't use one or more defined " +
            "dimensions", _claw.getPragma().lineNo());
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

    // Handle PURE function / subroutine
    ClawTransformer trans = (ClawTransformer) transformer;
    boolean pureRemoved = XnodeUtil.removePure(_fctDef, _fctType);
    if(trans.getConfiguration().isForcePure() && pureRemoved) {
      throw new IllegalTransformationException(
          "PURE specifier cannot be removed", _fctDef.lineNo());
    } else if(pureRemoved) {
      String fctName = _fctDef.matchDirectDescendant(Xcode.NAME).value();
      System.out.println("Warning: PURE specifier removed from function " +
          fctName + " at line " + _fctDef.lineNo() + ". Transformation and " +
          "code generation applied to it.");
    }

    // Prepare the array index that will be added to the array references.
    prepareArrayIndexes(xcodeml);

    // Insert the declarations of variables to iterate over the new dimensions.
    insertVariableToIterateOverDimension(xcodeml);

    // Promote all array fields with new dimensions.
    promoteFields(xcodeml);

    // Adapt array references.
    if(_claw.hasOverDataClause()) {
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i) {
        TransformationHelper.adaptArrayReferences(
            _claw.getOverDataClauseValues().get(i), i, _fctDef.body(),
            _promotions, _beforeCrt, _inMiddle, _afterCrt, xcodeml);
      }
    } else {
      TransformationHelper.adaptArrayReferences(_arrayFieldsInOut, DEFAULT_OVER,
          _fctDef.body(), _promotions, _beforeCrt, _inMiddle, _afterCrt,
          xcodeml);
    }

    // Delete the pragma
    _claw.getPragma().delete();

    // Apply specific target transformation
    if(_claw.getTarget() == Target.GPU) {
      transformForGPU(xcodeml);
    } else if(_claw.getTarget() == Target.CPU) {
      transformForCPU(xcodeml);
    }

    if(!_fctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
      XmoduleDefinition modDef = _fctDef.findParentModule();
      if(modDef != null) {
        TransformationHelper.updateModuleSignature(xcodeml, _fctDef, _fctType,
            modDef, _claw, transformer, false);
      }
    }
  }

  /**
   * Apply GPU based transformation.
   *
   * @param xcodeml Current XcodeML program unit.
   */
  private void transformForGPU(XcodeProgram xcodeml)
  {

    AcceleratorHelper.generateLoopSeq(_claw, xcodeml, _fctDef);

    /* Create a nested loop with the new defined dimensions and wrap it around
     * the whole subroutine's body. This is for the moment a really naive
     * transformation idea but it is our start point.
     * Use the first over clause to create it. */
    NestedDoStatement loops =
        new NestedDoStatement(getOrderedDimensionsFromDefinition(0), xcodeml);

    /* Subroutine/function can have a contains section with inner subroutines or
     * functions. The newly created (nested) do statements should stop before
     * this contains section if it exists. */
    Xnode contains = _fctDef.body().matchSeq(Xcode.FCONTAINSSTATEMENT);
    if(contains != null) {
      XnodeUtil.shiftStatementsInBody(_fctDef.body().child(0),
          contains, loops.getInnerStatement().body());
      contains.insertBefore(loops.getOuterStatement());
    } else {
      // No contains section, all the body is copied to the do statements.
      XnodeUtil.copyBody(_fctDef.body(), loops.getInnerStatement());
      _fctDef.body().delete();
      Xnode newBody = new Xnode(Xcode.BODY, xcodeml);
      newBody.append(loops.getOuterStatement(), false);
      _fctDef.append(newBody, false);
    }

    // Generate the data region
    List<String> presents =
        AcceleratorHelper.getPresentVariables(xcodeml, _fctDef);
    AcceleratorHelper.generateDataRegionClause(_claw, xcodeml, presents,
        loops.getOuterStatement(), loops.getOuterStatement());

    // Generate the parallel region
    List<String> privates =
        AcceleratorHelper.getLocalVariables(xcodeml, _fctDef);
    AcceleratorHelper.generateParallelLoopClause(_claw, xcodeml, privates,
        loops.getOuterStatement(), loops.getOuterStatement(),
        loops.getGroupSize());

    AcceleratorHelper.generateRoutineDirectives(_claw, xcodeml, _fctDef);
  }

  /**
   * Apply CPU based transformations.
   *
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
        _fctDef.body().matchAll(Xcode.FASSIGNSTATEMENT);

    for(Xnode assign : assignStatements) {
      Xnode lhs = assign.child(Xnode.LHS);
      String lhsName = (lhs.opcode() == Xcode.VAR) ? lhs.value() :
          lhs.matchSeq(Xcode.VARREF, Xcode.VAR).value();
      NestedDoStatement loops = null;
      if(lhs.opcode() == Xcode.FARRAYREF &&
          _arrayFieldsInOut.contains(lhsName))
      {
        loops = new NestedDoStatement(order, xcodeml);
        assign.insertAfter(loops.getOuterStatement());
        loops.getInnerStatement().body().append(assign, true);
        assign.delete();
      } else if(lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.FARRAYREF
          && _scalarFields.contains(lhsName))
      {
        /* If the assignment is in the column loop and is composed with some
         * promoted variables, the field must be promoted and the var reference
         * switch to an array reference */
        if(shouldBePromoted(assign)) {
          if(!_arrayFieldsInOut.contains(lhsName)) {

            _arrayFieldsInOut.add(lhsName);
            PromotionInfo promotionInfo;
            if(lhs.opcode() == Xcode.VAR) { // Scalar to array
              promotionInfo =
                  TransformationHelper.promoteField(lhsName, false, false,
                      DEFAULT_OVER, _overDimensions, _fctDef, _fctType,
                      _claw.getDimensionValues(), _claw, xcodeml, null);
            } else { // Array to array
              promotionInfo =
                  TransformationHelper.promoteField(lhsName, true, true,
                      DEFAULT_OVER, _overDimensions, _fctDef, _fctType,
                      _claw.getDimensionValues(), _claw, xcodeml, null);
            }
            _promotions.put(lhsName, promotionInfo);
          }
          if(lhs.opcode() == Xcode.VAR) {
            adaptScalarRefToArrayReferences(xcodeml,
                Collections.singletonList(lhsName), DEFAULT_OVER);
          } else {
            TransformationHelper.adaptArrayReferences(
                Collections.singletonList(lhsName), DEFAULT_OVER,
                _fctDef.body(), _promotions, _beforeCrt, _inMiddle,
                _afterCrt, xcodeml);
          }
          loops = new NestedDoStatement(order, xcodeml);
          assign.insertAfter(loops.getOuterStatement());
          loops.getInnerStatement().body().append(assign, true);
          assign.delete();
        }
      }
      if(loops != null) {
        // Generate the corresponding directive around the loop
        AcceleratorHelper.generateLoopDirectives(_claw, xcodeml,
            loops.getOuterStatement(), loops.getOuterStatement(),
            AcceleratorHelper.NO_COLLAPSE);
      }
    }

    // Generate the parallel region
    AcceleratorHelper.generateParallelClause(_claw, xcodeml,
        _fctDef.body().firstChild(), _fctDef.body().lastChild());
  }

  /**
   * Check whether the LHS variable should be promoted.
   *
   * @param assignStmt Assign statement node.
   * @return True if the LHS variable should be promoted. False otherwise.
   */
  private boolean shouldBePromoted(Xnode assignStmt) {
    Xnode rhs = assignStmt.child(Xnode.RHS);
    if(rhs == null) {
      return false;
    }
    List<Xnode> vars = XnodeUtil.findAllReferences(rhs);
    Set<String> names = XnodeUtil.getNamesFromReferences(vars);
    return Utility.hasIntersection(names, _arrayFieldsInOut);
  }

  /**
   * Prepare the arrayIndex elements that will be inserted before and after the
   * current indexes in the array references.
   *
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

      for(List<String> over : _claw.getOverClauseValues()) {
        List<Xnode> beforeCrt = new ArrayList<>();
        List<Xnode> afterCrt = new ArrayList<>();
        List<Xnode> inMiddle = new ArrayList<>();
        List<Xnode> crt = beforeCrt;

        // In middle insertion
        if(TransformationHelper.baseDimensionNb(over) == 2) {
          for(String dim : over) {
            if(!dim.equals(ClawDimension.BASE_DIM)) {
              ClawDimension d = _dimensions.get(dim);
              inMiddle.add(d.generateArrayIndex(xcodeml));
            }
          }
        } else {
          for(String dim : over) {
            if(dim.equals(ClawDimension.BASE_DIM)) {
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
      for(ClawDimension dim : _claw.getDimensionValues()) {
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
   *
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @throws IllegalTransformationException if elements cannot be created or
   *                                        elements cannot be found.
   */
  private void promoteFields(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(_claw.hasOverDataClause()) {
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i) {
        for(String fieldId : _claw.getOverDataClauseValues().get(i)) {
          PromotionInfo promotionInfo =
              TransformationHelper.promoteField(fieldId, true, true, i,
                  _overDimensions, _fctDef, _fctType,
                  _claw.getDimensionValues(), _claw, xcodeml, null);
          _promotions.put(fieldId, promotionInfo);
        }
      }
    } else {
      // Promote all arrays in a similar manner
      for(String fieldId : _arrayFieldsInOut) {
        PromotionInfo promotionInfo =
            TransformationHelper.promoteField(fieldId, true, true, DEFAULT_OVER,
                _overDimensions, _fctDef, _fctType, _claw.getDimensionValues(),
                _claw, xcodeml, null);
        _promotions.put(fieldId, promotionInfo);
      }
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   *
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @param ids     List of array identifiers that must be adapted.
   * @param index   Index designing the correct over clause to be used.
   */
  private void adaptScalarRefToArrayReferences(XcodeProgram xcodeml,
                                               List<String> ids,
                                               int index)
  {
    for(String id : ids) {
      List<Xnode> vars = XnodeUtil.findAllReferences(_fctDef.body(), id);

      Xid sId = _fctDef.getSymbolTable().get(id);
      XbasicType type = (XbasicType) xcodeml.getTypeTable().get(sId.getType());

      for(Xnode var : vars) {
        Xnode ref = xcodeml.createArrayRef(type, var.cloneNode());
        for(Xnode ai : _beforeCrt.get(index)) {
          ref.matchSeq(Xcode.VARREF).insertAfter(ai.cloneNode());
        }
        for(Xnode ai : _afterCrt.get(index)) {
          ref.append(ai, true);
        }

        var.insertAfter(ref);
        var.delete();
      }
    }
  }

  /**
   * Insert the declaration of the different variables needed to iterate over
   * the additional dimensions.
   *
   * @param xcodeml Current XcodeML program unit in which element are
   *                created.
   */
  private void insertVariableToIterateOverDimension(XcodeProgram xcodeml) {
    // Create type and declaration for iterations over the new dimensions
    XbasicType intTypeIntentIn = xcodeml.createBasicType(
        xcodeml.getTypeTable().generateIntegerTypeHash(),
        Xname.TYPE_F_INT, Xintent.IN);
    xcodeml.getTypeTable().add(intTypeIntentIn);

    // For each dimension defined in the directive
    for(ClawDimension dimension : _claw.getDimensionValues()) {
      // Create the parameter for the lower bound
      if(dimension.lowerBoundIsVar()) {
        xcodeml.createIdAndDecl(dimension.getLowerBoundId(),
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, _fctDef);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(dimension.getLowerBoundId(),
            intTypeIntentIn.getType(), _fctType);
        param.setAttribute(ClawAttr.IS_CLAW.toString(), Xname.TRUE);
      }

      // Create parameter for the upper bound
      if(dimension.upperBoundIsVar()) {
        xcodeml.createIdAndDecl(dimension.getUpperBoundId(),
            intTypeIntentIn.getType(), Xname.SCLASS_F_PARAM, _fctDef);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(dimension.getUpperBoundId(),
            intTypeIntentIn.getType(), _fctType);
        param.setAttribute(ClawAttr.IS_CLAW.toString(), Xname.TRUE);
      }
      // Create induction variable declaration
      xcodeml.createIdAndDecl(dimension.getIdentifier(), Xname.TYPE_F_INT,
          Xname.SCLASS_F_LOCAL, _fctDef);
    }
  }

  /**
   * Get the list of dimensions in order from the parallelize over definition.
   *
   * @param overIndex Which over clause to use.
   * @return Ordered list of dimension object.
   */
  private List<ClawDimension> getOrderedDimensionsFromDefinition(int overIndex)
  {
    if(_claw.hasOverClause()) {
      List<ClawDimension> dimensions = new ArrayList<>();
      for(String o : _claw.getOverClauseValues().get(overIndex)) {
        if(o.equals(ClawDimension.BASE_DIM)) {
          continue;
        }
        dimensions.add(_dimensions.get(o));
      }
      return dimensions;
    } else {
      return _claw.getDimensionValuesReversed();
    }
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // This is an independent transformation
  }
}

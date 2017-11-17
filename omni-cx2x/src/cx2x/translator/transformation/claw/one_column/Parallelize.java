/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.one_column;

import cx2x.configuration.CompilerDirective;
import cx2x.configuration.Configuration;
import cx2x.configuration.Target;
import cx2x.configuration.openacc.OpenAccLocalStrategy;
import cx2x.translator.common.Message;
import cx2x.translator.common.Utility;
import cx2x.translator.directive.Directive;
import cx2x.translator.language.ClawPragma;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformation.primitive.Body;
import cx2x.translator.transformation.primitive.Field;
import cx2x.translator.transformation.primitive.Module;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.NestedDoStatement;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.*;

import java.util.*;

/**
 * The one_column transformation transforms the code contained in a
 * subroutine/function by adding necessary dimensions and parallelism to the
 * defined data.
 * <p>
 * Transformation for the GPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Do statements over the additional dimensions is added as an outer
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

  private final Map<String, DimensionDefinition> _dimensions;
  private final Map<String, PromotionInfo> _promotions;
  private final List<String> _arrayFieldsInOut;
  private final List<String> _scalarFields;
  private int _overDimensions;
  private XfunctionDefinition _fctDef;
  private XfunctionType _fctType;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawPragma directive) {
    super(directive);
    _overDimensions = 0;
    _dimensions = new HashMap<>();
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new ArrayList<>();
    _scalarFields = new ArrayList<>();
  }

  /**
   * Print information about promoted arrays, candidate for promotion arrays and
   * scalars.
   *
   * @param name            Name of the subroutine.
   * @param promoted        List of promoted array variables.
   * @param candidateArrays List of candidate array variables for promotion.
   * @param scalars         List of candidate scalar variables for promotion.
   */
  private void printDebugPromotionInfos(String name, List<String> promoted,
                                        List<String> candidateArrays,
                                        List<String> scalars)
  {
    Message.debug("==========================================");
    Message.debug("Parallelize promotion infos for subroutine " + name);
    Message.debug("  - Promoted arrays(" + promoted.size() + "):");
    for(String array : promoted) {
      Message.debug("      " + array);
    }
    Message.debug("  - Candidate arrays(" + candidateArrays.size() + "):");
    for(String array : candidateArrays) {
      Message.debug("      " + array);
    }
    Message.debug("  - Candidate scalars(" + scalars.size() + "):");
    for(String array : scalars) {
      Message.debug("      " + array);
    }
    Message.debug("==========================================");
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {

    // Check for the parent fct/subroutine definition
    _fctDef = _claw.getPragma().findParentFunction();
    if(_fctDef == null) {
      xcodeml.addError("Parent function/subroutine cannot be found. " +
              "Parallelize directive must be defined in a function/subroutine.",
          _claw.getPragma().lineNo());
      return false;
    }
    _fctType = xcodeml.getTypeTable().getFunctionType(_fctDef);
    if(_fctType == null) {
      xcodeml.addError("Function/subroutine signature cannot be found. ",
          _claw.getPragma().lineNo());
      return false;
    }

    /* Check if unsupported statements are located in the future parallel
     * region. */
    if(_claw.getTarget() == Target.GPU
        && _claw.getDirectiveLanguage() == CompilerDirective.OPENACC)
    {
      Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
      Xnode parallelRegionStart = Directive.findParallelRegionStart(
          _claw.getAcceleratorGenerator(), _fctDef, null);
      Xnode parallelRegionEnd = Directive.findParallelRegionEnd(
          _claw.getAcceleratorGenerator(), _fctDef, contains);

      List<Xnode> unsupportedStatements =
          XnodeUtil.getNodes(parallelRegionStart, parallelRegionEnd,
              _claw.getAcceleratorGenerator().getUnsupportedStatements());

      if(unsupportedStatements.size() > 0) {
        for(Xnode statement : unsupportedStatements) {
          xcodeml.addError("Unsupported statement in parallel region",
              statement.lineNo());
        }
        return false;
      }
    }

    return analyzeDimension(xcodeml) && analyzeData(xcodeml) &&
        analyzeOver(xcodeml);
  }

  /**
   * Analyse the defined dimension.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeDimension(XcodeProgram xcodeml) {
    if(!_claw.hasDimensionClause()) {
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma().lineNo());
      return false;
    }

    for(DimensionDefinition d : _claw.getDimensionValues()) {
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
  private boolean analyzeData(XcodeProgram xcodeml) {
    /* If there is no data/over clause specified, an automatic deduction for
     * array promotion is performed. */
    if(!_claw.hasOverDataClause()) {

      List<String> scalars = new ArrayList<>();
      List<String> candidateArrays = new ArrayList<>();

      List<Xnode> declarations = _fctDef.getDeclarationTable().values();
      for(Xnode decl : declarations) {
        if(decl.opcode() != Xcode.VAR_DECL) {
          continue;
        }

        if(xcodeml.getTypeTable().isBasicType(decl)) {
          String varName = decl.matchSeq(Xcode.NAME).value();
          XbasicType bType = xcodeml.getTypeTable().getBasicType(decl);

          if(bType.isArray()) {
            if(bType.hasIntent() || bType.isPointer()) {
              _arrayFieldsInOut.add(varName);
            } else {
              candidateArrays.add(varName);
            }
          } else {
            if(_claw.hasScalarClause() &&
                _claw.getScalarClauseValues().contains(varName))
            {
              _arrayFieldsInOut.add(varName);
            }
            if(!bType.isParameter() && bType.hasIntent()) {
              scalars.add(varName);
            }
          }
        }
      }
      _scalarFields.addAll(scalars);
      _scalarFields.addAll(candidateArrays);

      printDebugPromotionInfos(_fctDef.getName(), _arrayFieldsInOut,
          candidateArrays, scalars);

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
  private boolean analyzeOver(XcodeProgram xcodeml) {
    if(!_claw.hasOverClause()) {
      _overDimensions += _claw.getDimensionValues().size();
      return true;
    }

    // Check if over dimensions are defined dimensions
    _overDimensions = _claw.getDimensionValues().size();
    for(List<String> overLst : _claw.getOverClauseValues()) {
      int usedDimension = 0;
      for(String o : overLst) {
        if(!o.equals(DimensionDefinition.BASE_DIM)) {
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
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Handle PURE function / subroutine
    boolean pureRemoved = _fctType.isPure();
    _fctType.removeAttribute(Xattr.IS_PURE);
    if(Configuration.get().isForcePure() && pureRemoved) {
      throw new IllegalTransformationException(
          "PURE specifier cannot be removed", _fctDef.lineNo());
    } else if(pureRemoved) {
      String fctName = _fctDef.matchDirectDescendant(Xcode.NAME).value();
      xcodeml.addWarning("Warning: PURE specifier removed from function " +
              fctName + ". Transformation and code generation applied to it.",
          _fctDef.lineNo());
    }

    // Insert the declarations of variables to iterate over the new dimensions.
    insertVariableToIterateOverDimension(xcodeml);

    // Promote all array fields with new dimensions.
    promoteFields(xcodeml);

    // Adapt array references.
    if(_claw.hasOverDataClause()) {
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i) {
        for(String id : _claw.getOverDataClauseValues().get(i)) {
          Field.adaptArrayRef(_promotions.get(id), _fctDef.body(), xcodeml);
        }
      }
    } else {
      for(String id : _arrayFieldsInOut) {
        Field.adaptArrayRef(_promotions.get(id), _fctDef.body(), xcodeml);
      }
    }

    removePragma();

    // Apply specific target transformation
    if(_claw.getTarget() == Target.GPU) {
      transformForGPU(xcodeml);
    } else if(_claw.getTarget() == Target.CPU) {
      transformForCPU(xcodeml);
    }

    if(!_fctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
      XmoduleDefinition modDef = _fctDef.findParentModule();
      if(modDef != null) {
        Module.updateSignature(modDef.getName(), xcodeml, _fctDef, _fctType,
            translator.getModCache(), false);
      }
    }
  }

  /**
   * Apply GPU based transformation.
   *
   * @param xcodeml Current XcodeML program unit.
   */
  private void transformForGPU(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {

    Directive.generateLoopSeq(_claw, xcodeml, _fctDef);

    /* Create a nested loop with the new defined dimensions and wrap it around
     * the whole subroutine's body. This is for the moment a really naive
     * transformation idea but it is our start point.
     * Use the first over clause to create it. */
    NestedDoStatement loops =
        new NestedDoStatement(_claw.getDimensionValuesReversed(), xcodeml);

    /* Subroutine/function can have a contains section with inner subroutines or
     * functions. The newly created (nested) do statements should stop before
     * this contains section if it exists. */
    Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
    if(contains != null) {

      Xnode parallelRegionStart = Directive.findParallelRegionStart(
          _claw.getAcceleratorGenerator(), _fctDef, null);
      Xnode parallelRegionEnd = Directive.findParallelRegionEnd(
          _claw.getAcceleratorGenerator(), _fctDef, contains);

      Body.shiftIn(parallelRegionStart, parallelRegionEnd,
          loops.getInnerStatement().body(), true);

      contains.insertBefore(loops.getOuterStatement());
    } else {
      // No contains section, all the body is copied to the do statements.

      Xnode parallelRegionStart = Directive.findParallelRegionStart(
          _claw.getAcceleratorGenerator(), _fctDef, null);
      Xnode parallelRegionEnd = Directive.findParallelRegionEnd(
          _claw.getAcceleratorGenerator(), _fctDef, null);

      // Define a hook from where we can insert the new do statement
      Xnode hook = parallelRegionEnd.nextSibling();
      Body.shiftIn(parallelRegionStart, parallelRegionEnd,
          loops.getInnerStatement().body(), true);

      // Hook is null then we append the do statement to the current fct body
      if(hook == null) {
        _fctDef.body().append(loops.getOuterStatement());
      } else {
        // Insert new do statement before the hook element
        hook.insertBefore(loops.getOuterStatement());
      }
    }

    // Prepare variables list for present/pcreate clauses and handle
    // promotion/privatize local strategy
    List<String> presentList =
        Directive.getPresentVariables(xcodeml, _fctDef);
    List<String> privateList = Collections.emptyList();
    List<String> createList = Collections.emptyList();
    if(Configuration.get().openACC().getLocalStrategy()
        == OpenAccLocalStrategy.PRIVATE)
    {
      privateList = Directive.getLocalArrays(xcodeml, _fctDef);
    } else if(Configuration.get().openACC().getLocalStrategy()
        == OpenAccLocalStrategy.PROMOTE)
    {
      createList = Directive.getLocalArrays(xcodeml, _fctDef);
      for(String arrayIdentifier : createList) {
        _arrayFieldsInOut.add(arrayIdentifier);
        PromotionInfo promotionInfo = new PromotionInfo(arrayIdentifier,
            _claw.getDimensionsForData(arrayIdentifier));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(arrayIdentifier, promotionInfo);

        Field.adaptArrayRef(promotionInfo, _fctDef.body(), xcodeml);
        Field.adaptAllocate(promotionInfo, _fctDef.body(), xcodeml);
      }
    }

    // Generate the data region
    Directive.generateDataRegionClause(_claw, xcodeml, presentList,
        createList, loops.getOuterStatement(), loops.getOuterStatement());

    // Generate the parallel region
    Directive.generateParallelLoopClause(_claw, xcodeml, privateList,
        loops.getOuterStatement(), loops.getOuterStatement(),
        loops.size());

    Directive.generateRoutineDirectives(_claw, xcodeml, _fctDef);
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
    List<Xnode> assignStatements =
        _fctDef.body().matchAll(Xcode.F_ASSIGN_STATEMENT);

    for(Xnode assign : assignStatements) {
      Xnode lhs = assign.child(Xnode.LHS);
      String lhsName = (lhs.opcode() == Xcode.VAR) ? lhs.value() :
          lhs.matchSeq(Xcode.VAR_REF, Xcode.VAR).value();
      NestedDoStatement loops = null;
      if(lhs.opcode() == Xcode.F_ARRAY_REF &&
          _arrayFieldsInOut.contains(lhsName))
      {
        loops = new NestedDoStatement(_claw.getDimensionValuesReversed(),
            xcodeml);
        assign.insertAfter(loops.getOuterStatement());
        loops.getInnerStatement().body().append(assign, true);
        assign.delete();
      } else if(lhs.opcode() == Xcode.VAR || lhs.opcode() == Xcode.F_ARRAY_REF
          && _scalarFields.contains(lhsName))
      {
        /* If the assignment is in the column loop and is composed with some
         * promoted variables, the field must be promoted and the var reference
         * switch to an array reference */
        if(shouldBePromoted(assign)) {
          if(!_arrayFieldsInOut.contains(lhsName)) {
            _arrayFieldsInOut.add(lhsName);
            PromotionInfo promotionInfo =
                new PromotionInfo(lhsName, _claw.getDimensionsForData(lhsName));
            Field.promote(promotionInfo, _fctDef, xcodeml);
            _promotions.put(lhsName, promotionInfo);
          }
          if(lhs.opcode() == Xcode.VAR) {
            Field.adaptScalarRefToArrayRef(lhsName, _fctDef,
                _claw.getDimensionValues(), xcodeml);
          } else {
            Field.adaptArrayRef(_promotions.get(lhsName), _fctDef.body(),
                xcodeml);
            Field.adaptAllocate(_promotions.get(lhsName), _fctDef.body(),
                xcodeml);
          }
          loops = new NestedDoStatement(_claw.getDimensionValuesReversed(),
              xcodeml);
          assign.insertAfter(loops.getOuterStatement());
          loops.getInnerStatement().body().append(assign, true);
          assign.delete();
        }
      }
      if(loops != null) {
        // Generate the corresponding directive around the loop
        Directive.generateLoopDirectives(_claw, xcodeml,
            loops.getOuterStatement(), loops.getOuterStatement(),
            Directive.NO_COLLAPSE);
      }
    }

    // Generate the parallel region
    Directive.generateParallelClause(_claw, xcodeml,
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
          PromotionInfo promotionInfo = new PromotionInfo(fieldId,
              _claw.getDimensionsForData(fieldId));
          Field.promote(promotionInfo, _fctDef, xcodeml);
          _promotions.put(fieldId, promotionInfo);
        }
      }
    } else {
      // Promote all arrays in a similar manner
      for(String fieldId : _arrayFieldsInOut) {
        PromotionInfo promotionInfo = new PromotionInfo(fieldId,
            _claw.getDimensionsForData(fieldId));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(fieldId, promotionInfo);
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
    XbasicType bt = xcodeml.createBasicType(XcodeType.INTEGER, Xintent.IN);
    xcodeml.getTypeTable().add(bt);

    // For each dimension defined in the directive
    for(DimensionDefinition dimension : _claw.getDimensionValues()) {
      // Create the parameter for the lower bound
      if(dimension.getLowerBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getLowerBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef, true);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getLowerBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }

      // Create parameter for the upper bound
      if(dimension.getUpperBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getUpperBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef, true);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getUpperBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }
      // Create induction variable declaration
      xcodeml.createIdAndDecl(dimension.getIdentifier(), XcodeType.INTEGER,
          XstorageClass.F_LOCAL, _fctDef, false);
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

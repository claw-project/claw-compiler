/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.*;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.*;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.*;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.ModelConfig;
import claw.wani.x2t.translator.ClawTranslator;

import java.util.*;

/**
 * The Single Column Abstraction (SCA) transformation transforms the code
 * contained in a subroutine/function by adding necessary dimensions and
 * parallelism to the defined data.
 *
 * This class holds the basic common elements for all targets. Specific
 * transformations are detailed in the children classes:
 *
 * @author clementval
 * @see claw.wani.transformation.sca.ScaGPU
 * @see claw.wani.transformation.sca.ScaCPUbasic
 * @see claw.wani.transformation.sca.ScaCPUsmartFusion
 */
public class Sca extends ClawTransformation {

  final Map<String, PromotionInfo> _promotions;
  final Set<String> _arrayFieldsInOut;
  final Set<String> _scalarFields;
  FfunctionDefinition _fctDef;
  private int _overDimensions;
  private FfunctionType _fctType;

  /**
   * Constructs a new Sca transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  Sca(ClawPragma directive) {
    super(directive);
    _overDimensions = 0;
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new HashSet<>();
    _scalarFields = new HashSet<>();
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
  private void printDebugPromotionInfos(String name, Set<String> promoted,
                                        List<String> candidateArrays,
                                        List<String> scalars)
  {
    Message.debug("==========================================");
    Message.debug("Sca promotion infos for subroutine " + name);
    Message.debug("  - Promoted arrays(" + promoted.size() + "):");
    for(String array : promoted) {
      Message.debug("\t" + array);
    }
    Message.debug("  - Candidate arrays(" + candidateArrays.size() + "):");
    for(String array : candidateArrays) {
      Message.debug("\t" + array);
    }
    Message.debug("  - Candidate scalars(" + scalars.size() + "):");
    for(String array : scalars) {
      Message.debug("\t" + array);
    }
    Message.debug("==========================================");
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {

    ClawTranslator trans = (ClawTranslator) translator;

    // Check for the parent fct/subroutine definition
    _fctDef = _claw.getPragma().findParentFunction();
    if(_fctDef == null) {
      xcodeml.addError("Parent function/subroutine cannot be found. " +
              "Sca directive must be defined in a function/subroutine.",
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
    if(Context.isTarget(Target.GPU)
        && (Context.get().getGenerator().getDirectiveLanguage()
        != CompilerDirective.NONE))
    {
      Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
      Xnode parallelRegionStart =
          Directive.findParallelRegionStart(_fctDef, null);
      Xnode parallelRegionEnd =
          Directive.findParallelRegionEnd(_fctDef, contains);

      List<Xnode> unsupportedStatements =
          XnodeUtil.getNodes(parallelRegionStart, parallelRegionEnd,
              Context.get().getGenerator().getUnsupportedStatements());

      if(!unsupportedStatements.isEmpty()) {
        for(Xnode statement : unsupportedStatements) {
          xcodeml.addError("Unsupported statement in parallel region",
              statement.lineNo());
        }
        return false;
      }
    }

    if(_claw.hasDimensionClause()) {
      return analyzeDimension(xcodeml) && analyzeData(xcodeml, trans) &&
          analyzeOver(xcodeml);
    } else {
      // Run analysis for model config only
      return analyzeDimension(xcodeml) && analyzeData(xcodeml, trans);
    }
  }

  /**
   * Analyse the defined dimension.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeDimension(XcodeProgram xcodeml) {
    if(!_claw.hasDimensionClause()
        && ModelConfig.get().getNbDimensions() == 0)
    {
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma().lineNo());
      return false;
    }

    if(_claw.hasDimensionClause()) {
      for(DimensionDefinition d : _claw.getDimensionValues()) {
        if(ModelConfig.get().getDimension(d.getIdentifier()) != null) {
          xcodeml.addError(
              String.format("Dimension with identifier %s already specified.",
                  d.getIdentifier()), _claw.getPragma().lineNo()
          );
          return false;
        }
        ModelConfig.get().putDimension(d.getIdentifier(), d);
      }
    }
    return true;
  }

  /**
   * Analyse the information defined in the data clause.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeData(XcodeProgram xcodeml, ClawTranslator trans) {
    if(!_claw.hasOverDataClause() && _claw.hasDimensionClause()) { // Full directive without over clause
      return analyzeDataForAutomaticPromotion(xcodeml);
    } else if(_claw.hasOverDataClause()) {
      return analyzeDataFromOverClause(xcodeml);
    } else {
      return analyzeModelData(xcodeml, trans);
    }
  }

  private boolean analyzeModelData(XcodeProgram xcodeml, ClawTranslator trans) {
    Set<String> modelVariables;
    if(trans.hasElement(_fctDef) != null) {
      modelVariables = Utility.convertToSet(trans.hasElement(_fctDef));

      for(String data : modelVariables) {
        _arrayFieldsInOut.add(data);
      }

      return true;
    } else {
      xcodeml.addError("No model-data defined in function " + _fctDef.getName(),
          _claw.getPragma().lineNo());
      return false;
    }
  }

  /**
   * If there is no data/over clause specified, an automatic deduction for
   * array promotion is performed.
   *
   * @param xcodeml Current translation unit
   * @return True if the analyzis succeed. False otherwise.
   */
  private boolean analyzeDataForAutomaticPromotion(XcodeProgram xcodeml) {
    List<String> scalars = new ArrayList<>();
    List<String> candidateArrays = new ArrayList<>();
    List<Xnode> declarations =
        _fctDef.getDeclarationTable().values(Xcode.VAR_DECL);

    for(Xnode decl : declarations) {
      if(xcodeml.getTypeTable().isBasicType(decl)) {
        String varName = decl.matchSeq(Xcode.NAME).value();
        FbasicType bType = xcodeml.getTypeTable().getBasicType(decl);

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

  /**
   * If the data clause is defined at least once, manual promotion is the
   * rule.
   * The array identifiers defined in the data clauses will be used as
   * the list of array to be promoted.
   * In the analysis, we control that all defined arrays in the data clauses
   * are actual declared variables.
   *
   * @param xcodeml Current translation unit.
   * @return True if all variable in over clause are real variables. False
   * otherwise.
   */
  private boolean analyzeDataFromOverClause(XcodeProgram xcodeml) {
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
          if(!ModelConfig.get().hasDimension(o)) {
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
      xcodeml.addWarning("PURE specifier removed from function " + fctName +
              ". Transformation and code generation applied to it.",
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
  }

  /**
   * This method should be call by any class inheriting this class to apply
   * the last steps fo the transformation common to all SCA transformation.
   *
   * @param xcodeml Current translation unit.
   */
  void finalizeTransformation(XcodeProgram xcodeml)
      throws IllegalTransformationException

  {
    if(!_fctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
      FmoduleDefinition modDef = _fctDef.findParentModule();
      if(modDef != null) {
        Xmod.updateSignature(modDef.getName(), xcodeml, _fctDef, _fctType,
            false);
      }
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
            getDimensionsForData(fieldId));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(fieldId, promotionInfo);
      }
    }
  }

  protected List<DimensionDefinition> getDimensionsForData(String fieldId) {
    if(_claw.hasDimensionClause()){
      return _claw.getDimensionsForData(fieldId);
    } else {
      return ModelConfig.get().getDefaultLayout();
    }
  }

  protected List<DimensionDefinition> getDimensionValuesReversed() {
    if(_claw.hasDimensionClause()){
      return _claw.getDimensionValuesReversed();
    } else {
      List<DimensionDefinition> tmp = new ArrayList<>(ModelConfig.get().getDefaultLayout());
      Collections.reverse(tmp);
      return tmp;
    }
  }

  protected List<DimensionDefinition> getDimensionValues() {
    if(_claw.hasDimensionClause()){
      return _claw.getDimensionValues();
    } else {
      return ModelConfig.get().getDefaultLayout();
    }
  }



  private List<DimensionDefinition> getUsedDimensions() {
    if(_claw.hasDimensionClause()) {
      return _claw.getDimensionValues();
    } else {
      return ModelConfig.get().getDefaultLayout();
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
    FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.IN);
    xcodeml.getTypeTable().add(bt);

    // For each dimension defined in the directive
    for(DimensionDefinition dimension : getUsedDimensions()) {
      // Create the parameter for the lower bound
      if(dimension.getLowerBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getLowerBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef,
            DeclarationPosition.FIRST);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getLowerBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }

      // Create parameter for the upper bound
      if(dimension.getUpperBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getUpperBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef,
            DeclarationPosition.FIRST);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getUpperBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }
      // Create induction variable declaration
      xcodeml.createIdAndDecl(dimension.getIdentifier(), FortranType.INTEGER,
          XstorageClass.F_LOCAL, _fctDef, DeclarationPosition.LAST);
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
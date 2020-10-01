/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.*;
import claw.tatsu.primitive.*;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.*;
import claw.wani.language.ClawPragma;
import claw.wani.language.ClawClause;
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
 * @see claw.wani.transformation.sca.ScaCPUvectorizeGroup
 */
public class Sca extends ClawTransformation {

  final Map<String, PromotionInfo> _promotions;
  final Set<String> _arrayFieldsInOut;
  final Set<String> _scalarFields;
  final Set<String> _noPromotion;
  FfunctionDefinition _fctDef;
  Set<String> _inductionVariables;
  FfunctionType _fctType;

  protected boolean forceAssumedShapedArrayPromotion = false;

  static final String SCA_DEBUG_PREFIX = "SCA:";

  /**
   * Constructs a new Sca transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  Sca(ClawPragma directive) {
    super(directive);
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new HashSet<>();
    _scalarFields = new HashSet<>();
    _noPromotion = new HashSet<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    // Analysis is performed in child classes.
    return false;
  }

  /**
   * Print information about promoted arrays, candidate for promotion arrays and
   * scalars.
   *
   * @param name            Name of the subroutine.
   * @param candidateArrays List of candidate array variables for promotion.
   * @param scalars         List of candidate scalar variables for promotion.
   */
  private void printDebugPromotionInfos(String name,
                                        List<String> candidateArrays,
                                        List<String> scalars)
  {
    Message.debug("==========================================");
    Message.debug("SCA automatic promotion deduction information for subroutine "
        + name);
    Message.debug("  - Promoted arrays(" + _arrayFieldsInOut.size() + "):");
    List<String> unsorted = new ArrayList<>(_arrayFieldsInOut);
    Collections.sort(unsorted);
    for(String array : unsorted) {
      Message.debug("\t" + array);
    }
    Message.debug("  - Excluded from promotion variables("
        + _noPromotion.size() + "):");
    List<String> unsortedNoPromotions = new ArrayList<>(_noPromotion);
    Collections.sort(unsortedNoPromotions);
    for(String array : unsortedNoPromotions) {
      Message.debug("\t" + array);
    }
    Message.debug("  - Candidate arrays(" + candidateArrays.size() + "):");
    Collections.sort(candidateArrays);
    for(String array : candidateArrays) {
      Message.debug("\t" + array);
    }
    Message.debug("  - Candidate scalars(" + scalars.size() + "):");
    Collections.sort(scalars);
    for(String array : scalars) {
      Message.debug("\t" + array);
    }
    Message.debug("==========================================");
  }

  /**
   * Locate the parent function/subroutine in which the pragma is located.
   *
   * @param xcodeml Current translation unit.
   * @return True if the function/subroutine was located. False otherwise.
   */
  boolean detectParentFunction(XcodeProgram xcodeml) {
    // Check for the parent fct/subroutine definition
    _fctDef = _claw.getPragma().findParentFunction();
    if(_fctDef == null) {
      xcodeml.addError("Parent function/subroutine cannot be found. " +
              "SCA directive must be defined in a function/subroutine.",
          _claw.getPragma());
      return false;
    }
    _fctType = xcodeml.getTypeTable().getFunctionType(_fctDef);
    if(_fctType == null) {
      xcodeml.addError("Function/subroutine signature cannot be found!",
          _claw.getPragma());
      return false;
    }
    return true;
  }

  /**
   * Populate list of induction variables used in the function/subroutine body.
   */
  void detectInductionVariables() {
    _inductionVariables = _fctDef.detectInductionVariables();
  }

  /**
   * Analyse the defined dimension.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  boolean analyzeDimension(XcodeProgram xcodeml) {
    if(!_claw.hasClause(ClawClause.DIMENSION)
        && (_claw.isScaModelConfig()
        && Configuration.get().getModelConfig().getNbDimensions() == 0))
    {
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma());
      return false;
    }
    return true;
  }

  /**
   * Analyse the information defined in the data clause.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  boolean analyzeData(XcodeProgram xcodeml, ClawTranslator trans) {
    if(_claw.isScaModelConfig()) {
      return analyzeModelData(xcodeml, trans);
    } else {
      if(!_claw.hasClause(ClawClause.DATA_OVER)) {
        return analyzeDataForAutomaticPromotion(xcodeml);
      } else {
        return analyzeDataFromOverClause(xcodeml);
      }
    }
  }

  /**
   * Recover data defined by the model-data directive in this subroutine.
   *
   * @param xcodeml Current translation unit.
   * @param trans   Current translator holding information from model-data
   *                directive.
   * @return True if model-data information could be recovered. False otherwise.
   */
  private boolean analyzeModelData(XcodeProgram xcodeml, ClawTranslator trans) {
    Map<String, String> modelVariables;
    if(trans.hasElement(_fctDef) != null) {
      modelVariables = Utility.convertToMap(trans.hasElement(_fctDef));

      for(Map.Entry<String, String> dataInfo : modelVariables.entrySet()) {

        _arrayFieldsInOut.add(dataInfo.getKey());

        String layoutName = dataInfo.getValue();
        ModelConfig global = Configuration.get().getModelConfig();
        ModelConfig local = _claw.getLocalModelConfig();

        if(layoutName != null && global.hasLayout(layoutName)) {
          if(!local.hasLayout(layoutName)) {
            local.putLayout(layoutName, global.getLayout(layoutName));
          }

          local.putLayout(dataInfo.getKey(), local.getLayout(layoutName));
        }
      }
      return true;
    } else {
      xcodeml.addError("No model-data defined in function " + _fctDef.getName(),
          _claw.getPragma());
      return false;
    }
  }

  /**
   * If there is no data/over clause specified, an automatic deduction for
   * array promotion is performed.
   *
   * @param xcodeml Current translation unit
   * @return True if the analysis succeed. False otherwise.
   */
  private boolean analyzeDataForAutomaticPromotion(XcodeProgram xcodeml) {
    List<String> scalars = new ArrayList<>();
    List<String> candidateArrays = new ArrayList<>();
    List<Xnode> declarations =
        _fctDef.getDeclarationTable().values(Xcode.VAR_DECL);

    if(_claw.hasClause(ClawClause.NO_PROMOTE)) {
      _noPromotion.addAll(_claw.values(ClawClause.NO_PROMOTE));
    }

    for(Xnode decl : declarations) {
      if(xcodeml.getTypeTable().isBasicType(decl)) {
        String varName = decl.matchSeq(Xcode.NAME).value();
        FbasicType bType = xcodeml.getTypeTable().getBasicType(decl);

        if(!_noPromotion.contains(varName)) {
          if(bType.isArray()) {
            if(bType.hasIntent() || bType.isPointer()) {
              _arrayFieldsInOut.add(varName);
            } else {
              candidateArrays.add(varName);
            }
          } else {
            // Scalars mentioned in the scalar clause will be promoted.
            if(_claw.hasClause(ClawClause.SCALAR) &&
                _claw.values(ClawClause.SCALAR).contains(varName))
            {
              if(!bType.hasIntent()) {
                xcodeml.addWarning(String.format(
                    "Variable %s in scalar clause but not a dummy argument!",
                    varName), _claw.getPragma());
              }
              _arrayFieldsInOut.add(varName);
            } else if(!bType.isParameter() && !bType.hasIntent()) {
              scalars.add(varName); // Add scalar as candidate
            }
          }
        }
      }
    }
    _scalarFields.addAll(scalars);

    printDebugPromotionInfos(_fctDef.getName(), candidateArrays, scalars);

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
    for(String d : _claw.getDataOverClauseValues()) {
      if(!_fctDef.getSymbolTable().contains(d)) {
        xcodeml.addError(
            String.format("Data %s is not defined in the current block.", d),
            _claw.getPragma()
        );
        return false;
      }
      if(!_fctDef.getDeclarationTable().contains(d)) {
        xcodeml.addError(
            String.format("Data %s is not declared in the current block.", d),
            _claw.getPragma()
        );
        return false;
      }
    }
    _arrayFieldsInOut.addAll(_claw.getDataOverClauseValues());
    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Handle PURE function / subroutine
    if(Configuration.get().isForcePure() && _fctType.isPure()) {
      throw new IllegalTransformationException(
          "PURE specifier cannot be removed", _fctDef.lineNo());
    } else {
      removeAttributesWithWaring(xcodeml, _fctType, Xattr.IS_PURE);
    }

    // Insert the declarations of variables to iterate over the new dimensions.
    insertVariableToIterateOverDimension(xcodeml);

    // Promote all array fields with new dimensions.
    promoteFields(xcodeml);

    boolean adaptedNakedArrayRef = Context.get().getTarget() == Target.GPU;

    // Adapt array references.
    if(_claw.hasClause(ClawClause.DATA_OVER)) {
      for(String id : _claw.getDataOverClauseValues()) {
        Field.adaptArrayRef(_promotions.get(id), _fctDef.body(),
            adaptedNakedArrayRef, xcodeml);
      }
    } else {
      for(String id : _arrayFieldsInOut) {
        Field.adaptArrayRef(_promotions.get(id), _fctDef.body(),
            adaptedNakedArrayRef, xcodeml);
      }
    }

    removePragma();
  }

  /**
   * Remove the given attribute if exists and add a warning.
   *
   * @param xcodeml   Current translation unit.
   * @param fctType   Function type on which attribute is removed.
   * @param attribute Attribute to remove.
   */
  void removeAttributesWithWaring(XcodeProgram xcodeml, FfunctionType fctType,
                                  Xattr attribute)
  {
    if(fctType.hasAttribute(attribute)) {
      Message.debug(String.format(
          "%s attribute %s removed from function/subroutine %s",
          SCA_DEBUG_PREFIX, attribute.toStringForMsg(), _fctDef.getName()));
      fctType.removeAttribute(attribute);
    }
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
    /* If the subroutine/function is public and part of a module, update the
     * module signature to propagate the promotion information. */
    if(Function.isModuleProcedure(_fctDef, xcodeml)
        || !_fctType.getBooleanAttribute(Xattr.IS_PRIVATE))
    {
      FmoduleDefinition modDef = _fctDef.findParentModule();
      if(modDef != null) {
        if(forceAssumedShapedArrayPromotion) {
          _fctType.setBooleanAttribute(Xattr.IS_FORCE_ASSUMED, true);
        }
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
    if(_claw.hasClause(ClawClause.DATA_OVER)) {
      for(String fieldId : _claw.getDataOverClauseValues()) {
        PromotionInfo promotionInfo = new PromotionInfo(fieldId,
            _claw.getLayoutForData(fieldId));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(fieldId, promotionInfo);
      }
    } else {
      // Promote all arrays in a similar manner
      for(String fieldId : _arrayFieldsInOut) {
        PromotionInfo promotionInfo = new PromotionInfo(fieldId,
            _claw.getLayoutForData(fieldId));
        if(forceAssumedShapedArrayPromotion) {
          promotionInfo.forceAssumedShape();
        }
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
    FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.IN);
    xcodeml.getTypeTable().add(bt);

    // For each dimension defined in the directive
    for(DimensionDefinition dimension : _claw.getDefaultLayout()) {
      if(!forceAssumedShapedArrayPromotion) {
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

        // Create the parameter for the iteration lower bound
        if(dimension.getIterationLowerBound().isVar()) {
	  String itLowerBound = dimension.getIterationLowerBound().getValue();
	  if(!itLowerBound.equals(dimension.getLowerBound().getValue())) {
	    xcodeml.createIdAndDecl(itLowerBound, bt.getType(),
                XstorageClass.F_PARAM, _fctDef, DeclarationPosition.FIRST);

            // Add parameter to the local type table
            Xnode param = xcodeml.createAndAddParam(
                dimension.getIterationLowerBound().getValue(),
                bt.getType(), _fctType);
            param.setBooleanAttribute(Xattr.IS_INSERTED, true);
	  }
        }

        // Create parameter for the upper bound
        if(dimension.getIterationUpperBound().isVar()) {
	  String itUpperBound = dimension.getIterationUpperBound().getValue();
	  if(!itUpperBound.equals(dimension.getUpperBound().getValue())) {
            xcodeml.createIdAndDecl(itUpperBound, bt.getType(),
                XstorageClass.F_PARAM, _fctDef, DeclarationPosition.FIRST);

            // Add parameter to the local type table
            Xnode param = xcodeml.createAndAddParam(
                dimension.getIterationUpperBound().getValue(),
                bt.getType(), _fctType);
            param.setBooleanAttribute(Xattr.IS_INSERTED, true);
          }
        }
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
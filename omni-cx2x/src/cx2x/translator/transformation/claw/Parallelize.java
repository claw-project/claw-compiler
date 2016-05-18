/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawDimension;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

import java.util.HashMap;
import java.util.Map;

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
  private XfunctionDefinition _fctDef;

  /**
   * Constructs a new Parallelize transfomration triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
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

    // Check if any dimension has been defined.
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
      }
      _dimensions.put(d.getIdentifier(), d);
    }

    // Check data information
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

    // Check the dimension information
    if(!_claw.getOverClauseValues().contains(":")){
      xcodeml.addError("The column dimension has not been specified in the " +
          "over clause. Use : to specify it.",
          _claw.getPragma().getLineNo());
      return false;
    }

    for(String o : _claw.getOverClauseValues()){
      if(o != ":" && !_dimensions.containsKey(o)){
        xcodeml.addError(
            String.format("Dimension %s is not defined. Cannot be used in over " +
                "clause", o), _claw.getPragma().getLineNo()
        );
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
    if(_claw.getTarget() == Target.GPU){
      transformForGPU(xcodeml, transformer);
    } else {
      transformForCPU(xcodeml, transformer);
    }
  }

  /**
   * Apply GPU based transformation.
   * @param xcodeProgram Current XcodeML program unit.
   * @param transformer  Current transformer.
   * @throws Exception
   */
  private void transformForGPU(XcodeProgram xcodeProgram,
                               Transformer transformer)
      throws Exception
  {
    // Insert the declarations of variables to iterate over the new dimensions
    insertVariableToIterateOverDimension(xcodeProgram);


    

    // Common
    for(String data : _claw.getDataClauseValues()){
      Xid id = _fctDef.getSymbolTable().get(data);
      XvarDecl decl = _fctDef.getDeclarationTable().get(data);
      XbasicType bType = (XbasicType) xcodeProgram.getTypeTable().get(id.getType());
      if(bType == null){
        throw new IllegalTransformationException("Cannot find type for " + data,
            _claw.getPragma().getLineNo());
      }
      XbasicType newType = bType.cloneObject();
      newType.setType(xcodeProgram.getTypeTable().generateArrayTypeHash());
      XindexRange index = XindexRange.createEmptyAssumedShaped(xcodeProgram);
      newType.addDimension(index, 0);
      id.setType(newType.getType());
      decl.getName().setType(newType.getType());
      xcodeProgram.getTypeTable().add(newType);
    }


  }

  /**
   * Apply CPU based transformations.
   * @param xcodeProgram Current XcodeML program unit
   * @param transformer  Current transformer
   * @throws Exception
   */
  private void transformForCPU(XcodeProgram xcodeProgram,
                               Transformer transformer)
      throws Exception
  {

  }


  /**
   * Insert the declaration of the different variables needed to iterate over
   * the additional dimensions.
   * @param xcodeml Current XcodeML program unit in which element are created.
   * @throws IllegalTransformationException
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
    Xid id = Xid.create(type, XelementName.SCLASS_F_PARAM, name, xcodeml);
    _fctDef.getSymbolTable().add(id);
    XvarDecl decl = XvarDecl.create(type, name, xcodeml);
    _fctDef.getDeclarationTable().add(decl);
  }


  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // This is an independent transformation
  }
}

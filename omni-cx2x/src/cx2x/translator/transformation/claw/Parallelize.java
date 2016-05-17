/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XfunctionDefinition;

/**
 * The parallelize transformation transforms the code contained in a
 * subtourine/function by adding necessary dimensions and parallelism to the
 * defined data.
 *
 * @author clementval
 */
public class Parallelize extends Transformation {

  private final ClawLanguage _claw;

  /**
   * Constructs a new Parallelize transfomration triggered from a specific
   * pragma.
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawLanguage directive) {
    super(directive);
    _claw = directive; // Keep information about the claw directive here
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // Check for the parent fct/subroutine definition
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());
    if(fctDef == null){
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

    // Check data information
    for(String d : _claw.getDataClauseValues()){
      if(!fctDef.getSymbolTable().contains(d)){
        xcodeml.addError(
            String.format("Data %s is not defined in the current block.", d),
            _claw.getPragma().getLineNo()
        );
        return false;
      }
      if(!fctDef.getDeclarationTable().contains(d)){
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



    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other)
      throws Exception
  {

  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    return false; // This is an independent transformation
  }
}

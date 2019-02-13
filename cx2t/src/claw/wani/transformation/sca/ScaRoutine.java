/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.generator.DirectiveGenerator;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.wani.language.ClawPragma;

/**
 * Sca routine is a simple transformation targeting ELEMENTAL
 * function/subroutine used inside other ELEMENT function/subroutine that have
 * been parallelized.
 *
 * @author clementval
 */
public class ScaRoutine extends Sca {

  /**
   * Constructs a new Sca transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaRoutine(ClawPragma directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    if(!detectParentFunction(xcodeml)) {
      return false;
    }

    if(!_fctType.isElemental()) {
      xcodeml.addError("Parent function/subroutine must be ELEMENTAL.", _claw);
      return false;
    }

    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    if(Context.isTarget(Target.GPU)) {
      _fctType.removeAttribute(Xattr.IS_PURE);
      _fctType.removeAttribute(Xattr.IS_ELEMENTAL);
      DirectiveGenerator dirGen = Context.get().getGenerator();
      Xnode hook = null;
      for(String directive : dirGen.getRoutineDirective(true)) {
        Xnode pragma = xcodeml.createSinglePragma(directive);
        if(hook == null) {
          _fctDef.body().insert(pragma);
        } else {
          hook.insertAfter(pragma);
        }
        hook = pragma;
      }
    }
    removePragma();
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // independent transformation
  }

}

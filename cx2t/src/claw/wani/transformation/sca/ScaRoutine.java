/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.common.Directive;
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
public class ScaRoutine extends Sca
{

    /**
     * Constructs a new Sca transformation triggered from a specific pragma.
     *
     * @param directive The directive that triggered the define transformation.
     */
    public ScaRoutine(ClawPragma directive)
    {
        super(directive);
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        if (!detectParentFunction(xcodeml))
        {
            xcodeml.addError("Cannot find parent subroutine/function.", _claw.getPragma());
            return false;
        }
        return true;
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other)
    {
        if (Context.isTarget(Target.GPU))
        {

            DirectiveGenerator dirGen = Context.get().getGenerator();

            if (_fctType.isElemental())
            {
                _fctType.removeAttribute(Xattr.IS_PURE);
                _fctType.removeAttribute(Xattr.IS_ELEMENTAL);
            }

            if (Directive.hasDirectives(_fctDef))
            {
                xcodeml.addWarning(
                        String.format("%s %s", SCA_DEBUG_PREFIX,
                                "Function/subroutine has some directives! "
                                        + "Cannot insert new directives without breaking existing ones!"),
                        _claw.getPragma());
            } else
            {
                Directive.addPragmasBefore(xcodeml, dirGen.getRoutineDirective(true), _fctDef.body().child(0));
            }
        }
        removePragma();
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        return false; // independent transformation
    }

}

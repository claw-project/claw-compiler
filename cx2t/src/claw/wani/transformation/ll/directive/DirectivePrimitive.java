/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.directive;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.wani.ClawConstant;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.transformation.internal.OpenAccContinuation;

/**
 * Directive primitive transformation allows to enable specific directive
 * primitive specified by the --directive option. The "directive primitive"
 * directive is the prefix of the directive primitive language. acc for OpenACC
 * and omp for OpenMP for example.
 *
 * @author clementval
 */
public class DirectivePrimitive extends ClawTransformation
{

    /**
     * Constructs a new DirectivePrimitive triggered from a specific pragma.
     *
     * @param directive The directive that triggered the directive primitive
     *                  transformation.
     */
    public DirectivePrimitive(ClawPragma directive)
    {
        super(directive);
    }

    /**
     * Analysis of the transformation.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @return True always.
     */
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        return true;
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

    /**
     * Apply the directive primitive transformation.
     *
     * @param xcodeml        The XcodeML on which the transformations are applied.
     * @param translator     The translator used to applied the transformations.
     * @param transformation Not used in this transformation
     * @throws IllegalTransformationException if the transformation cannot be
     *                                        applied.
     */
    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation transformation)
            throws IllegalTransformationException
    {
        String prefix = xcodeml.context().getGenerator().getDirectiveLanguage().getPrefix();
        if (prefix == null)
        {
            return;
        }

        if (getDirective().getPragma().value().toLowerCase().contains(prefix))
        {
            String regex = ClawConstant.CLAW + " *" + prefix;
            getDirective().getPragma().setValue(getDirective().getPragma().value().replaceAll(regex, prefix));

            getDirective().getPragma().setValue(getDirective().getPragma().value().replaceAll(ClawConstant.CLAW, ""));
        }

        translator.addTransformation(xcodeml, new OpenAccContinuation((ClawPragma) getDirective()));
    }
}

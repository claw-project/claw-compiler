/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.internal;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

/**
 * <pre>
 * OpenACC line continuation transformation. The XcodeML/F pragma statement
 * representation is an aggregated version of the pragma with all its
 * continuation lines.
 * As those directives are not handled by the CLAW XcodeML to XcodeML
 * translator, they must be output in a correct way. This transformation divides
 * the XcodeML representation back to a multi-line pragma definition.
 *
 * Example:
 * The followings OpenACC directives in Fortran code:
 *
 *   !$acc data &amp;
 *   !$acc present (a,b,c,d,e,f,g)
 *
 * are represented in XcodeML with
 *
 * &lt;FpragmaStatement&gt;acc data present (a,b,c,d,e,f,g)
 * &lt;/FpragmaStatement&gt;
 *
 * Based on the defined max columns, the pragma statement will be splitted.
 * </pre>
 *
 * @author clementval
 */
public class OpenAccContinuation extends ClawTransformation
{

    /**
     * Constructs a new OpenACC continuation triggered from a specific pragma.
     *
     * @param directive The directive that triggered the OpenACC continuation
     *                  transformation.
     */
    public OpenAccContinuation(ClawPragma directive)
    {
        super(directive);
    }

    /**
     * Check if the directive starts with the OpenACC prefix.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @return True the directive starts with the OpenACC prefix.
     */
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        return getDirective().getPragma().value().startsWith(CompilerDirective.OPENACC.getPrefix());
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        // independent transformation
        return false;
    }

    /**
     * Apply the OpenACC continuation transformation.
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
        final Context context = xcodeml.context();
        if (Pragma.fromClawPrimitive(context, getDirective().getPragma()))
        {
            Pragma.splitByCont(getDirective().getPragma(), CompilerDirective.OPENACC.getPrefix(), xcodeml);
        } else if (context.getMaxColumns() > 0 && !getDirective().getPragma().isDeleted())
        {
            Pragma.splitByLength(getDirective().getPragma(), xcodeml, CompilerDirective.OPENACC.getPrefix());
        }
    }

    @Override
    public boolean abortOnFailedAnalysis()
    {
        return false;
    }
}

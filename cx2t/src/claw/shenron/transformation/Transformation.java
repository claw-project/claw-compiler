/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.transformation;

import claw.shenron.translator.AnalyzedPragma;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;

/**
 * A Transformation is an object capable of analyzing a possible code
 * transformation to be applied and the steps to apply it to the intermediate
 * representation. Normally, only derived classes of Transformation should be
 * applied as the base class does not implement the core methods.
 *
 * @author clementval
 */

public abstract class Transformation
{

    private final AnalyzedPragma _directive;
    private boolean _transformed = false;
    private int _startLine = 0;

    /**
     * Transformation ctor for translation_unit triggered transformation.
     */
    protected Transformation()
    {
        _directive = null;
    }

    /**
     * Transformation ctor for directive triggered transformation.
     *
     * @param directive The directive that triggered the transformation.
     */
    protected Transformation(AnalyzedPragma directive)
    {
        _directive = directive;

        if (_directive != null && _directive.getPragma() != null)
        {
            _startLine = _directive.getPragma().lineNo();
        }
    }

    /**
     * Analyze the possibility to apply the transformation. Gather information to be
     * able to apply the transformation in when calling #transform.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @return True if analysis succeeded. False otherwise.
     */
    public abstract boolean analyze(XcodeProgram xcodeml, Translator translator);

    /**
     * Check whether the current transformation can be transformed together with the
     * given transformation. Useful only for dependent transformation.
     *
     * @param xcodeml The XcodeML on which the transformations are applied.
     * @param other   The other transformation part of the dependent transformation.
     * @return True if the two transformation can be transform together. False
     *         otherwise.
     * @see DependentTransformationGroup
     */
    public abstract boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other);

    /**
     * Tells whether the transformation should abort the translation if the analysis
     * fails. Default behavior is to abort.
     *
     * @return True if it aborts. False otherwise.
     */
    public boolean abortOnFailedAnalysis()
    {
        return true;
    }

    /**
     * Apply the actual transformation.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @param other      Only for dependent transformation. The other transformation
     *                   part of the transformation.
     * @throws IllegalTransformationException if the transformation cannot be
     *                                        applied.
     */
    public abstract void transform(XcodeProgram xcodeml, Translator translator, Transformation other) throws Exception;

    /**
     * Get the directive that triggered the transformation.
     *
     * @return The analyzed directive as a ClawPragma object.
     */
    public AnalyzedPragma getDirective()
    {
        return _directive;
    }

    /**
     * Get the line number where the pragma was found.
     *
     * @return Line number of the pragma.
     */
    protected int getStartLine()
    {
        return _startLine;
    }

    /**
     * Set the start line of the transformation. Normally, this is the line where
     * the pragma was found.
     *
     * @param lineno An positive integer value representing the line number.
     */
    protected void setStartLine(int lineno)
    {
        _startLine = lineno;
    }

    /**
     * Get the information whether the transformation has been applied or not.
     *
     * @return True is the transformation has been applied. False otherwise.
     */
    protected boolean isTransformed()
    {
        return _transformed;
    }

    /**
     * Set the transformation as transformed.
     */
    protected void transformed()
    {
        _transformed = true;
    }
}

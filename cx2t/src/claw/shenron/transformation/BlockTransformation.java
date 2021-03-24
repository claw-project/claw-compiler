/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.transformation;

import claw.shenron.translator.AnalyzedPragma;

/**
 * A BlockTransformation is an extension of the standard Transformation that is
 * defined by a start directive and an additional end directive. The
 * transformation is then applied on the structured block between the two
 * directive.
 *
 * @author clementval
 */
public abstract class BlockTransformation extends Transformation
{

    private final AnalyzedPragma _endDirective;

    /**
     * BlockTransformation ctor.
     *
     * @param startDirective The directive that triggered the transformation.
     * @param endDirective   The end directive that close the structured block.
     */
    protected BlockTransformation(AnalyzedPragma startDirective, AnalyzedPragma endDirective)
    {
        super(startDirective);
        _endDirective = endDirective;
    }

    /**
     * Get the end directive that triggered the transformation.
     *
     * @return The analyzed directive as a language object.
     */
    public AnalyzedPragma getEndDirective()
    {
        return _endDirective;
    }
}

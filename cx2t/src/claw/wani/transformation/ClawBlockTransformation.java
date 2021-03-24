/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation;

import claw.shenron.transformation.BlockTransformation;
import claw.wani.language.ClawPragma;

/**
 * A BlockTransformation is an extension of the standard Transformation that is
 * defined by a start directive and an additional end directive. The
 * transformation is then applied on the structured block between the two
 * directive.
 *
 * @author clementval
 */
public abstract class ClawBlockTransformation extends BlockTransformation
{

    protected final ClawPragma _clawStart;
    protected final ClawPragma _clawEnd;

    protected ClawBlockTransformation(ClawPragma startDirective, ClawPragma endDirective)
    {
        super(startDirective, endDirective);
        _clawStart = startDirective;
        _clawEnd = endDirective;
    }

    /**
     * Delete the associated pragma statement(s).
     */
    protected void removePragma()
    {
        if (_clawStart != null && _clawStart.getPragma() != null)
        {
            _clawStart.getPragma().delete();
        }
        if (_clawEnd != null && _clawEnd.getPragma() != null)
        {
            _clawEnd.getPragma().delete();
        }
    }
}

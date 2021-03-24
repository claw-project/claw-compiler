/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.translator;

import claw.tatsu.xcodeml.xnode.common.Xnode;

/**
 * Base class for any analyzed pragma. This object is then passed to a
 * transformation. Base implementation only stores the raw pragma element
 * object.
 *
 * @author clementval
 */
public class AnalyzedPragma
{

    protected Xnode _pragma;
    private boolean _isEndPragma;

    /**
     * Default ctor.
     */
    public AnalyzedPragma()
    {
        _isEndPragma = false;
    }

    /**
     * Constructs an AnalyzedPragma object with a raw pragma element object
     * attached.
     *
     * @param rawPragma Pragma object to be attached.
     */
    public AnalyzedPragma(Xnode rawPragma)
    {
        _pragma = rawPragma;
        _isEndPragma = false;
    }

    /**
     * Get the attached pragma object.
     *
     * @return Attached pragma object.
     */
    public Xnode getPragma()
    {
        return _pragma;
    }

    /**
     * Attach a pragma object.
     *
     * @param rawPragma Pragma object to be attached.
     */
    public void setPragma(Xnode rawPragma)
    {
        _pragma = rawPragma;
    }

    /**
     * Check whether the pragma is an end block pragma.
     *
     * @return True if the pragma ends a block. False otherwise.
     */
    public boolean isEndPragma()
    {
        return _isEndPragma;
    }

    /**
     * Set value to the endPragma flag.
     */
    public void setEndPragma()
    {
        _isEndPragma = true;
    }
}

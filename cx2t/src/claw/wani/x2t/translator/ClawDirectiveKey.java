/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.translator;

import claw.wani.language.ClawDirective;

/**
 * Class used as a composite key for the block transformation handling.
 *
 * @author clementval
 */
class ClawDirectiveKey
{

    private final ClawDirective _directive;
    private final int _depth;

    /**
     * Constructs a new composite key with its two elements.
     *
     * @param directive ClawDirective to be associated with this key.
     * @param depth     Depth of the directive in the AST.
     */
    public ClawDirectiveKey(ClawDirective directive, int depth)
    {
        _directive = directive;
        _depth = depth;
    }

    /**
     * Get the directive associated with this key.
     *
     * @return ClawDirective value.
     */
    private ClawDirective getDirective()
    {
        return _directive;
    }

    /**
     * Get the depth of this directive in the AST.
     *
     * @return depth
     */
    private int getDepth()
    {
        return _depth;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj instanceof ClawDirectiveKey)
        {
            ClawDirectiveKey o = (ClawDirectiveKey) obj;
            return _directive.equals(o.getDirective()) && _depth == o.getDepth();
        }
        return false;
    }

    @Override
    public int hashCode()
    {
        return _directive.hashCode() + _depth;
    }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xnode;

/**
 * Xblock represent a block of statements in the XcodeML representation with a
 * start and an end.
 *
 * @author clementval
 */
public class Xblock
{

    private final Xnode _startNode;
    private Xnode _endNode;

    public Xblock(Xnode start)
    {
        _startNode = start;
        _endNode = null;
    }

    public Xblock(Xnode start, Xnode end)
    {
        _startNode = start;
        _endNode = end;
    }

    public Xnode getStart()
    {
        return _startNode;
    }

    public Xnode getEnd()
    {
        return _endNode != null ? _endNode : _startNode;
    }

    public void setEnd(Xnode end)
    {
        if (end != null)
        {
            _endNode = end;
        }
    }

}

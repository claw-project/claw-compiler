/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xnode;

/**
 * Class holding information about a group of nested loop in a do statement
 * hoisting transformation.
 *
 * @author clementval
 */
public class HoistedNestedDoStatement extends NestedDoStatement
{

    private boolean _needExtraction = false;
    private boolean _needIfication = false;

    public HoistedNestedDoStatement(Xnode outerDoStatement, int nb)
    {
        super(outerDoStatement, nb);
    }

    /**
     * Set the IF extraction flag to true.
     */
    public void setExtraction()
    {
        _needExtraction = true;
    }

    /**
     * Set the IF statement creation flag to true.
     */
    public void setIfStatement()
    {
        _needIfication = true;
    }

    /**
     * Check whether this group of do statements needs the extraction from na IF
     * statement.
     *
     * @return True if the group need the extraction. False otherwise.
     */
    public boolean needExtraction()
    {
        return _needExtraction;
    }

    /**
     * Check whether this group of do statements needs the creation of na IF
     * statement.
     *
     * @return True if the group need the creation. False otherwise.
     */
    public boolean needIfStatement()
    {
        return _needIfication;
    }

    /**
     * Clone current nested group with all its elements.
     *
     * @return Newly created nested do statements group.
     */
    @Override
    public HoistedNestedDoStatement cloneNestedGroup()
    {
        Xnode newDoStmt = get(0).cloneNode();
        return new HoistedNestedDoStatement(newDoStmt, size());
    }
}

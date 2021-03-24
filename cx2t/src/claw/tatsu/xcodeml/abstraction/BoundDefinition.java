/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;

/**
 * Class holding information about bound (upper/lower).
 *
 * @author clementval
 */
public class BoundDefinition
{

    private String _strBoundValue;
    private int _intBoundValue;
    private final BoundType _boundType;

    /**
     * Constructs a BoundDefinition from String value. Detects if bound is an
     * integer constant or a var.
     *
     * @param boundValue String representation of the bound value.
     */
    BoundDefinition(String boundValue, BoundType type)
    {
        _boundType = type;
        try
        {
            _intBoundValue = Integer.parseInt(boundValue);
            _strBoundValue = "";
        } catch (NumberFormatException ex)
        {
            _intBoundValue = -1;
            _strBoundValue = boundValue;
        }
    }

    /**
     * Check whether the bound is a var.
     *
     * @return True if the bound is a var.
     */
    public boolean isVar()
    {
        return _strBoundValue != null && !_strBoundValue.isEmpty();
    }

    /**
     * Get integer value of the bound object.
     *
     * @return Integer value. -1 if not set.
     */
    public int getIntValue()
    {
        return _intBoundValue;
    }

    /**
     * Get string value of the bound object.
     *
     * @return String value. null if not set.
     */
    public String getValue()
    {
        return _strBoundValue;
    }

    /**
     * Generate the corresponding node to represent the bound.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @return Newly created node (lowerBound or upperBound).
     */
    public Xnode generate(XcodeML xcodeml)
    {
        Xcode opcode = _boundType == BoundType.LOWER ? Xcode.LOWER_BOUND : Xcode.UPPER_BOUND;
        Xnode bound = xcodeml.createNode(opcode);
        bound.append(generateValueNode(xcodeml));
        return bound;
    }

    /**
     * Generate the corresponding node to represent the bound value.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @return Newly created value node (Var or FintConstant).
     */
    Xnode generateValueNode(XcodeML xcodeml)
    {
        if (isVar())
        {
            FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.IN);
            xcodeml.getTypeTable().add(bt);
            return xcodeml.createVar(bt.getType(), _strBoundValue, Xscope.LOCAL);
        } else
        {
            return xcodeml.createIntConstant(_intBoundValue);
        }
    }

    // Enum representing the type of bound
    public enum BoundType {
        LOWER, UPPER, STEP
    }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeML;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;

import java.util.List;

/**
 * @author clementval
 */
public class Type
{

    // Avoid instantiation of
    private Type()
    {
    }

    /**
     * Duplicates the type to update and add extra dimensions to match the base
     * type.
     *
     * @param base       Base type.
     * @param toUpdate   Type to update.
     * @param xcodemlSrc Source XcodeML unit. Contains base dimension.
     * @param xcodemlDst Destination XcodeML unit. Duplicate will be created here.
     * @param dimensions List of dimensions definitions to be used.
     * @return The new type hash generated.
     * @throws IllegalTransformationException If action is not supported.
     */
    public static FbasicType duplicateWithDimension(FbasicType base, FbasicType toUpdate, XcodeML xcodemlSrc,
            XcodeML xcodemlDst, List<DimensionDefinition> dimensions) throws IllegalTransformationException
    {
        FbasicType newType = toUpdate.cloneNode();
        String type = xcodemlDst.getTypeTable().generateHash(FortranType.ARRAY);
        newType.setType(type);

        if (base.isAllAssumedShape() && toUpdate.isAllAssumedShape())
        {
            int additionalDimensions = base.getDimensions() - toUpdate.getDimensions();
            for (int i = 0; i < additionalDimensions; ++i)
            {
                Xnode index = xcodemlDst.createEmptyAssumedShaped();
                newType.addDimension(index, 0);
            }
        } else if (base.isAllAssumedShape() && !toUpdate.isAllAssumedShape())
        {
            for (DimensionDefinition dim : dimensions)
            {
                switch (dim.getInsertionPosition())
                {
                case BEFORE:
                    // TODO control and validate the before/after
                    newType.addDimension(dim.generateIndexRange(xcodemlDst, false, false));
                    break;
                case AFTER:
                    newType.addDimension(dim.generateIndexRange(xcodemlDst, false, false), 0);
                    break;
                case IN_MIDDLE:
                    throw new IllegalTransformationException(
                            "Not supported yet. " + "Insertion in middle for duplicated array type.", 0);
                }
            }
        } else
        {
            newType.resetDimension();
            for (int i = 0; i < base.getDimensions(); ++i)
            {
                Xnode newDim = xcodemlDst.createNode(Xcode.INDEX_RANGE);
                newType.append(newDim);

                Xnode baseDim = base.getDimensions(i);
                Xnode lowerBound = baseDim.matchSeq(Xcode.LOWER_BOUND);
                Xnode upperBound = baseDim.matchSeq(Xcode.UPPER_BOUND);

                if (lowerBound != null)
                {
                    Xnode newLowerBound = Type.duplicateBound(lowerBound, xcodemlSrc, xcodemlDst);
                    newDim.append(newLowerBound);
                }
                if (upperBound != null)
                {
                    Xnode newUpperBound = Type.duplicateBound(upperBound, xcodemlSrc, xcodemlDst);
                    newDim.append(newUpperBound);
                }
                newType.addDimension(newDim);
            }
        }

        xcodemlDst.getTypeTable().add(newType);
        return newType;
    }

    /**
     * Duplicate a lower or an upper bound between two different XcodeML units.
     *
     * @param baseBound  Base bound to be duplicated.
     * @param xcodemlSrc Source XcodeML unit. Contains base bound.
     * @param xcodemlDst Destination XcodeML unit. Duplicate will be created here.
     * @return The newly duplicated bound element.
     * @throws IllegalTransformationException If bound cannot be duplicated.
     */
    private static Xnode duplicateBound(Xnode baseBound, XcodeML xcodemlSrc, XcodeML xcodemlDst)
            throws IllegalTransformationException
    {
        if (!Xnode.isOfCode(baseBound, Xcode.LOWER_BOUND) && !Xnode.isOfCode(baseBound, Xcode.UPPER_BOUND))
        {
            throw new IllegalTransformationException("Cannot duplicate bound");
        }

        if (xcodemlSrc == xcodemlDst)
        {
            return baseBound.cloneNode();
        }

        Xnode boundChild = baseBound.child(0);
        if (boundChild == null)
        {
            throw new IllegalTransformationException("Cannot duplicate bound as it " + "has no children element");
        }

        Xnode bound = xcodemlDst.createNode(baseBound.opcode());
        bound.append(xcodemlDst.importElement(boundChild, xcodemlSrc));
        return bound;
    }
}

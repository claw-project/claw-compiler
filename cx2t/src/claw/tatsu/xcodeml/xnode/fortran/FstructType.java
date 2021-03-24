/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xnode;

/**
 * The FstructType class represents the FstructType (3.5) element in XcodeML/F.
 *
 * Elements: (params?) - Optional: typeParams, symbols, typeBoundProcedures
 *
 * Attributes: - Required: type (text) - Optional: is_public (bool), is_private
 * (bool), is_sequence (bool), is_internal_private (bool), is_abstract (bool),
 * extends (text), bind (text)
 *
 * @author clementval
 */
public class FstructType extends Xnode
{

    /**
     * Basic ctor from Xnode.
     *
     * @param node Xnode object.
     */
    public FstructType(Xnode node)
    {
        super(node == null ? null : node.element());
    }

    /**
     * Check is the struct type is an extension of another type.
     *
     * @return True if the struct type is an extension. False otherwise.
     */
    public boolean isExtend()
    {
        return hasAttribute(Xattr.EXTENDS);
    }

    /**
     * Get the value of the extend attribute.
     *
     * @return String value of the extend attribute.
     */
    public String getExtend()
    {
        return getAttribute(Xattr.EXTENDS);
    }

    /**
     * Get the value of the bind attribute.
     *
     * @return String value of the bind attribute.
     */
    public String getBind()
    {
        return getAttribute(Xattr.BIND);
    }

    /**
     * Check whether the type is public.
     *
     * @return True if the attribute is_public is set to true. False otherwise.
     */
    public boolean isPublic()
    {
        return getBooleanAttribute(Xattr.IS_PUBLIC);
    }

    /**
     * Check whether the type is private.
     *
     * @return True if the attribute is_private is set to true. False otherwise.
     */
    public boolean isPrivate()
    {
        return getBooleanAttribute(Xattr.IS_PRIVATE);
    }

    /**
     * Check whether the type is sequence.
     *
     * @return True if the attribute is_sequence is set to true. False otherwise.
     */
    public boolean isSequence()
    {
        return getBooleanAttribute(Xattr.IS_SEQUENCE);
    }

    /**
     * Check whether the type is internal private.
     *
     * @return True if the attribute is_internal_private is set to true. False
     *         otherwise.
     */
    public boolean isInternalPrivate()
    {
        return getBooleanAttribute(Xattr.IS_INTERNAL_PRIVATE);
    }

    /**
     * Check whether the type is abstract.
     *
     * @return True if the attribute is_abstract is set to true. False otherwise.
     */
    public boolean isAbstract()
    {
        return getBooleanAttribute(Xattr.IS_ABSTRACT);
    }
}

/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;

/**
 * Enumeration representing the different storage class type from XcodeML/F IR
 * (8.2)
 *
 * @author clementval
 */
public enum XstorageClass {
    AUTO(Xname.SCLASS_AUTO), EXTERN(Xname.SCLASS_EXTERN), EXTERN_DEF(Xname.SCLASS_EXTERN_DEF),
    F_LOCAL(Xname.SCLASS_F_LOCAL), F_FUNC(Xname.SCLASS_F_FUNC), F_PARAM(Xname.SCLASS_F_PARAM),
    LABEL(Xname.SCLASS_LABEL), PARAM(Xname.SCOPE_PARAM), NONE("");

    // Member holding XcodeML/F IR value.
    private final String _irValue;

    /**
     * Default constructor of enum member with associated IR value.
     *
     * @param irValue Value from XcodeML/F IR.
     */
    XstorageClass(String irValue)
    {
        _irValue = irValue;
    }

    /**
     * Get type from XcodeML/F IR value.
     *
     * @param value Type value from XcodeML/F IR.
     * @return Corresponding enum value. NONE if value corresponds to nothing.
     */
    public static XstorageClass fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value)
        {
        case Xname.SCLASS_AUTO:
            return AUTO;
        case Xname.SCLASS_EXTERN:
            return EXTERN;
        case Xname.SCLASS_EXTERN_DEF:
            return EXTERN_DEF;
        case Xname.SCLASS_F_LOCAL:
            return F_LOCAL;
        case Xname.SCLASS_F_FUNC:
            return F_FUNC;
        case Xname.SCLASS_F_PARAM:
            return F_PARAM;
        case Xname.SCLASS_LABEL:
            return LABEL;
        case Xname.SCOPE_PARAM:
            return PARAM;
        default:
            return NONE;
        }
    }

    @Override
    public String toString()
    {
        return _irValue;
    }
}

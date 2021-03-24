/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;

import java.util.HashMap;
import java.util.Map;

/**
 * XcodeML element attributes code.
 *
 * @author clementval
 */
public enum Xattr {
    ATTR(Xname.ATTR_ATTR), BIND(Xname.ATTR_BIND), BIND_NAME(Xname.ATTR_BIND_NAME), CODE(Xname.ATTR_CODE),
    COMPILER_INFO(Xname.ATTR_COMPILER_INFO), CONSTRUCT_NAME(Xname.ATTR_CONSTRUCT_NAME), EXTENDS(Xname.ATTR_EXTENDS),
    DATA_REF(Xname.ATTR_DATA_REF), FILE(Xname.ATTR_FILE), FORMAT(Xname.ATTR_FORMAT), INTENT(Xname.ATTR_INTENT),
    IS_ABSTRACT(Xname.ATTR_IS_ABSTRACT), IS_ALLOCATABLE(Xname.ATTR_IS_ALLOCATABLE),
    IS_ASSIGNMENT(Xname.ATTR_IS_ASSIGNMENT), IS_ASSUMED_SIZE(Xname.ATTR_IS_ASSUMED_SIZE),
    IS_ASSUMED_SHAPE(Xname.ATTR_IS_ASSUMED_SHAPE), IS_ASYNCHRONOUS(Xname.ATTR_IS_ASYNCHRONOUS),
    IS_CLASS(Xname.ATTR_IS_CLASS), IS_COMPLEX_PART(Xname.ATTR_IS_COMPLEX_PART), IS_CONTIGUOUS(Xname.ATTR_IS_CONTIGUOUS),
    IS_DEFERRED(Xname.ATTR_IS_DEFERRED), IS_DEFINED_IO(Xname.ATTR_IS_DEFINED_IO), IS_ELEMENTAL(Xname.ATTR_IS_ELEMENTAL),
    IS_EXTERNAL(Xname.ATTR_IS_EXTERNAL), IS_INTERNAL(Xname.ATTR_IS_INTERNAL),
    IS_INTERNAL_PRIVATE(Xname.ATTR_IS_INTERNAL_PRIVATE), IS_INTRINSIC(Xname.ATTR_IS_INTRINSIC),
    IS_NON_OVERRIDABLE(Xname.ATTR_IS_NON_OVERRIDABLE), IS_OPERATOR(Xname.ATTR_IS_OPERATOR),
    IS_OPTIONAL(Xname.ATTR_IS_OPTIONAL), IS_PARAMETER(Xname.ATTR_IS_PARAMETER), IS_POINTER(Xname.ATTR_IS_POINTER),
    IS_PRIVATE(Xname.ATTR_IS_PRIVATE), IS_PROCEDURE(Xname.ATTR_IS_PROCEDURE), IS_PROGRAM(Xname.ATTR_IS_PROGRAM),
    IS_PROTECTED(Xname.ATTR_IS_PROTECTED), IS_PUBLIC(Xname.ATTR_IS_PUBLIC), IS_PURE(Xname.ATTR_IS_PURE),
    IS_RECURSIVE(Xname.ATTR_IS_RECURSIVE), IS_SAVE(Xname.ATTR_IS_SAVE), IS_SEQUENCE(Xname.ATTR_IS_SEQUENCE),
    IS_SUB(Xname.ATTR_IS_SUB), IS_TARGET(Xname.ATTR_IS_TARGET), IS_VALUE(Xname.ATTR_IS_VALUE),
    IS_VOLATILE(Xname.ATTR_IS_VOLATILE), KIND(Xname.ATTR_KIND), LABEL_NAME(Xname.ATTR_LABEL_NAME),
    LANGUAGE(Xname.ATTR_LANGUAGE), LINENO(Xname.ATTR_LINENO), LOCAL_NAME(Xname.ATTR_LOCAL_NAME),
    MEMBER(Xname.ATTR_MEMBER), MESSAGE(Xname.ATTR_MESSAGE), NAME(Xname.ATTR_NAME), PARENT_NAME(Xname.ATTR_PARENT_NAME),
    PASS(Xname.ATTR_PASS), PASS_ARG_NAME(Xname.ATTR_PASS_ARG_NAME), REPEAT_COUNT(Xname.ATTR_REPEAT_COUNT),
    REF(Xname.ATTR_REF), RESULT_NAME(Xname.ATTR_RESULT_NAME), RETURN_TYPE(Xname.ATTR_RETURN_TYPE),
    SCLASS(Xname.ATTR_SCLASS), SCOPE(Xname.ATTR_SCOPE), SOURCE(Xname.ATTR_SOURCE), STAT_NAME(Xname.ATTR_STAT_NAME),
    TIME(Xname.ATTR_TIME), TYPE(Xname.ATTR_TYPE), USE_NAME(Xname.ATTR_USE_NAME), VALUE(Xname.ATTR_VALUE),
    VERSION(Xname.ATTR_VERSION),

    // FortranModule extension to share promotion information
    IS_INSERTED(Xname.ATTR_IS_INSERTED), IS_FORCE_ASSUMED(Xname.ATTR_IS_FORCE_ASSUMED),
    PROMOTION_INFO(Xname.ATTR_PROMOTION_INFO), WAS_ELEMENTAL(Xname.ATTR_WAS_ELEMENTAL);

    private static final Map<String, Xattr> _stringToEnum = new HashMap<>();

    static
    {
        for (Xattr attr : values())
        {
            _stringToEnum.put(attr.toString().toLowerCase(), attr);
        }
    }

    private final String _irValue;

    Xattr(String s)
    {
        _irValue = s;
    }

    public static Xattr fromString(String value)
    {
        return (value == null || !_stringToEnum.containsKey(value.toLowerCase())) ? null
                : _stringToEnum.get(value.toLowerCase());
    }

    @Override
    public String toString()
    {
        return _irValue;
    }

    public String toStringForMsg()
    {
        return _irValue.replaceAll("is_", "").toUpperCase();
    }
}
